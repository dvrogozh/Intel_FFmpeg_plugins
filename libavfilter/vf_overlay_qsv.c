/*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @vf_overlay_qsv
 * A hardware accelerated overlay filter based on Intel Quick Sync Video VPP
 */
#include "config.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "libavutil/eval.h"
#include "libavutil/avstring.h"
#include "libavutil/hwcontext.h"
#include "internal.h"
#include "framesync.h"
#include "qsvvpp.h"

#define OFFSET(x) offsetof(QSVOverlayContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM
#define MAKE_FUNC(name) \
static double get_ ## name(void *opaque, double param1) \
{ \
    AVFilterContext *ctx = opaque; \
    QSVOverlayContext *s = ctx->priv; \
    int              idx = (int) param1; \
    return (double)s->layouts[idx].name; \
}

enum LayoutPreset {
    GRID,
    MANUAL,
    OVERLAY,
};

enum {
    VAR_W,
    VAR_H,
    VAR_N,
    VAR_n,
    VAR_NUM,
};

enum {
    ATTR_X,
    ATTR_Y,
    ATTR_W,
    ATTR_H,
    ATTR_A,
    ATTR_NUM,
};

typedef struct QSVLayout {
    int iw, ih;
    int x, y, w, h, alpha;
} QSVLayout;

typedef struct QSVOverlayContext {
    /* Don't edit. This is must.  */
    const AVClass *class;

    /* Runtime object */
    FFFrameSync fsync;
    FFQSVVPPContext *qsv;
    QSVLayout *layouts;

    /* AVExpressions */
    double var_values[VAR_NUM];
    AVExpr *(*iexpr)[ATTR_NUM];
    AVExpr *ow_expr, *oh_expr;

    /*
     * User-defined values.
     */
    int nb_inputs;
    enum LayoutPreset layout;
    const char *out_w, *out_h;
    const char *descs[ATTR_NUM];
    enum FFFrameSyncExtMode eof_action;
} QSVOverlayContext;

MAKE_FUNC(iw)
MAKE_FUNC(ih)

MAKE_FUNC(x)
MAKE_FUNC(y)
MAKE_FUNC(w)
MAKE_FUNC(h)
MAKE_FUNC(alpha)

static const char * const var_funcs[] = {
    "iw", "ih",
    "x", "y", "w", "h", "alpha",
    NULL,
};

static double (* const func_list[]) (void *, double) = {
    get_iw, get_ih,
    get_x, get_y, get_w, get_h, get_alpha,
    NULL,
};

static const char *var_names[] = {
    "W", "H", "N", "n",
    NULL,
};

static const AVOption overlay_qsv_options[] = {
    { "nb_inputs", "number of inputs", OFFSET(nb_inputs), AV_OPT_TYPE_INT, { .i64 = 2 }, 1, INT_MAX, FLAGS },

    { "layout", "preset layout", OFFSET(layout),  AV_OPT_TYPE_INT, { .i64 = OVERLAY }, GRID, OVERLAY, FLAGS, "layout" },
        { "grid",    NULL, 0, AV_OPT_TYPE_CONST, { .i64 = GRID },    .flags = FLAGS, "layout" },
        { "manual",  NULL, 0, AV_OPT_TYPE_CONST, { .i64 = MANUAL },  .flags = FLAGS, "layout" },
        { "overlay", NULL, 0, AV_OPT_TYPE_CONST, { .i64 = OVERLAY }, .flags = FLAGS, "layout" },

    { "width",  "output width",  OFFSET(out_w), AV_OPT_TYPE_STRING, { .str = "-2" }, .flags = FLAGS },
    { "w",      "output width",  OFFSET(out_w), AV_OPT_TYPE_STRING, { .str = "-2" }, .flags = FLAGS },
    { "height", "output height", OFFSET(out_h), AV_OPT_TYPE_STRING, { .str = "-2" }, .flags = FLAGS },
    { "h",      "output height", OFFSET(out_h), AV_OPT_TYPE_STRING, { .str = "-2" }, .flags = FLAGS },

    { "x_expr", "each win's x position", OFFSET(descs[ATTR_X]), AV_OPT_TYPE_STRING, { .str = "0" },   .flags = FLAGS },
    { "y_expr", "each win's y position", OFFSET(descs[ATTR_Y]), AV_OPT_TYPE_STRING, { .str = "0" },   .flags = FLAGS },
    { "w_expr", "each win's width",      OFFSET(descs[ATTR_W]), AV_OPT_TYPE_STRING, { .str = "-2" },  .flags = FLAGS },
    { "h_expr", "each win's height",     OFFSET(descs[ATTR_H]), AV_OPT_TYPE_STRING, { .str = "-2" },  .flags = FLAGS },
    { "a_expr", "each win's alpha",      OFFSET(descs[ATTR_A]), AV_OPT_TYPE_STRING, { .str = "255" }, .flags = FLAGS },

    { "eof_action", "Action to take when encountering EOF", OFFSET(eof_action), AV_OPT_TYPE_INT, { .i64 = EXT_INFINITY }, EXT_STOP, EXT_INFINITY, FLAGS, "eof_action" },
        { "repeat", "Repeat the previous frame", 0, AV_OPT_TYPE_CONST, { .i64 = EXT_INFINITY }, .flags = FLAGS, "eof_action" },
        { "endall", "End both streams",          0, AV_OPT_TYPE_CONST, { .i64 = EXT_STOP },     .flags = FLAGS, "eof_action" },

    { NULL }
};

AVFILTER_DEFINE_CLASS(overlay_qsv);

static void free_layout(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;
    int i, j;

    if (s->ow_expr)    av_expr_free(s->ow_expr);
    if (s->oh_expr)    av_expr_free(s->oh_expr);

    if (s->iexpr) {
        for (i = 0; i < ctx->nb_inputs; i++)
            for (j = ATTR_X; j < ATTR_NUM; j++)
                if (s->iexpr[i][j])
                    av_expr_free(s->iexpr[i][j]);
        av_freep(&s->iexpr);
    }
    av_freep(&s->layouts);
}

static int setup_expr(AVFilterContext *ctx, const char *desc[][ATTR_NUM])
{
    QSVOverlayContext *s = ctx->priv;
    int i, j;

#define ALLOC_EXPR(expr, str, ctx) { \
    int err = av_expr_parse(&(expr), str, var_names, var_funcs, func_list, NULL, NULL, 0, ctx); \
    if (err < 0) \
        return err; \
}

    s->iexpr = av_mallocz_array(ctx->nb_inputs, sizeof(*s->iexpr));
    if (!s->iexpr)
        return AVERROR(ENOMEM);

    ALLOC_EXPR(s->ow_expr, s->out_w, ctx);
    ALLOC_EXPR(s->oh_expr, s->out_h, ctx);

    for (i = 0; i < ctx->nb_inputs; i++)
        for (j = ATTR_X; j < ATTR_NUM; j++)
            ALLOC_EXPR(s->iexpr[i][j], desc[i][j], ctx);

    return 0;
}

static void eval_iexpr(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;
    int i, j, k;

    for (k = ATTR_X; k < ATTR_NUM; k++) {
        for (i = 0; i < ctx->nb_inputs; i++) {
            QSVLayout *layout = s->layouts + i;
            int *values[ATTR_NUM] = {
                &layout->x, &layout->y, &layout->w, &layout->h, &layout->alpha
            };

            s->var_values[VAR_n] = i;
            for (j = ATTR_X; j < ATTR_NUM; j++)
                *values[j] = av_expr_eval(s->iexpr[i][j], s->var_values, ctx);
        }
    }
}

static void eval_oexpr(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;

    s->var_values[VAR_n] = 0;
    s->var_values[VAR_W] = av_expr_eval(s->ow_expr, s->var_values, ctx);
    s->var_values[VAR_H] = av_expr_eval(s->oh_expr, s->var_values, ctx);
    s->var_values[VAR_W] = av_expr_eval(s->ow_expr, s->var_values, ctx);
}

static void eval_expr(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;
    int i;

    for (i = 0; i < ctx->nb_inputs; i++) {
        s->layouts[i].iw = ctx->inputs[i]->w;
        s->layouts[i].ih = ctx->inputs[i]->h;
    }

    s->var_values[VAR_N] = ctx->nb_inputs;

    eval_oexpr(ctx);
    eval_iexpr(ctx);
    eval_oexpr(ctx);

    eval_factor(s->var_values[VAR_W], s->var_values[VAR_H], ctx->inputs[0]);

    eval_iexpr(ctx);
    for (i = 0; i < ctx->nb_inputs; i++)
        eval_factor(s->layouts[i].w, s->layouts[i].h, ctx->inputs[i]);
}

static int setup_overlay(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;
    const char *exprs[][ATTR_NUM] = {
        { "0", "0", "iw(0)", "ih(0)", "255" },
        { s->descs[ATTR_X], s->descs[ATTR_Y], s->descs[ATTR_W], s->descs[ATTR_H], s->descs[ATTR_A] },
    };

    return setup_expr(ctx, exprs);
}

static int setup_grid(AVFilterContext *ctx)
{
    static const char *desc[ATTR_NUM] = {
        "w(n)*mod(n,ceil(sqrt(N)))",
        "h(n)*floor(n/ceil(sqrt(N)))",
        "W/ceil(sqrt(N))",
        "H/ceil(sqrt(N))",
        "255"
    };
    const char *(*array)[ATTR_NUM] = av_mallocz_array(ctx->nb_inputs, sizeof(*array));
    int i, ret;

    if (!array) return AVERROR(ENOMEM);

    for (i = 0; i < ctx->nb_inputs; i++)
        memcpy(&array[i], &desc, sizeof(desc));

    ret = setup_expr(ctx, array);
    av_freep(&array);

    return ret;
}

static int setup_manual(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;
    const char *tmp, *(*sep_expr)[ATTR_NUM];
    int i, j, ret = 0;

    sep_expr = av_mallocz_array(ctx->nb_inputs, sizeof(*sep_expr));
    if (!sep_expr)
        return AVERROR(ENOMEM);

    for (i = ATTR_X; i < ATTR_NUM; i++) {
        if (!av_stristart(s->descs[i], "array", &tmp)) {
            for (j = 0; j < ctx->nb_inputs; j++)
                sep_expr[j][i] = av_strdup(s->descs[i]);
        } else {
            if (!tmp) {
                ret = AVERROR(EINVAL);
                goto failed;
            }

            tmp += strspn(tmp, "(");
            for (j = 0; j < ctx->nb_inputs && *tmp; j++, tmp++) {
                sep_expr[j][i] = av_get_token(&tmp, ",)");
                if (!sep_expr[j][i]) {
                    ret = AVERROR(ENOMEM);
                    goto failed;
                }
            }

            if (j < ctx->nb_inputs) {
                av_log(ctx, AV_LOG_ERROR, "Not enough exprs (%d < %d) around %s.\n",
                       j, i, s->descs[i]);
                ret = AVERROR(EINVAL);
                goto failed;
            }
        }
    }

    ret = setup_expr(ctx, sep_expr);

failed:
    for (i = 0; i < ctx->nb_inputs; i++)
        for (j = ATTR_X; j < ATTR_NUM; j++)
            av_freep(&sep_expr[i][j]);
    av_freep(&sep_expr);
            
    return ret;
}

static int setup_layout(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;

    s->layouts = av_mallocz_array(ctx->nb_inputs, sizeof(*s->layouts));
    if (!s->layouts)
        return AVERROR(ENOMEM);

    switch (s->layout) {
        case GRID:
            return setup_grid(ctx);
        case MANUAL:
            return setup_manual(ctx);
        case OVERLAY:
            return setup_overlay(ctx);
        default:
            return AVERROR(EINVAL);
    }
}

static int have_alpha_planar(AVFilterLink *link)
{
    enum AVPixelFormat pix_fmt;
    const AVPixFmtDescriptor *desc;
    AVHWFramesContext *fctx;

    if (link->format == AV_PIX_FMT_QSV) {
        fctx    = (AVHWFramesContext *)link->hw_frames_ctx->data;
        pix_fmt = fctx->sw_format;
    }

    if (!(desc = av_pix_fmt_desc_get(pix_fmt)))
        return 0;

    return !!(desc->flags & AV_PIX_FMT_FLAG_ALPHA);
}

/*
 * Callback from framesync.
 * Framesync will "on_event" and call this function once a new frame is pushed to fs
 * via main input.
 */
static int process_frame(FFFrameSync *fs)
{
    AVFilterContext  *ctx = fs->parent;
    QSVOverlayContext  *s = fs->opaque;
    AVFrame        *frame = NULL;
    int               ret = 0, i;

    for (i = 0; i < ctx->nb_inputs; i++) {
        ret = ff_framesync_get_frame(fs, i, &frame, 0);
        if (ret == 0)
            ret = ff_qsvvpp_filter_frame(s->qsv, ctx->inputs[i], frame);
        if (ret < 0)
            break;
    }

    if (ret < 0)
        ff_framesync_drop(fs);

    return ret;
}

static int init_framesync(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;
    int ret, i;

    ff_framesync_uninit(&s->fsync);

    s->fsync.on_event = process_frame;
    s->fsync.opaque   = s;
    ret = ff_framesync_init(&s->fsync, ctx, ctx->nb_inputs);
    if (ret < 0)
        return ret;

    for (i = 0; i < ctx->nb_inputs; i++) {
        FFFrameSyncIn *in = &s->fsync.in[i];
        in->before    = EXT_STOP;
        in->after     = s->eof_action;
        in->sync      = i ? 1 : 2;
        in->time_base = ctx->inputs[i]->time_base;
    }

    return ff_framesync_configure(&s->fsync);
}

static int filter_callback(AVFilterLink *outlink, AVFrame *frame)
{
    QSVOverlayContext *s = outlink->src->priv;

    frame->pts = av_rescale_q(s->fsync.pts,
                              s->fsync.time_base, outlink->time_base);
    return ff_filter_frame(outlink, frame);
}

static int init_qsvvpp(AVFilterContext *ctx)
{
    QSVOverlayContext    *s = ctx->priv;
    AVFilterLink       *in0 = ctx->inputs[0];
    AVFilterLink   *outlink = ctx->outputs[0];
    FFQSVVPPParam qsv_param = { NULL };
    mfxExtVPPComposite comp_conf;
    mfxExtBuffer *extbuff[1];
    int i, ret;

    comp_conf.Header.BufferId = MFX_EXTBUFF_VPP_COMPOSITE;
    comp_conf.Header.BufferSz = sizeof(comp_conf);
    comp_conf.Y               = 16;
    comp_conf.U               = 128;
    comp_conf.V               = 128;
    comp_conf.NumInputStream  = ctx->nb_inputs;
    comp_conf.InputStream     = av_mallocz_array(ctx->nb_inputs,
                                                 sizeof(*comp_conf.InputStream));
    if(!comp_conf.InputStream)
        return AVERROR(ENOMEM);

    for (i = 0; i < ctx->nb_inputs; i++) {
        mfxVPPCompInputStream *st = &comp_conf.InputStream[i];
        AVFilterLink      *inlink = ctx->inputs[i];
        QSVLayout         *layout = &s->layouts[i];

        st->DstX              = av_clip(layout->x, 0, outlink->w);
        st->DstY              = av_clip(layout->y, 0, outlink->h);
        st->DstW              = layout->w;
        st->DstH              = layout->h;
        st->GlobalAlpha       = layout->alpha;
        st->GlobalAlphaEnable = (st->GlobalAlpha < 255);
        st->PixelAlphaEnable  = have_alpha_planar(inlink);
        av_log(ctx, AV_LOG_VERBOSE, "Input[%d] is of %s, rect[%d,%d,%d,%d].\n",
               i, av_get_pix_fmt_name(inlink->format), st->DstX, st->DstY, st->DstW, st->DstH);
    }

    qsv_param.cb                    = filter_callback;
    qsv_param.vpp_param.AsyncDepth  = 4;
    qsv_param.vpp_param.NumExtParam = 1;
    qsv_param.vpp_param.ExtParam    = extbuff;
    qsv_param.vpp_param.ExtParam[0] = (mfxExtBuffer *)&comp_conf;

    ret = ff_qsvvpp_frameinfo_fill(&qsv_param.vpp_param.vpp.In, in0, 0);
    if (ret != 0) {
        av_log(ctx, AV_LOG_ERROR, "Invalid input param.\n");
        goto failed;
    }

    if (in0->format == AV_PIX_FMT_QSV)
        qsv_param.vpp_param.IOPattern = MFX_IOPATTERN_IN_VIDEO_MEMORY;
    else
        qsv_param.vpp_param.IOPattern = MFX_IOPATTERN_IN_SYSTEM_MEMORY;

    ret = ff_qsvvpp_frameinfo_fill(&qsv_param.vpp_param.vpp.Out, outlink, 1);
    if (ret != 0) {
        av_log(ctx, AV_LOG_ERROR, "Invalid output param.\n");
        goto failed;
    }

    if (outlink->format == AV_PIX_FMT_QSV)
        qsv_param.vpp_param.IOPattern |= MFX_IOPATTERN_OUT_VIDEO_MEMORY;
    else
        qsv_param.vpp_param.IOPattern |= MFX_IOPATTERN_OUT_SYSTEM_MEMORY;

    ret = ff_qsvvpp_create(ctx, &s->qsv, &qsv_param);

failed:
    av_freep(&comp_conf.InputStream);
    return ret;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    QSVOverlayContext *s = inlink->dst->priv;

    return ff_framesync_filter_frame(&s->fsync, inlink, frame);
}

static int request_frame(AVFilterLink *outlink)
{
    QSVOverlayContext *s = outlink->src->priv;

    return ff_framesync_request_frame(&s->fsync, outlink);
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    QSVOverlayContext  *s = ctx->priv;
    AVFilterLink    *in0 = ctx->inputs[0];
    int ret;

    eval_expr(ctx);

    outlink->w          = s->var_values[VAR_W];
    outlink->h          = s->var_values[VAR_H];
    outlink->frame_rate = in0->frame_rate;
    outlink->time_base  = av_inv_q(outlink->frame_rate);

    ret = init_framesync(ctx);
    if (ret < 0)
        return ret;

    return init_qsvvpp(ctx);
}

static int overlay_qsv_query_formats(AVFilterContext *ctx)
{
    int ret, i;

    const enum AVPixelFormat main_in_fmts[] = {
        AV_PIX_FMT_YUV420P,
        AV_PIX_FMT_NV12,
        AV_PIX_FMT_YUYV422,
        AV_PIX_FMT_RGB32,
        AV_PIX_FMT_QSV,
        AV_PIX_FMT_NONE
    };
    const enum AVPixelFormat out_pix_fmts[] = {
        AV_PIX_FMT_NV12,
        AV_PIX_FMT_QSV,
        AV_PIX_FMT_NONE
    };

    for(i = 0; i < ctx->nb_inputs; i++) {
        ret = ff_formats_ref(ff_make_format_list(main_in_fmts), &ctx->inputs[i]->out_formats);
        if (ret < 0)
            return ret;
    }

    return ff_formats_ref(ff_make_format_list(out_pix_fmts), &ctx->outputs[0]->in_formats);
}

static void overlay_qsv_uninit(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;
    int i;

    free_layout(ctx);
    ff_qsvvpp_free(&s->qsv);
    ff_framesync_uninit(&s->fsync);
    for (i = 0; i < ctx->nb_inputs; i++)
        av_freep(&ctx->input_pads[i].name);
}

static int overlay_qsv_init(AVFilterContext *ctx)
{
    QSVOverlayContext *s = ctx->priv;
    AVFilterPad inpad;
    int ret, idx;

    /* Force nb_inputs to 2 in overlay mode */
    if (s->layout == OVERLAY)
        s->nb_inputs = 2;

    for (idx = 0; idx < s->nb_inputs; idx++) {
        memset(&inpad, 0, sizeof(inpad));
        inpad.name         = NULL;
        inpad.type         = AVMEDIA_TYPE_VIDEO;
        inpad.filter_frame = filter_frame;
        ret = ff_insert_inpad(ctx, idx, &inpad);
        if (ret < 0)
            goto failed;
    }

    ret = setup_layout(ctx);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "setup layout failed.\n");
        goto failed;
    }

failed:
    if (ret < 0)
        overlay_qsv_uninit(ctx);

    return ret;
}

static const AVFilterPad overlay_qsv_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = config_output,
        .request_frame = request_frame,
    },
    { NULL }
};

AVFilter ff_vf_overlay_qsv = {
    .name           = "overlay_qsv",
    .description    = NULL_IF_CONFIG_SMALL("Quick Sync Video overlay."),
    .priv_size      = sizeof(QSVOverlayContext),
    .query_formats  = overlay_qsv_query_formats,
    .init           = overlay_qsv_init,
    .uninit         = overlay_qsv_uninit,
    .outputs        = overlay_qsv_outputs,
    .priv_class     = &overlay_qsv_class,
    .flags          = AVFILTER_FLAG_DYNAMIC_INPUTS,
    .flags_internal = FF_FILTER_FLAG_HWFRAME_AWARE,
};
