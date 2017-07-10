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
#include "libavutil/timestamp.h"
#include "libavutil/pixdesc.h"
#include "libavutil/eval.h"
#include "libavutil/hwcontext.h"
#include "internal.h"
#include "framesync.h"
#include "qsvvpp.h"

#define OFFSET(x) offsetof(QSVOverlayContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM
#define ARRAYSIZE(a) (sizeof(a)/sizeof((a)[0]))

enum var_name {
    VAR_MAIN_iW,     VAR_MW,
    VAR_MAIN_iH,     VAR_MH,
    VAR_OVERLAY_iW,
    VAR_OVERLAY_iH,
    VAR_OVERLAY_X,  VAR_OX,
    VAR_OVERLAY_Y,  VAR_OY,
    VAR_OVERLAY_W,  VAR_OW,
    VAR_OVERLAY_H,  VAR_OH,
    VAR_VARS_NB
};

typedef struct QSVOverlayContext {
    /*
     * This clas should always be put here,
     * as we have personal options.
     */
    AVClass *class;
    /*
     * Runtime object
     */
    FFFrameSync        fsync;     ///Used to sync dual inputs
    FFQSVVPPContext   *qsv;
    FFQSVVPPParam      qsv_param;
    mfxExtVPPComposite comp_conf;
    double             var_values[VAR_VARS_NB];
    /*
     * User-defined values.
     */
    char     *overlay_ox, *overlay_oy, *overlay_ow, *overlay_oh;
    uint16_t  overlay_alpha, overlay_pixel_alpha;
    enum FFFrameSyncExtMode eof_action;
} QSVOverlayContext;

static const char *const var_names[] = {
    "main_w",     "W",   /// input width of the main layer
    "main_h",     "H",   /// input height of the main layer
    "overlay_iw",        /// input width of the overlay layer
    "overlay_ih",        /// input height of the overlay layer
    "overlay_x",  "x",   /// x position of the overlay layer inside of main
    "overlay_y",  "y",   /// y position of the overlay layer inside of main
    "overlay_w",  "w",   /// output width of overlay layer
    "overlay_h",  "h",   /// output height of overlay layer
    NULL
};

static const AVOption overlay_qsv_options[] = {
    { "x", "Overlay x position", OFFSET(overlay_ox), AV_OPT_TYPE_STRING, {.str="0"}, 0, 255, .flags = FLAGS},
    { "y", "Overlay y position", OFFSET(overlay_oy), AV_OPT_TYPE_STRING, {.str="0"}, 0, 255, .flags = FLAGS},
    { "w", "Overlay width",      OFFSET(overlay_ow), AV_OPT_TYPE_STRING, {.str="overlay_iw"}, 0, 255, .flags = FLAGS},
    { "h", "Overlay height",     OFFSET(overlay_oh), AV_OPT_TYPE_STRING, {.str="overlay_ih*w/overlay_iw"}, 0, 255, .flags = FLAGS},
    { "alpha", "Overlay global alpha", OFFSET(overlay_alpha), AV_OPT_TYPE_INT, {.i64 = 255}, 0, 255, .flags = FLAGS},
    { "eof_action", "Action to take when encountering EOF from overlay input", OFFSET(eof_action), AV_OPT_TYPE_INT, { .i64 = EXT_INFINITY }, EXT_STOP, EXT_INFINITY, .flags = FLAGS, "eof_action" },
        { "repeat", "Repeat the previous frame.",   0, AV_OPT_TYPE_CONST, { .i64 = EXT_INFINITY }, .flags = FLAGS, "eof_action" },
        { "endall", "End both streams.",            0, AV_OPT_TYPE_CONST, { .i64 = EXT_STOP }, .flags = FLAGS, "eof_action" },

    { NULL }
};

AVFILTER_DEFINE_CLASS(overlay_qsv);

static int eval_expr(AVFilterContext *ctx)
{
    QSVOverlayContext *vpp        = ctx->priv;
    double            *var_values = vpp->var_values;
    AVExpr            *ox_expr, *oy_expr, *ow_expr, *oh_expr;
    int                ret;

#define PASS_EXPR(e, s) {\
    ret = av_expr_parse(&e, s, var_names, NULL, NULL, NULL, NULL, 0, ctx); \
    if (ret < 0) {\
        av_log(ctx, AV_LOG_ERROR, "Error when passing '%s'.\n", s);\
        return ret;\
    }\
}
    PASS_EXPR(ox_expr, vpp->overlay_ox);
    PASS_EXPR(oy_expr, vpp->overlay_oy);
    PASS_EXPR(ow_expr, vpp->overlay_ow);
    PASS_EXPR(oh_expr, vpp->overlay_oh);
#undef PASS_EXPR

    /*
     * Fill constant values. iW and iH are fixed values.
     */

    /*
     * Calculate the dynamic values.
     */
    var_values[VAR_OVERLAY_W] =
    var_values[VAR_OW]        = av_expr_eval(ow_expr, var_values, NULL);
    var_values[VAR_OVERLAY_H] =
    var_values[VAR_OH]        = av_expr_eval(oh_expr, var_values, NULL);
    ///calc again in case ow is relative to oh
    var_values[VAR_OVERLAY_W] =
    var_values[VAR_OW]        = av_expr_eval(ow_expr, var_values, NULL);

    var_values[VAR_OVERLAY_X] =
    var_values[VAR_OX]        = av_expr_eval(ox_expr, var_values, NULL);
    var_values[VAR_OVERLAY_Y] =
    var_values[VAR_OY]        = av_expr_eval(oy_expr, var_values, NULL);
    ///calc again in case ox is relative to oy
    var_values[VAR_OVERLAY_X] =
    var_values[VAR_OX]        = av_expr_eval(ox_expr, var_values, NULL);

    ///Calc overlay_w and overlay_h again incase they are relative to ox,oy.
    var_values[VAR_OVERLAY_W] =
    var_values[VAR_OW]        = av_expr_eval(ow_expr, var_values, NULL);
    var_values[VAR_OVERLAY_H] =
    var_values[VAR_OH]        = av_expr_eval(oh_expr, var_values, NULL);
    var_values[VAR_OVERLAY_W] =
    var_values[VAR_OW]        = av_expr_eval(ow_expr, var_values, NULL);

    av_expr_free(ox_expr);
    av_expr_free(oy_expr);
    av_expr_free(ow_expr);
    av_expr_free(oh_expr);

    return 0;
}

/*
 * Callback for qsvvpp.
 * @Note: When using composite, qsvvpp won't generate a proper PTS for
 * outputed frame. So we assign framesync's current PTS to the new filtered
 * frame.
 */
static int filter_callback(AVFilterLink *outlink, AVFrame *frame)
{
    QSVOverlayContext *vpp = outlink->src->priv;

    frame->pts = av_rescale_q(vpp->fsync.pts,
                              vpp->fsync.time_base, outlink->time_base);
    return ff_filter_frame(outlink, frame);
}

/*
 * Callback from framesync.
 * Framesync will "on_event" and call this function once a new frame is pushed to fs
 * via main input.
 */
static int fs_process_frame(FFFrameSync *fs)
{
    AVFilterContext   *ctx = fs->parent;
    QSVOverlayContext *vpp = fs->opaque;
    AVFrame           *mpic = NULL, *opic = NULL;
    int                ret = 0;

    /*
     * Request frames from each input of fs.
     */
    if ((ret = ff_framesync_get_frame(fs, 0, &mpic, 0)) < 0 ||
        (ret = ff_framesync_get_frame(fs, 1, &opic, 0)) < 0)
        goto err_out;

    if ((ret = ff_qsvvpp_filter_frame(vpp->qsv, ctx->inputs[0], mpic)) < 0 ||
        (ret = ff_qsvvpp_filter_frame(vpp->qsv, ctx->inputs[1], opic)) < 0)
        goto err_out;

err_out:
    /*
     * Once some frames are filtered with error, we drop this cycle.
     */
    if (ret < 0)
        ff_framesync_drop(fs);

    return ret;
}

static int have_alpha_planar(AVFilterLink *link)
{
    enum AVPixelFormat pix_fmt;
    const AVPixFmtDescriptor *desc;
    AVHWFramesContext *fctx;

    if (link->format == AV_PIX_FMT_QSV) {
        fctx = (AVHWFramesContext *)link->hw_frames_ctx->data;
        pix_fmt = fctx->sw_format;
    }

    if (!(desc = av_pix_fmt_desc_get(pix_fmt)))
        return 0;

    return !!(desc->flags & AV_PIX_FMT_FLAG_ALPHA);
}

static int overlay_qsv_filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    QSVOverlayContext *vpp = inlink->dst->priv;

    return ff_framesync_filter_frame(&vpp->fsync, inlink, frame);
}

static int overlay_qsv_config_main_input(AVFilterLink *inlink)
{
    AVFilterContext       *ctx = inlink->dst;
    QSVOverlayContext     *vpp = ctx->priv;
    FFFrameSyncIn         *in  = &vpp->fsync.in[0];
    mfxVPPCompInputStream *st  = &vpp->comp_conf.InputStream[0];
    int                    ret;

    av_log(ctx, AV_LOG_DEBUG, "Input[%d] is of %s.\n", FF_INLINK_IDX(inlink),
            av_get_pix_fmt_name(inlink->format));

    vpp->var_values[VAR_MAIN_iW] =
    vpp->var_values[VAR_MW]      = inlink->w;
    vpp->var_values[VAR_MAIN_iH] =
    vpp->var_values[VAR_MH]      = inlink->h;

    in->before    = EXT_STOP;
    in->after     = EXT_STOP;
    in->sync      = 2;
    in->time_base = inlink->time_base;

    st->DstX      = 0;
    st->DstY      = 0;
    st->DstW      = inlink->w;
    st->DstH      = inlink->h;
    st->GlobalAlphaEnable = 0;
    st->PixelAlphaEnable  = 0;

    /*
     * Fill in->frameinfo according to inlink
     */
    ret = ff_qsvvpp_frameinfo_fill(&vpp->qsv_param.vpp_param.vpp.In, inlink, 0);
    if (ret != 0) {
        av_log(ctx, AV_LOG_ERROR, "Invalid input param.\n");
        return ret;
    }

    if (inlink->format == AV_PIX_FMT_QSV)
        vpp->qsv_param.vpp_param.IOPattern = MFX_IOPATTERN_IN_VIDEO_MEMORY;
    else
        vpp->qsv_param.vpp_param.IOPattern = MFX_IOPATTERN_IN_SYSTEM_MEMORY;

    return 0;
}

static int overlay_qsv_config_overlay_input(AVFilterLink *inlink)
{
    AVFilterContext       *ctx = inlink->dst;
    AVFilterLink          *in0 = ctx->inputs[0];
    QSVOverlayContext     *vpp = ctx->priv;
    FFFrameSyncIn         *in  = &vpp->fsync.in[1];
    mfxVPPCompInputStream *st  = &vpp->comp_conf.InputStream[1];
    int                    ret;

    av_log(ctx, AV_LOG_DEBUG, "Input[%d] is of %s.\n", FF_INLINK_IDX(inlink),
            av_get_pix_fmt_name(inlink->format));
    if ((in0->format == AV_PIX_FMT_QSV && inlink->format != AV_PIX_FMT_QSV) ||
        (in0->format != AV_PIX_FMT_QSV && inlink->format == AV_PIX_FMT_QSV)) {
        av_log(ctx, AV_LOG_ERROR, "One of the inputs is of AV_PIX_FMT_QSV,"
                "but the other is of soft pixel format.\n");
        av_log(ctx, AV_LOG_ERROR, "HW/SW mixed format is not supported now.\n");
        return AVERROR(EINVAL);
    }

    vpp->var_values[VAR_OVERLAY_iW] = inlink->w;
    vpp->var_values[VAR_OVERLAY_iH] = inlink->h;
    ret = eval_expr(ctx);
    if (ret < 0)
        return ret;

    in->before    = EXT_STOP;
    in->after     = vpp->eof_action;
    in->sync      = 1;
    in->time_base = ctx->inputs[1]->time_base;

    st->DstX      = vpp->var_values[VAR_OX];
    st->DstY      = vpp->var_values[VAR_OY];
    st->DstW      = vpp->var_values[VAR_OW];
    st->DstH      = vpp->var_values[VAR_OH];
    st->GlobalAlpha       = vpp->overlay_alpha;
    st->GlobalAlphaEnable = (st->GlobalAlpha < 255);
    st->PixelAlphaEnable  = have_alpha_planar(inlink);

    return 0;
}

static int overlay_qsv_request_frame(AVFilterLink *outlink)
{
    QSVOverlayContext *vpp = outlink->src->priv;

    return ff_framesync_request_frame(&vpp->fsync, outlink);
}

static int overlay_qsv_config_output(AVFilterLink *outlink)
{
    AVFilterContext   *ctx = outlink->src;
    QSVOverlayContext *vpp = ctx->priv;
    AVFilterLink      *inlink = ctx->inputs[0];
    int                ret;

    ret = ff_framesync_configure(&vpp->fsync);
    if (ret != 0)
        return ret;

    outlink->w          = vpp->var_values[VAR_MW];
    outlink->h          = vpp->var_values[VAR_MH];
    outlink->frame_rate = inlink->frame_rate;
    outlink->time_base  = av_inv_q(outlink->frame_rate);

    /*
     * Fill out->frameinfo according to outlink
     */
    ret = ff_qsvvpp_frameinfo_fill(&vpp->qsv_param.vpp_param.vpp.Out, outlink, 1);
    if (ret != 0) {
        av_log(ctx, AV_LOG_ERROR, "Invalid output param.\n");
        return ret;
    }

    if (outlink->format == AV_PIX_FMT_QSV)
        vpp->qsv_param.vpp_param.IOPattern |= MFX_IOPATTERN_OUT_VIDEO_MEMORY;
    else
        vpp->qsv_param.vpp_param.IOPattern |= MFX_IOPATTERN_OUT_SYSTEM_MEMORY;

    vpp->qsv_param.sw_format = AV_PIX_FMT_NV12;

    return ff_qsvvpp_create(ctx, &vpp->qsv, &vpp->qsv_param);
}

static int overlay_qsv_query_formats(AVFilterContext *ctx)
{
    int ret, i;

    static const enum AVPixelFormat main_in_fmts[] = {
        AV_PIX_FMT_YUV420P,
        AV_PIX_FMT_NV12,
        AV_PIX_FMT_YUYV422,
        AV_PIX_FMT_RGB32,
        AV_PIX_FMT_QSV,
        AV_PIX_FMT_NONE
    };
    static const enum AVPixelFormat out_pix_fmts[] = {
        AV_PIX_FMT_NV12,
        AV_PIX_FMT_QSV,
        AV_PIX_FMT_NONE
    };

    for(i = 0; i < ctx->nb_inputs; i++) {
        ret = ff_formats_ref(ff_make_format_list(main_in_fmts), &ctx->inputs[i]->out_formats);
        if (ret < 0)
            return ret;
    }

    ret = ff_formats_ref(ff_make_format_list(out_pix_fmts), &ctx->outputs[0]->in_formats);
    if (ret < 0)
        return ret;

    return 0;
}

static int overlay_qsv_init(AVFilterContext *ctx)
{
    QSVOverlayContext *vpp = ctx->priv;
    int                ret;

    /*
     * A framesync object, used to sync up all inputs.
     * @Note: we consider the 1st input as the main input,
     * and frames won't be filtered out until it is fed in frames.
     */
    ret = ff_framesync_init(&vpp->fsync, ctx, ctx->nb_inputs);
    if (ret != 0)
        return ret;
    vpp->fsync.on_event = fs_process_frame;
    vpp->fsync.opaque   = vpp;

    /*
     * Fill composite config
     */
    vpp->comp_conf.Header.BufferId = MFX_EXTBUFF_VPP_COMPOSITE;
    vpp->comp_conf.Header.BufferSz = sizeof(vpp->comp_conf);
    vpp->comp_conf.NumInputStream  = ctx->nb_inputs;
    vpp->comp_conf.InputStream     =
            av_mallocz(sizeof(*vpp->comp_conf.InputStream) * ctx->nb_inputs);
    if (!vpp->comp_conf.InputStream)
        return AVERROR(ENOMEM);

    /*
     * Initiallize QSVVPP params.
     */
    vpp->qsv_param.cb = filter_callback;
    vpp->qsv_param.vpp_param.AsyncDepth  = 1;
    vpp->qsv_param.vpp_param.NumExtParam = 1; /// 1 for composite config
    vpp->qsv_param.vpp_param.ExtParam    =
            av_mallocz(sizeof(*vpp->qsv_param.vpp_param.ExtParam));
    if (!vpp->qsv_param.vpp_param.ExtParam)
        return AVERROR(ENOMEM);
    vpp->qsv_param.vpp_param.ExtParam[0] = (mfxExtBuffer *)&vpp->comp_conf;

    return 0;
}

static void overlay_qsv_uninit(AVFilterContext *ctx)
{
    QSVOverlayContext *vpp = ctx->priv;

    ff_qsvvpp_free(&vpp->qsv);
    av_freep(&vpp->comp_conf.InputStream);
    av_freep(&vpp->qsv_param.vpp_param.ExtParam);
    ff_framesync_uninit(&vpp->fsync);
}

static const AVFilterPad overlay_qsv_inputs[] = {
    {
        .name          = "main",
        .type          = AVMEDIA_TYPE_VIDEO,
        .filter_frame  = overlay_qsv_filter_frame,
        .config_props  = overlay_qsv_config_main_input,
    },
    {
        .name          = "overlay",
        .type          = AVMEDIA_TYPE_VIDEO,
        .filter_frame  = overlay_qsv_filter_frame,
        .config_props  = overlay_qsv_config_overlay_input,
    },
    { NULL }
};

static const AVFilterPad overlay_qsv_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = overlay_qsv_config_output,
        .request_frame = overlay_qsv_request_frame,
    },
    { NULL }
};

AVFilter ff_vf_overlay_qsv = {
    .name          = "overlay_qsv",
    .description   = NULL_IF_CONFIG_SMALL("Quick Sync Video overlay."),
    .priv_size     = sizeof(QSVOverlayContext),
    .query_formats = overlay_qsv_query_formats,
    .init          = overlay_qsv_init,
    .uninit        = overlay_qsv_uninit,
    .inputs        = overlay_qsv_inputs,
    .outputs       = overlay_qsv_outputs,
    .priv_class    = &overlay_qsv_class,
};
