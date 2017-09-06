/*
 * Intel MediaSDK Quick Sync Video VPP filter
 *
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

#include "internal.h"
#include <float.h>
#include <pthread.h>
#include "libavutil/parseutils.h"
#include "libavutil/eval.h"
#include "avfilter.h"
#include "libavutil/avassert.h"
#include "libavutil/opt.h"
#include "libavutil/time.h"
#include "libavutil/avstring.h"

#include <mfx/mfxvideo.h>

#include "libavutil/error.h"
#include "libavcodec/avcodec.h"
#include "libavformat/avformat.h"
#include "libavutil/pixdesc.h"
#include "libswscale/swscale.h"
#include "libavutil/fifo.h"
#include "libavutil/hwcontext.h"
#include "libavutil/hwcontext_qsv.h"
#include "qsvvpp.h"

#define OFFSET(x) offsetof(VPPContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

/* number of video enhancement filters */
#define ENH_FILTERS_COUNT           6

typedef struct {
    const AVClass *class;

    AVFilterContext *ctx;
    FFQSVVPPContext   *qsv;
    FFQSVVPPParam      qsv_param;

    AVRational framerate;                           // target framerate

    /* Video Enhancement Algorithms */
    mfxExtVPPDeinterlacing  deinterlace_conf;
    mfxExtVPPFrameRateConversion frc_conf;
    mfxExtVPPDenoise denoise_conf;
    mfxExtVPPDetail detail_conf;
    mfxExtVPPProcAmp procamp_conf;

    int out_width;
    int out_height;

    int deinterlace;            // deinterlace mode : 0=off, 1=bob, 2=advanced
    int denoise;                // Enable Denoise algorithm. Level is the optional value from the interval [0; 100]
    int detail;                 // Enable Detail Enhancement algorithm.
                                // Level is the optional value from the interval [0; 100]
    int async_depth;            // async dept used by encoder

    int use_crop;               // 1 = use crop; 0=none
    int crop_w;
    int crop_h;
    int crop_x;
    int crop_y;

    /* param for the procamp */
    int    procamp;            //enable the procamp
    float  hue;
    float  saturation;
    float  contrast;
    float  brightness;

    char *cx, *cy, *cw, *ch;
    char *ow, *oh;
    int use_frc;                // use framerate conversion
    int vpp_ready;

} VPPContext;

static const AVOption vpp_options[] = {
    { "deinterlace", "deinterlace mode: 0=off, 1=bob, 2=advanced",             OFFSET(deinterlace),  AV_OPT_TYPE_INT, {.i64=0}, 0, MFX_DEINTERLACING_ADVANCED, .flags = FLAGS },
    { "denoise",     "denoise level [0, 100]",                                 OFFSET(denoise),      AV_OPT_TYPE_INT, {.i64=0}, 0, 100, .flags = FLAGS },
    { "detail",      "detail enhancement level [0, 100]",                      OFFSET(detail),       AV_OPT_TYPE_INT, {.i64=0}, 0, 100, .flags = FLAGS },
    { "framerate",   "output framerate",                                       OFFSET(framerate),    AV_OPT_TYPE_RATIONAL, { .dbl = 0.0 },0, DBL_MAX, .flags = FLAGS },
    { "async_depth", "Maximum processing parallelism [default = 4]",           OFFSET(async_depth),  AV_OPT_TYPE_INT, { .i64 = 4 }, 0, INT_MAX, .flags = FLAGS },

    { "procamp",     "Enable ProcAmp",                                         OFFSET(procamp),    AV_OPT_TYPE_INT,   {.i64 = 0}, 0, 1, .flags = FLAGS},
    { "hue",         "ProcAmp hue",                                            OFFSET(hue),        AV_OPT_TYPE_FLOAT, {.dbl = 0.0 }, -180.0, 180.0, .flags = FLAGS},
    { "saturation",  "ProcAmp saturation",                                     OFFSET(saturation), AV_OPT_TYPE_FLOAT, {.dbl = 1.0 }, 0.0, 10.0, .flags = FLAGS},
    { "contrast",    "ProcAmp contrast",                                       OFFSET(contrast),   AV_OPT_TYPE_FLOAT, {.dbl = 1.0 }, 0.0, 10.0, .flags = FLAGS},
    { "brightness",  "ProcAmp brightness",                                     OFFSET(brightness), AV_OPT_TYPE_FLOAT, {.dbl = 0.0 }, -100.0, 100.0, .flags = FLAGS},

    { "cw",   "set the width crop area expression",       OFFSET(cw), AV_OPT_TYPE_STRING, {.str = "iw"}, CHAR_MIN, CHAR_MAX, FLAGS },
    { "ch",   "set the height crop area expression",      OFFSET(ch), AV_OPT_TYPE_STRING, {.str = "ih"}, CHAR_MIN, CHAR_MAX, FLAGS },
    { "cx",   "set the x crop area expression",           OFFSET(cx), AV_OPT_TYPE_STRING, {.str = "(in_w-out_w)/2"}, CHAR_MIN, CHAR_MAX, FLAGS },
    { "cy",   "set the y crop area expression",           OFFSET(cy), AV_OPT_TYPE_STRING, {.str = "(in_h-out_h)/2"}, CHAR_MIN, CHAR_MAX, FLAGS },

    { "w",      "Output video width",  OFFSET(ow), AV_OPT_TYPE_STRING, {.str="cw"}, 0, 255, .flags = FLAGS },
    { "width",  "Output video width",  OFFSET(ow), AV_OPT_TYPE_STRING, {.str="cw"}, 0, 255, .flags = FLAGS },
    { "h",      "Output video height", OFFSET(oh), AV_OPT_TYPE_STRING, {.str="w*ch/cw"}, 0, 255, .flags = FLAGS },
    { "height", "Output video height", OFFSET(oh), AV_OPT_TYPE_STRING, {.str="w*ch/cw"}, 0, 255, .flags = FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(vpp);

static const char *const var_names[] = {
    "iw", "in_w",
    "ih", "in_h",
    "ow", "out_w", "w",
    "oh", "out_h", "h",
    "cw",
    "ch",
    "cx",
    "cy",
    NULL
};

enum var_name {
    VAR_iW, VAR_IN_W,
    VAR_iH, VAR_IN_H,
    VAR_oW, VAR_OUT_W, VAR_W,
    VAR_oH, VAR_OUT_H, VAR_H,
    CW,
    CH,
    CX,
    CY,
    VAR_VARS_NB
};

static int eval_expr(AVFilterContext *ctx)
{
#define PASS_EXPR(e, s) {\
    ret = av_expr_parse(&e, s, var_names, NULL, NULL, NULL, NULL, 0, ctx); \
    if (ret < 0) {\
        av_log(ctx, AV_LOG_ERROR, "Error when passing '%s'.\n", s);\
        return ret;\
    }\
}
#define CALC_EXPR(e, v, i) {\
    i = v = av_expr_eval(e, var_values, NULL); \
}
    VPPContext *vpp = ctx->priv;
    double  var_values[VAR_VARS_NB] = { NAN };
    AVExpr *w_expr = NULL, *h_expr = NULL;
    AVExpr *cw_expr = NULL, *ch_expr = NULL;
    AVExpr *cx_expr = NULL, *cy_expr = NULL;
    int     ret = 0;

    PASS_EXPR(cw_expr, vpp->cw);
    PASS_EXPR(ch_expr, vpp->ch);

    PASS_EXPR(w_expr, vpp->ow);
    PASS_EXPR(h_expr, vpp->oh);

    PASS_EXPR(cx_expr, vpp->cx);
    PASS_EXPR(cy_expr, vpp->cy);

    var_values[VAR_iW] =
    var_values[VAR_IN_W] = ctx->inputs[0]->w;

    var_values[VAR_iH] =
    var_values[VAR_IN_H] = ctx->inputs[0]->h;

    //crop
    CALC_EXPR(cw_expr, var_values[CW], vpp->crop_w);
    CALC_EXPR(ch_expr, var_values[CH], vpp->crop_h);
    //calc again in case cw is relative to ch
    CALC_EXPR(cw_expr, var_values[CW], vpp->crop_w);

    CALC_EXPR(w_expr,
            var_values[VAR_OUT_W] = var_values[VAR_oW] = var_values[VAR_W],
            vpp->out_width);
    CALC_EXPR(h_expr,
            var_values[VAR_OUT_H] = var_values[VAR_oH] = var_values[VAR_H],
            vpp->out_height);
    ///calc again in case ow is relative to oh
    CALC_EXPR(w_expr,
            var_values[VAR_OUT_W] = var_values[VAR_oW] = var_values[VAR_W],
            vpp->out_width);


    CALC_EXPR(cx_expr, var_values[CX], vpp->crop_x);
    CALC_EXPR(cy_expr, var_values[CY], vpp->crop_y);
    //calc again in case cx is relative to cy
    CALC_EXPR(cx_expr, var_values[CX], vpp->crop_x);

    if((vpp->crop_w != var_values[VAR_iW]) || (vpp->crop_h != var_values[VAR_iH])) {
        vpp->use_crop = 1;
        av_log(NULL, AV_LOG_INFO, "Using the crop feature \n");
     }

    av_expr_free(w_expr);
    av_expr_free(h_expr);
    av_expr_free(cw_expr);
    av_expr_free(ch_expr);
    av_expr_free(cx_expr);
    av_expr_free(cy_expr);
#undef PASS_EXPR
#undef CALC_EXPR

    return ret;
}

static int config_vpp_param(VPPContext *vpp)
{
    mfxU16  num_param = 0;
    mfxVideoParam *pParam;
    mfxExtBuffer** pExtParam;

    pParam = &vpp->qsv_param.vpp_param;

    if ((pParam->vpp.In.FrameRateExtN * pParam->vpp.Out.FrameRateExtD) !=
        (pParam->vpp.Out.FrameRateExtN * pParam->vpp.In.FrameRateExtD)) {
        vpp->use_frc = 1;
        av_log(vpp->ctx, AV_LOG_INFO, "VPP: Framerate conversion enabled\n");
    }
    else
        vpp->use_frc = 0;

    vpp->qsv_param.vpp_param.ExtParam    =
         av_mallocz(ENH_FILTERS_COUNT * sizeof(*vpp->qsv_param.vpp_param.ExtParam));
    if (!vpp->qsv_param.vpp_param.ExtParam)
        return AVERROR(ENOMEM);

     pExtParam = vpp->qsv_param.vpp_param.ExtParam;

    if (vpp->deinterlace) {
        memset(&vpp->deinterlace_conf, 0, sizeof(mfxExtVPPDeinterlacing));
        vpp->deinterlace_conf.Header.BufferId = MFX_EXTBUFF_VPP_DEINTERLACING;
        vpp->deinterlace_conf.Header.BufferSz = sizeof(mfxExtVPPDeinterlacing);
        vpp->deinterlace_conf.Mode = vpp->deinterlace == 1 ? MFX_DEINTERLACING_BOB : MFX_DEINTERLACING_ADVANCED;

        pExtParam[num_param++] = (mfxExtBuffer*)&(vpp->deinterlace_conf);
    }

    if (vpp->use_frc) {
        memset(&vpp->frc_conf, 0, sizeof(mfxExtVPPFrameRateConversion));
        vpp->frc_conf.Header.BufferId = MFX_EXTBUFF_VPP_FRAME_RATE_CONVERSION;
        vpp->frc_conf.Header.BufferSz = sizeof(mfxExtVPPFrameRateConversion);
        vpp->frc_conf.Algorithm = MFX_FRCALGM_DISTRIBUTED_TIMESTAMP; // make optional

        pExtParam[num_param++] = (mfxExtBuffer*)&(vpp->frc_conf);
    }

    if (vpp->denoise) {
        memset(&vpp->denoise_conf, 0, sizeof(mfxExtVPPDenoise));
        vpp->denoise_conf.Header.BufferId = MFX_EXTBUFF_VPP_DENOISE;
        vpp->denoise_conf.Header.BufferSz = sizeof(mfxExtVPPDenoise);
        vpp->denoise_conf.DenoiseFactor   = vpp->denoise;

        pExtParam[num_param++] = (mfxExtBuffer*)&(vpp->denoise_conf);
    }

    if (vpp->detail) {
        memset(&vpp->detail_conf, 0, sizeof(mfxExtVPPDetail));
        vpp->detail_conf.Header.BufferId  = MFX_EXTBUFF_VPP_DETAIL;
        vpp->detail_conf.Header.BufferSz  = sizeof(mfxExtVPPDetail);
        vpp->detail_conf.DetailFactor = vpp->detail;

        pExtParam[num_param++] = (mfxExtBuffer*)&(vpp->detail_conf);
    }

    if (vpp->procamp) {
        memset(&vpp->procamp_conf, 0, sizeof(mfxExtVPPProcAmp));
        vpp->procamp_conf.Header.BufferId  = MFX_EXTBUFF_VPP_PROCAMP;
        vpp->procamp_conf.Header.BufferSz  = sizeof(mfxExtVPPProcAmp);
        vpp->procamp_conf.Hue              = vpp->hue;
        vpp->procamp_conf.Saturation       = vpp->saturation;
        vpp->procamp_conf.Contrast         = vpp->contrast;
        vpp->procamp_conf.Brightness       = vpp->brightness;

        pExtParam[num_param++] = (mfxExtBuffer*)&(vpp->procamp_conf);
    }
    vpp->qsv_param.vpp_param.NumExtParam = num_param;
    vpp->vpp_ready = 1;

    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    VPPContext      *vpp = ctx->priv;
    int              ret;

    if (vpp->framerate.den == 0 || vpp->framerate.num == 0)
        vpp->framerate = inlink->frame_rate;

    if (vpp->out_height == 0 || vpp->out_width == 0) {
        vpp->out_width  = inlink->w;
        vpp->out_height = inlink->h;
    }

    ret = eval_expr(ctx);

    if (vpp->use_crop) {
        if (vpp->crop_x < 0)  vpp->crop_x = 0;
        if (vpp->crop_y < 0)  vpp->crop_y = 0;

        if(vpp->crop_w % 2)       vpp->crop_w++;
        if(vpp->crop_h % 2)       vpp->crop_h++;
        if(vpp->out_width % 2)    vpp->out_width++;
        if(vpp->out_height % 2)   vpp->out_height++;

        if(vpp->crop_w + vpp->crop_x > inlink->w)
           vpp->crop_x = inlink->w - vpp->crop_w;
        if(vpp->crop_h + vpp->crop_y > inlink->h)
           vpp->crop_y = inlink->h - vpp->crop_h;

        av_log(vpp->ctx, AV_LOG_DEBUG, "Crop paramters: x=%d y=%d w=%d h=%d \n",
               vpp->crop_x, vpp->crop_y, vpp->crop_w, vpp->crop_h);
    }

    /* Fill in->frameinfo according to inlink */
    ret = ff_qsvvpp_frameinfo_fill(&vpp->qsv_param.vpp_param.vpp.In, inlink, 0);
    if (ret != 0) {
        av_log(ctx, AV_LOG_ERROR, "Invalid input param.\n");
        return ret;
    }

    /* Fill the crop info */
    if (vpp->use_crop) {
        vpp->qsv_param.vpp_param.vpp.In.CropX = vpp->crop_x;
        vpp->qsv_param.vpp_param.vpp.In.CropY = vpp->crop_y;
        vpp->qsv_param.vpp_param.vpp.In.CropW = vpp->crop_w;
        vpp->qsv_param.vpp_param.vpp.In.CropH = vpp->crop_h;

        av_log(vpp->ctx, AV_LOG_DEBUG, "Crop parameters: x=%d y=%d w=%d h=%d \n",
               vpp->crop_x, vpp->crop_y, vpp->crop_w,vpp->crop_h);
    }

    if (inlink->format == AV_PIX_FMT_QSV)
        vpp->qsv_param.vpp_param.IOPattern = MFX_IOPATTERN_IN_VIDEO_MEMORY;
    else
        vpp->qsv_param.vpp_param.IOPattern = MFX_IOPATTERN_IN_SYSTEM_MEMORY;

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    VPPContext      *vpp = ctx->priv;
    int              ret = 0;
    AVFilterLink *inlink = ctx->inputs[0];

    outlink->w             = vpp->out_width;
    outlink->h             = vpp->out_height;
    outlink->frame_rate    = vpp->framerate;
    outlink->time_base     = av_inv_q(vpp->framerate);

    /* Fill out->frameinfo according to outlink */
    ret = ff_qsvvpp_frameinfo_fill(&vpp->qsv_param.vpp_param.vpp.Out, outlink, 1);
    if (ret != 0) {
        av_log(ctx, AV_LOG_ERROR, "Invalid output param.\n");
        return ret;
    }

    if(!vpp->vpp_ready)
       config_vpp_param(vpp);

    if (outlink->format == AV_PIX_FMT_QSV)
        vpp->qsv_param.vpp_param.IOPattern |= MFX_IOPATTERN_OUT_VIDEO_MEMORY;
    else
        vpp->qsv_param.vpp_param.IOPattern |= MFX_IOPATTERN_OUT_SYSTEM_MEMORY;

    if (vpp->use_frc || vpp->use_crop || vpp->deinterlace || vpp->denoise ||
        vpp->detail || inlink->w != outlink->w || inlink->h != outlink->h)
        ret = ff_qsvvpp_create(ctx, &vpp->qsv, &vpp->qsv_param);
    else
        av_log(ctx, AV_LOG_VERBOSE, "QSVVPP pass through mode.\n");

    return ret;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *picref)
{
    int               ret = 0;
    AVFilterContext  *ctx = inlink->dst;
    VPPContext       *vpp = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];

    if (vpp->qsv) {
        ret = ff_qsvvpp_filter_frame(vpp->qsv, inlink, picref);
        av_frame_free(&picref);
    } else {
        if (picref->pts != AV_NOPTS_VALUE)
            picref->pts = av_rescale_q(picref->pts, inlink->time_base, outlink->time_base);
        ret = ff_filter_frame(outlink, picref);
    }

    return ret;
}

static int query_formats(AVFilterContext *ctx)
{
    AVFilterFormats *in_fmts, *out_fmts;
    static const enum AVPixelFormat in_pix_fmts[] = {
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

    in_fmts  = ff_make_format_list(in_pix_fmts);
    out_fmts = ff_make_format_list(out_pix_fmts);
    ff_formats_ref(in_fmts, &ctx->inputs[0]->out_formats);
    ff_formats_ref(out_fmts, &ctx->outputs[0]->in_formats);

    return 0;
}

static av_cold int vpp_init(AVFilterContext *ctx)
{
    VPPContext *vpp= ctx->priv;

    vpp->vpp_ready       = 0;
    vpp->ctx             = ctx;

    return 0;
}

static av_cold void vpp_uninit(AVFilterContext *ctx)
{
    VPPContext *vpp = ctx->priv;

    ff_qsvvpp_free(&vpp->qsv);
    av_freep(&vpp->qsv_param.vpp_param.ExtParam);
}

static const AVFilterPad vpp_inputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = config_input,
        .filter_frame  = filter_frame,

    },
    { NULL }
};

static const AVFilterPad vpp_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = config_output,
    },
    { NULL }
};

AVFilter ff_vf_vpp_qsv = {
    .name          = "vpp_qsv",
    .description   = NULL_IF_CONFIG_SMALL("Quick Sync Video VPP."),
    .priv_size     = sizeof(VPPContext),
    .query_formats = query_formats,
    .init          = vpp_init,
    .uninit        = vpp_uninit,
    .inputs        = vpp_inputs,
    .outputs       = vpp_outputs,
    .priv_class    = &vpp_class,
    .flags_internal = FF_FILTER_FLAG_HWFRAME_AWARE,
};
