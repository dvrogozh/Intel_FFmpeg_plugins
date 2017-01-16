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

    int out_width;
    int out_height;

    int dpic;                   // destination picture structure
                                // -1 = unkown
                                // 0 = interlaced top field first
                                // 1 = progressive
                                // 2 = interlaced bottom field first

    int deinterlace;            // deinterlace mode : 0=off, 1=bob, 2=advanced
    int denoise;                // Enable Denoise algorithm. Level is the optional value from the interval [0; 100]
    int detail;                 // Enable Detail Enhancement algorithm.
                                // Level is the optional value from the interval [0; 100]
    int async_depth;            // async dept used by encoder
    int max_b_frames;           // maxiumum number of b frames used by encoder

    char *ow, *oh;
    int use_frc;                // use framerate conversion
    int vpp_ready;

} VPPContext;

static const AVOption vpp_options[] = {
    { "deinterlace", "deinterlace mode: 0=off, 1=bob, 2=advanced",             OFFSET(deinterlace),  AV_OPT_TYPE_INT, {.i64=0}, 0, MFX_DEINTERLACING_ADVANCED, .flags = FLAGS },
    { "denoise",     "denoise level [0, 100]",                                 OFFSET(denoise),      AV_OPT_TYPE_INT, {.i64=0}, 0, 100, .flags = FLAGS },
    { "detail",      "detail enhancement level [0, 100]",                      OFFSET(detail),       AV_OPT_TYPE_INT, {.i64=0}, 0, 100, .flags = FLAGS },
    { "dpic",        "dest pic struct: 0=tff, 1=progressive [default], 2=bff", OFFSET(dpic),         AV_OPT_TYPE_INT, {.i64 = 1 }, 0, 2, .flags = FLAGS },
    { "framerate",   "output framerate",                                       OFFSET(framerate),    AV_OPT_TYPE_RATIONAL, { .dbl = 0.0 },0, DBL_MAX, .flags = FLAGS },
    { "async_depth", "Maximum processing parallelism [default = 4]",           OFFSET(async_depth),  AV_OPT_TYPE_INT, { .i64 = 4 }, 0, INT_MAX, .flags = FLAGS },
    { "max_b_frames","Maximum number of b frames  [default = 3]",              OFFSET(max_b_frames), AV_OPT_TYPE_INT, { .i64 = 3 }, 0, INT_MAX, .flags = FLAGS },
    { "default", NULL, 0, AV_OPT_TYPE_CONST, { .i64 = MFX_GPUCOPY_DEFAULT }, MFX_GPUCOPY_DEFAULT, MFX_GPUCOPY_OFF, .flags = FLAGS, "gpu_copy" },
    { "on", NULL, 0, AV_OPT_TYPE_CONST, { .i64 = MFX_GPUCOPY_ON }, MFX_GPUCOPY_DEFAULT, MFX_GPUCOPY_OFF, .flags = FLAGS, "gpu_copy" },
    { "off", NULL, 0, AV_OPT_TYPE_CONST, { .i64 = MFX_GPUCOPY_OFF }, MFX_GPUCOPY_DEFAULT, MFX_GPUCOPY_OFF, .flags = FLAGS, "gpu_copy" },
    { "w", "Output video width", OFFSET(ow), AV_OPT_TYPE_STRING, {.str="iw"}, 0, 255, .flags = FLAGS },
    { "width", "Output video width", OFFSET(ow), AV_OPT_TYPE_STRING, {.str="iw"}, 0, 255, .flags = FLAGS },
    { "h", "Output video height", OFFSET(oh), AV_OPT_TYPE_STRING, {.str="w*ih/iw"}, 0, 255, .flags = FLAGS },
    { "height", "Output video height", OFFSET(oh), AV_OPT_TYPE_STRING, {.str="w*ih/iw"}, 0, 255, .flags = FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(vpp);

static const char *const var_names[] = {
    "iw", "in_w",
    "ih", "in_h",
    "ow", "out_w", "w",
    "oh", "out_h", "h",
    NULL
};

enum var_name {
    VAR_iW, VAR_IN_W,
    VAR_iH, VAR_IN_H,
    VAR_oW, VAR_OUT_W, VAR_W,
    VAR_oH, VAR_OUT_H, VAR_H,
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
    int     ret = 0;

    PASS_EXPR(w_expr, vpp->ow);
    PASS_EXPR(h_expr, vpp->oh);

    var_values[VAR_iW] =
    var_values[VAR_IN_W] = ctx->inputs[0]->w;

    var_values[VAR_iH] =
    var_values[VAR_IN_H] = ctx->inputs[0]->h;

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

    av_expr_free(w_expr);
    av_expr_free(h_expr);
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
        av_log(vpp->ctx, AV_LOG_ERROR, "VPP: Framerate conversion enabled\n");
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

    /* Fill in->frameinfo according to inlink */
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

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    VPPContext      *vpp = ctx->priv;
    int              ret = 0;

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

    ret = ff_qsvvpp_create(ctx, &vpp->qsv, &vpp->qsv_param);

    return ret;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *picref)
{
    int               ret  = 0;
    AVFilterContext  *ctx         = inlink->dst;
    VPPContext       *vpp         = ctx->priv;

    ret = ff_qsvvpp_filter_frame(vpp->qsv, inlink, picref);

    av_frame_free(&picref);

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
