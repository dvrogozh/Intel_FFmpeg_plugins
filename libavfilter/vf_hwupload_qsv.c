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

#include "config.h"
#include "libavutil/opt.h"
#include "libavutil/hwcontext.h"
#include "internal.h"
#include "qsvvpp.h"

#define OFFSET(x) offsetof(QSVHWUploadContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM
#define ARRAYSIZE(a) (sizeof(a)/sizeof((a)[0]))

typedef struct QSVHWUploadContext {
    /*
     * This clas should always be put here,
     * as we have personal options.
     */
    AVClass *class;
    /*
     * Runtime object
     */
    enum AVPixelFormat sw_format;
    FFQSVVPPContext *qsvvpp;
} QSVHWUploadContext;

static const AVOption hwupload_qsv_options[] = {
    { "sw_format", "Output software pixel format", OFFSET(sw_format),
            AV_OPT_TYPE_PIXEL_FMT, {.i64 = AV_PIX_FMT_NONE}, AV_PIX_FMT_NONE, AV_PIX_FMT_NB, .flags = FLAGS},
    { NULL }
};

static const enum AVPixelFormat in_fmts[] = {
    AV_PIX_FMT_NV12,
    AV_PIX_FMT_YUV420P,
    AV_PIX_FMT_YUYV422,
    AV_PIX_FMT_RGB32,
    AV_PIX_FMT_QSV,
    AV_PIX_FMT_NONE,
};

static const enum AVPixelFormat out_fmts[] = {
    AV_PIX_FMT_QSV,
    AV_PIX_FMT_NONE,
};

AVFILTER_DEFINE_CLASS(hwupload_qsv);

static int hwupload_qsv_filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    QSVHWUploadContext *s = inlink->dst->priv;
    int ret;

    ret =  ff_qsvvpp_filter_frame(s->qsvvpp, inlink, frame);
    av_frame_free(&frame);

    return ret;
}

static int hwupload_qsv_config_output(AVFilterLink *outlink)
{
    AVFilterContext  *ctx = outlink->src;
    QSVHWUploadContext *s = ctx->priv;
    AVFilterLink  *inlink = ctx->inputs[0];
    FFQSVVPPParam   param = { NULL };
    const enum AVPixelFormat *format;

    outlink->frame_rate = inlink->frame_rate;
    outlink->format     = AV_PIX_FMT_QSV;
    outlink->w          = inlink->w;
    outlink->h          = inlink->h;
    outlink->time_base  = av_inv_q(outlink->frame_rate);

    if (ff_qsvvpp_frameinfo_fill(&param.vpp_param.vpp.In, inlink, 0) < 0)
        return AVERROR(EINVAL);

    if (ff_qsvvpp_frameinfo_fill(&param.vpp_param.vpp.Out, outlink, 1) < 0)
        return AVERROR(EINVAL);

    if (s->sw_format == AV_PIX_FMT_NONE) {
        if (inlink->format == AV_PIX_FMT_QSV) {
            AVHWFramesContext *frame_ctx = (AVHWFramesContext*)inlink->hw_frames_ctx->data;
            s->sw_format = frame_ctx->sw_format;
        } else
            s->sw_format = inlink->format;
    }
    param.vpp_param.AsyncDepth = 4;
    param.vpp_param.NumExtParam = 0;
    param.vpp_param.IOPattern = MFX_IOPATTERN_IN_SYSTEM_MEMORY | MFX_IOPATTERN_OUT_VIDEO_MEMORY;

    for (format = in_fmts; *format != AV_PIX_FMT_QSV; format ++) {
        if (s->sw_format == *format)
            break;
    }

    if (*format == AV_PIX_FMT_QSV) {
        av_log(ctx, AV_LOG_ERROR, "Unsupported sw format.\n");
        return AVERROR(EINVAL);
    }

    param.sw_format = s->sw_format;

    return ff_qsvvpp_create(ctx, &s->qsvvpp, &param);
}

static int hwupload_qsv_query_formats(AVFilterContext *ctx)
{
    int ret;

    ret = ff_formats_ref(ff_make_format_list(in_fmts),
                         &ctx->inputs[0]->out_formats);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Registering in-formats fails.\n");
        return ret;
    }

    ret = ff_formats_ref(ff_make_format_list(out_fmts),
                         &ctx->outputs[0]->in_formats);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Registering out-formats fails.\n");
        return ret;
    }

    return 0;
}

static void hwupload_qsv_uninit(AVFilterContext *ctx)
{
    QSVHWUploadContext *s = ctx->priv;

    ff_qsvvpp_free(&s->qsvvpp);
}

static const AVFilterPad hwupload_qsv_inputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .filter_frame  = hwupload_qsv_filter_frame,
    },
    { NULL }
};

static const AVFilterPad hwupload_qsv_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = hwupload_qsv_config_output,
    },
    { NULL }
};

AVFilter ff_vf_hwupload_qsv = {
    .name          = "hwupload_qsv",
    .description   = NULL_IF_CONFIG_SMALL("Quick Sync Video hwupload."),
    .priv_size     = sizeof(QSVHWUploadContext),
    .query_formats = hwupload_qsv_query_formats,
    .uninit        = hwupload_qsv_uninit,
    .inputs        = hwupload_qsv_inputs,
    .outputs       = hwupload_qsv_outputs,
    .priv_class    = &hwupload_qsv_class,
};
