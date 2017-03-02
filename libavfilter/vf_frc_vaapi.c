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
#include <string.h>

#include <va/va.h>
#include <va/va_vpp.h>

#include "libavutil/avassert.h"
#include "libavutil/hwcontext.h"
#include "libavutil/hwcontext_vaapi.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"

#include "avfilter.h"
#include "formats.h"
#include "internal.h"

#define MAX_OUT_SURFACE 5
#define MAX_IN_BUFFER 2

typedef struct FrcVAAPIContext {
    const AVClass *class;

    AVVAAPIDeviceContext *hwctx;
    AVBufferRef *device_ref;

    int valid_ids;
    VAConfigID  va_config;
    VAContextID va_context;

    AVBufferRef       *input_frames_ref;
    AVHWFramesContext *input_frames;

    AVBufferRef       *output_frames_ref;
    AVHWFramesContext *output_frames;

    enum AVPixelFormat output_format;
    int output_width;
    int output_height;
    int output_framerate;

    int queue_depth;
    int queue_count;
    int cyclic_cnt;

    int64_t first_pts;
    int frame_out;
    int out_finished;
    AVFrame *first_frame;

    AVFrame *output_frame[MAX_OUT_SURFACE];
    VABufferID filter_buffer[MAX_OUT_SURFACE];
    VABufferID pipeline_buffer[MAX_IN_BUFFER];
} FrcVAAPIContext;


static int frc_vaapi_query_formats(AVFilterContext *avctx)
{
    enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_VAAPI, AV_PIX_FMT_NONE,
    };

    ff_formats_ref(ff_make_format_list(pix_fmts),
                   &avctx->inputs[0]->out_formats);
    ff_formats_ref(ff_make_format_list(pix_fmts),
                   &avctx->outputs[0]->in_formats);

    return 0;
}

static int frc_vaapi_pipeline_uninit(FrcVAAPIContext *ctx)
{
    if (ctx->va_context != VA_INVALID_ID) {
        vaDestroyContext(ctx->hwctx->display, ctx->va_context);
        ctx->va_context = VA_INVALID_ID;
    }

    for(int i = 0; i < MAX_OUT_SURFACE; i++) {
        if (ctx->filter_buffer[i] != VA_INVALID_ID) {
            vaDestroyBuffer(ctx->hwctx->display, ctx->filter_buffer[i]);
            ctx->filter_buffer[i] = VA_INVALID_ID;
        }
    }
    for (int i = 0; i < MAX_IN_BUFFER; i++) {
        if (ctx->pipeline_buffer[i] != VA_INVALID_ID) {
            vaDestroyBuffer(ctx->hwctx->display, ctx->pipeline_buffer[i]);
            ctx->pipeline_buffer[i] = VA_INVALID_ID;
        }
    }

    if (ctx->va_config != VA_INVALID_ID) {
        vaDestroyConfig(ctx->hwctx->display, ctx->va_config);
        ctx->va_config = VA_INVALID_ID;
    }

    av_buffer_unref(&ctx->output_frames_ref);
    av_buffer_unref(&ctx->device_ref);
    ctx->hwctx = 0;

    return 0;
}

static int frc_vaapi_config_input(AVFilterLink *inlink)
{
    AVFilterContext *avctx = inlink->dst;
    FrcVAAPIContext *ctx = avctx->priv;

    frc_vaapi_pipeline_uninit(ctx);

    if (!inlink->hw_frames_ctx) {
        av_log(avctx, AV_LOG_ERROR, "A hardware frames reference is "
               "required to associate the processing device.\n");
        return AVERROR(EINVAL);
    }

    ctx->input_frames_ref = av_buffer_ref(inlink->hw_frames_ctx);
    ctx->input_frames = (AVHWFramesContext*)ctx->input_frames_ref->data;

    return 0;
}


#define IS_24FPS (ctx->output_framerate == 24)
#define IN_BUF_NUM (IS_24FPS ? 2 : 1)
#define OUT_BUF_NUM (IS_24FPS ? 5 : 2)

static int frc_vaapi_build_filter_params(AVFilterContext *avctx, AVFilterLink *outlink)
{
    FrcVAAPIContext *ctx = avctx->priv;
    VAStatus vas;
    VAProcFilterParameterBufferFrameRateConversion frc_params;
    AVRational time_base = {1 , 60};
    AVRational frame_rate = {60 , 1};

    if (avctx->inputs[0]->frame_rate.num == 24000)
        ctx->output_framerate = 24;
    else
        ctx->output_framerate = 30;
    memset(&frc_params, 0, sizeof(frc_params));

    for (int i = 0; i < OUT_BUF_NUM; i++) {
        frc_params.type = VAProcFilterFrameRateConversion;
        if (IS_24FPS) {
            frc_params.input_fps = 24;
            frc_params.output_fps = 60;
            frc_params.num_output_frames = 5;
            frc_params.repeat_frame = 0;
        } else {
            frc_params.input_fps = 30;
            frc_params.output_fps = 60;
            frc_params.num_output_frames = 2;
            frc_params.repeat_frame = 0;
        }
        av_assert0(ctx->filter_buffer[i] == VA_INVALID_ID);
        vas = vaCreateBuffer(ctx->hwctx->display, ctx->va_context,
                             VAProcFilterParameterBufferType,
                             sizeof(frc_params), 1, &frc_params,
                             &ctx->filter_buffer[i]);
        if (vas != VA_STATUS_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR, "Failed to create frame rate conversion "
                   "parameter buffer: %d (%s).\n", vas, vaErrorStr(vas));
            return AVERROR(EIO);
        }
    }

    outlink->time_base = time_base;
    outlink->frame_rate = frame_rate;

    return 0;
}

static int frc_vaapi_config_output(AVFilterLink *outlink)
{
    AVFilterContext *avctx = outlink->src;
    FrcVAAPIContext *ctx = avctx->priv;
    AVVAAPIHWConfig *hwconfig = NULL;
    AVHWFramesConstraints *constraints = NULL;
    AVVAAPIFramesContext *va_frames;
    VAStatus vas;
    int err, i;

    frc_vaapi_pipeline_uninit(ctx);

    ctx->device_ref = av_buffer_ref(ctx->input_frames->device_ref);
    ctx->hwctx = ((AVHWDeviceContext*)ctx->device_ref->data)->hwctx;

    ctx->output_width = ctx->input_frames->width;
    ctx->output_height = ctx->input_frames->height;

    av_assert0(ctx->va_config == VA_INVALID_ID);
    vas = vaCreateConfig(ctx->hwctx->display, VAProfileNone,
                         VAEntrypointVideoProc, 0, 0, &ctx->va_config);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(ctx, AV_LOG_ERROR, "Failed to create processing pipeline "
               "config: %d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail;
    }

    hwconfig = av_hwdevice_hwconfig_alloc(ctx->device_ref);
    if (!hwconfig) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    hwconfig->config_id = ctx->va_config;

    constraints = av_hwdevice_get_hwframe_constraints(ctx->device_ref,
                                                      hwconfig);
    if (!constraints) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    if (ctx->output_format == AV_PIX_FMT_NONE)
        ctx->output_format = ctx->input_frames->sw_format;
    if (constraints->valid_sw_formats) {
        for (i = 0; constraints->valid_sw_formats[i] != AV_PIX_FMT_NONE; i++) {
            if (ctx->output_format == constraints->valid_sw_formats[i])
                break;
        }
        if (constraints->valid_sw_formats[i] == AV_PIX_FMT_NONE) {
            av_log(ctx, AV_LOG_ERROR, "Hardware does not support output "
                   "format %s.\n", av_get_pix_fmt_name(ctx->output_format));
            err = AVERROR(EINVAL);
            goto fail;
        }
    }

    if (ctx->output_width  < constraints->min_width  ||
        ctx->output_height < constraints->min_height ||
        ctx->output_width  > constraints->max_width  ||
        ctx->output_height > constraints->max_height) {
        av_log(ctx, AV_LOG_ERROR, "Hardware does not support scaling to "
               "size %dx%d (constraints: width %d-%d height %d-%d).\n",
               ctx->output_width, ctx->output_height,
               constraints->min_width,  constraints->max_width,
               constraints->min_height, constraints->max_height);
        err = AVERROR(EINVAL);
        goto fail;
    }

    ctx->output_frames_ref = av_hwframe_ctx_alloc(ctx->device_ref);
    if (!ctx->output_frames_ref) {
        av_log(ctx, AV_LOG_ERROR, "Failed to create HW frame context "
               "for output.\n");
        err = AVERROR(ENOMEM);
        goto fail;
    }

    ctx->output_frames = (AVHWFramesContext*)ctx->output_frames_ref->data;

    ctx->output_frames->format    = AV_PIX_FMT_VAAPI;
    ctx->output_frames->sw_format = ctx->output_format;
    ctx->output_frames->width     = ctx->output_width;
    ctx->output_frames->height    = ctx->output_height;

    // The number of output frames we need is determined by what follows
    // the filter.  If it's an encoder with complex frame reference
    // structures then this could be very high.
    ctx->output_frames->initial_pool_size = 10;

    err = av_hwframe_ctx_init(ctx->output_frames_ref);
    if (err < 0) {
        av_log(ctx, AV_LOG_ERROR, "Failed to initialise VAAPI frame "
               "context for output: %d\n", err);
        goto fail;
    }

    va_frames = ctx->output_frames->hwctx;

    av_assert0(ctx->va_context == VA_INVALID_ID);
    vas = vaCreateContext(ctx->hwctx->display, ctx->va_config,
                          ctx->output_width, ctx->output_height,
                          VA_PROGRESSIVE,
                          va_frames->surface_ids, va_frames->nb_surfaces,
                          &ctx->va_context);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(ctx, AV_LOG_ERROR, "Failed to create processing pipeline "
               "context: %d (%s).\n", vas, vaErrorStr(vas));
        return AVERROR(EIO);
    }


    err = frc_vaapi_build_filter_params(avctx, outlink);
    if (err < 0)
        goto fail;

    outlink->w = ctx->output_width;
    outlink->h = ctx->output_height;
    outlink->hw_frames_ctx = av_buffer_ref(ctx->output_frames_ref);
    if (!outlink->hw_frames_ctx) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    av_freep(&hwconfig);
    av_hwframe_constraints_free(&constraints);
    return 0;

fail:
    av_buffer_unref(&ctx->output_frames_ref);
    av_freep(&hwconfig);
    av_hwframe_constraints_free(&constraints);
    return err;
}

static int vaapi_proc_colour_standard(enum AVColorSpace av_cs)
{
    switch(av_cs) {
#define CS(av, va) case AVCOL_SPC_ ## av: return VAProcColorStandard ## va;
        CS(BT709,     BT709);
        CS(BT470BG,   BT601);
        CS(SMPTE170M, SMPTE170M);
        CS(SMPTE240M, SMPTE240M);
#undef CS
    default:
        return VAProcColorStandardNone;
    }
}


    //only support 24 fps -> 60 fps and 30 fps -> 60 fps , while 24 fps to 60 fps
    //we will use the income two frames and output five frames;
    //while 30 fps to 60 fps we will use income a frame and output two frames
static int frc_vaapi_filter_frame(AVFilterLink *inlink, AVFrame *input_frame)
{
    AVFilterContext *avctx = inlink->dst;
    AVFilterLink *outlink = avctx->outputs[0];
    FrcVAAPIContext *ctx = avctx->priv;

    //use the 24 fps surface output frame num as max out surface num
    VASurfaceID input_surface, output_surface[MAX_OUT_SURFACE];
    VABufferID in_buffer, out_buffer;
    AVFrame *failed_frame;
    VAProcPipelineParameterBuffer params;
    VAProcFilterParameterBufferFrameRateConversion *filter_params;
    void *filter_params_addr = NULL;
    VARectangle input_region;
    VAStatus vas;
    int err;

    av_log(ctx, AV_LOG_DEBUG, "Filter input: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(input_frame->format),
           input_frame->width, input_frame->height, input_frame->pts);

    if (ctx->va_context == VA_INVALID_ID)
        return AVERROR(EINVAL);
    input_surface = (VASurfaceID)(uintptr_t)input_frame->data[3];
    av_log(ctx, AV_LOG_DEBUG, "Using surface %#x for frame rate conversion input.\n",
           input_surface);

    //in 24 fps mode ,it will render picture after the next frame income.
    if (!ctx->out_finished  && IS_24FPS) {
        ctx->first_frame= input_frame;
        ctx->out_finished = 1;
        return 0;
    }

    memset(&params, 0, sizeof(params));

    input_region = (VARectangle) {
        .x      = 0,
        .y      = 0,
        .width  = input_frame->width,
        .height = input_frame->height,
    };


    //configure the VAProcPipelineParameterBuffer for pipeline
    //in 24 fps mode the pipeline_buffer[0] will save the 1st frame param buffer
    //and pipeline_buffer[1] will save the 2nd frame param buffer
    //in 30 fps mode the only use the pipeline_buffer[0] save the frame param buffer
    params.surface_region = &input_region;
    params.surface_color_standard = vaapi_proc_colour_standard(
            input_frame->colorspace);

    params.output_region = NULL;
    params.output_background_color = 0xff000000;
    params.output_color_standard = params.surface_color_standard;

    params.pipeline_flags = 0;
    params.filter_flags = VA_FRAME_PICTURE;

    params.filters     = ctx->filter_buffer;
    params.num_filters = OUT_BUF_NUM;

    for (int i = 0; i < IN_BUF_NUM; i++) {
        if (i == 0 && IS_24FPS)
            params.surface = (VASurfaceID)(uintptr_t)ctx->first_frame->data[3];//first surface
        else
            params.surface = input_surface;
        in_buffer = ctx->pipeline_buffer[i];
        vas = vaCreateBuffer(ctx->hwctx->display, ctx->va_context,
                             VAProcPipelineParameterBufferType,
                             sizeof(params), 1, &params, &ctx->pipeline_buffer[i]);
        if (vas != VA_STATUS_SUCCESS) {
            av_log(ctx, AV_LOG_ERROR, "Failed to create parameter buffer: "
                   "%d (%s).\n", vas, vaErrorStr(vas));
            err = AVERROR(EIO);
            goto fail_after_begin;
        }
        av_log(ctx, AV_LOG_DEBUG, "Frame rate conversion parameter buffer is %#x.\n",
                ctx->pipeline_buffer[i]);
    }

    //configure and render output pictures
    for (int i = 0; i < OUT_BUF_NUM; i++) {
        ctx->output_frame[i] = av_frame_alloc();
        if (!ctx->output_frame[i]) {
            av_log(ctx, AV_LOG_ERROR, "Failed to allocate output frame.");
            err = AVERROR(ENOMEM);
            goto fail;
        }
 
        failed_frame = ctx->output_frame[i];
        err = av_hwframe_get_buffer(ctx->output_frames_ref, ctx->output_frame[i], 0);
        if (err < 0) {
            av_log(ctx, AV_LOG_ERROR, "Failed to get surface for "
                   "output: %d\n.", err);
        }
        output_surface[i] = (VASurfaceID)(uintptr_t)ctx->output_frame[i]->data[3];
        av_log(ctx, AV_LOG_DEBUG, "Using surface %#x for frc output.\n",
                   output_surface[i]);

        //configure each output picuture's param
        out_buffer = ctx->filter_buffer[i];
        vas = vaMapBuffer(ctx->hwctx->display, ctx->filter_buffer[i],
                                      &filter_params_addr);
        if (vas != VA_STATUS_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR, "Failed to map filter parameter "
                   "buffer: %d (%s).\n", vas, vaErrorStr(vas));
            err = AVERROR(EIO);
            goto fail;
        }
        filter_params = filter_params_addr;

        filter_params->cyclic_counter = ctx->cyclic_cnt++;    //
        if ((filter_params->input_fps == 30) && (ctx->cyclic_cnt == 2))
            ctx->cyclic_cnt = 0;
        if ((filter_params->input_fps == 24) && (ctx->cyclic_cnt == 5))
            ctx->cyclic_cnt = 0;
        filter_params->output_frames = (VASurfaceID*)&output_surface[i];
        filter_params_addr = NULL;
        vas = vaUnmapBuffer(ctx->hwctx->display, ctx->filter_buffer[i]);
        if (vas != VA_STATUS_SUCCESS)
            av_log(avctx, AV_LOG_ERROR, "Failed to unmap filter parameter "
                   "buffer: %d (%s).\n", vas, vaErrorStr(vas));

        //apply the out param and decide copy witch input picture for each output picture
        vas = vaBeginPicture(ctx->hwctx->display,
                             ctx->va_context, output_surface[i]);

        if (vas != VA_STATUS_SUCCESS) {
            av_log(ctx, AV_LOG_ERROR, "Failed to attach new picture: "
                   "%d (%s).\n", vas, vaErrorStr(vas));
            err = AVERROR(EIO);
            goto fail;
        }

        if ((i < 3 && IS_24FPS) || (!IS_24FPS)) {
            in_buffer = ctx->pipeline_buffer[0];
            vas = vaRenderPicture(ctx->hwctx->display, ctx->va_context,
                                  &ctx->pipeline_buffer[0], 1);
        }
        else {
            in_buffer = ctx->pipeline_buffer[1];
            vas = vaRenderPicture(ctx->hwctx->display, ctx->va_context,
                                  &ctx->pipeline_buffer[1], 1);
        }
        if (vas != VA_STATUS_SUCCESS) {
            av_log(ctx, AV_LOG_ERROR, "Failed to render parameter buffer: "
                   "%d (%s).\n", vas, vaErrorStr(vas));
                err = AVERROR(EIO);
                goto fail_after_begin;
        }

        vas = vaEndPicture(ctx->hwctx->display, ctx->va_context);
        if (vas != VA_STATUS_SUCCESS) {
            av_log(ctx, AV_LOG_ERROR, "Failed to start picture processing: "
                   "%d (%s).\n", vas, vaErrorStr(vas));
            err = AVERROR(EIO);
            goto fail_after_render;
        }

        //set the output frame pts. In 24 fps mode , the front
        if (ctx->first_pts == AV_NOPTS_VALUE && input_frame->pts != AV_NOPTS_VALUE)
            ctx->first_pts = input_frame->pts;
        if (i < 3 && IS_24FPS)
            av_frame_copy_props(ctx->output_frame[i], ctx->first_frame);
        else
            av_frame_copy_props(ctx->output_frame[i], input_frame);
        ctx->output_frame[i]->pts = av_rescale_q(ctx->first_pts,
            inlink->time_base, outlink->time_base) + ctx->frame_out;
        if (ff_filter_frame(outlink, ctx->output_frame[i])){
            av_frame_free(&ctx->output_frame[i]);
            av_log(avctx, AV_LOG_ERROR, "Failed to filter frame "
                   "parameter buffer: %d (%s).\n", vas, vaErrorStr(vas));
        }
        ctx->frame_out++;
    }
    for (int i = 0; i < MAX_IN_BUFFER; i++)
        if (ctx->pipeline_buffer[i] != VA_INVALID_ID || (ctx->hwctx->driver_quirks &
        AV_VAAPI_DRIVER_QUIRK_RENDER_PARAM_BUFFERS)) {
            vaDestroyBuffer(ctx->hwctx->display, ctx->pipeline_buffer[i]);
            ctx->pipeline_buffer[i] = VA_INVALID_ID;
        }

    av_frame_free(&input_frame);
    if (ctx->first_frame)
        av_frame_free(&ctx->first_frame);

    ctx->out_finished = 0;
    return 0;

    // We want to make sure that if vaBeginPicture has been called, we also
    // call vaRenderPicture and vaEndPicture.  These calls may well fail or
    // do something else nasty, but once we're in this failure case there
    // isn't much else we can do.
fail_after_begin:
    vaRenderPicture(ctx->hwctx->display, ctx->va_context, &in_buffer, 1);
fail_after_render:
    vaEndPicture(ctx->hwctx->display, ctx->va_context);
fail:
    if (filter_params_addr)
        vaUnmapBuffer(ctx->hwctx->display, out_buffer);
    av_frame_free(&input_frame);
    av_frame_free(&failed_frame);
    return err;
}

static av_cold int frc_vaapi_init(AVFilterContext *avctx)
{
    FrcVAAPIContext *ctx = avctx->priv;

    ctx->va_config  = VA_INVALID_ID;
    ctx->va_context = VA_INVALID_ID;
    for (int i = 0; i < MAX_OUT_SURFACE; i++) {
        ctx->filter_buffer[i] = VA_INVALID_ID;
    }
    for (int i = 0; i < MAX_IN_BUFFER; i++) {
        ctx->pipeline_buffer[i] = VA_INVALID_ID;
    }
    ctx->valid_ids  = 1;

    ctx->output_format = AV_PIX_FMT_NONE;
    ctx->frame_out = 0;
    ctx->out_finished = 0;
    ctx->first_frame = NULL;
    ctx->first_pts = AV_NOPTS_VALUE;
    return 0;
}

static av_cold void frc_vaapi_uninit(AVFilterContext *avctx)
{
    FrcVAAPIContext *ctx = avctx->priv;

    if (ctx->valid_ids)
        frc_vaapi_pipeline_uninit(ctx);

    if (ctx->first_frame)
        av_frame_free(&ctx->first_frame);

    av_buffer_unref(&ctx->input_frames_ref);
    av_buffer_unref(&ctx->output_frames_ref);
    av_buffer_unref(&ctx->device_ref);
}


#define OFFSET(x) offsetof(FrcVAAPIContext, x)
#define FLAGS (AV_OPT_FLAG_VIDEO_PARAM)
static const AVOption frc_vaapi_options[] = {
    { NULL },
};

static const AVClass frc_vaapi_class = {
    .class_name = "frc_vaapi",
    .item_name  = av_default_item_name,
    .option     = frc_vaapi_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

static const AVFilterPad frc_vaapi_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = &frc_vaapi_filter_frame,
        .config_props = &frc_vaapi_config_input,
    },
    { NULL }
};

static const AVFilterPad frc_vaapi_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
        .config_props = &frc_vaapi_config_output,
    },
    { NULL }
};

AVFilter ff_vf_frc_vaapi = {
    .name          = "frc_vaapi",
    .description   = NULL_IF_CONFIG_SMALL("Frame rate conversion for 60fps to/from VAAPI surfaces."),
    .priv_size     = sizeof(FrcVAAPIContext),
    .init          = &frc_vaapi_init,
    .uninit        = &frc_vaapi_uninit,
    .query_formats = &frc_vaapi_query_formats,
    .inputs        = frc_vaapi_inputs,
    .outputs       = frc_vaapi_outputs,
    .priv_class    = &frc_vaapi_class,
};
