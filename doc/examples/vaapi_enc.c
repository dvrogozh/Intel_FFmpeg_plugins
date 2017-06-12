/*
 * Video Acceleration API (video encoding) encode sample
 *
 * You can compile it like "gcc -o vaapi_enc \
 * vaapi_enc.c -lavformat -lavcodec -lavutil -lva", and you
 * can execute this sample code using command like "./vaapi_enc
 * inputfile width height outputfile"
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
#include <stdio.h>
#include <string.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavfilter/avfiltergraph.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavutil/pixdesc.h>
#include <libavutil/hwcontext.h>
#include <libavutil/hwcontext_vaapi.h>
#include <va/va.h>
#include <va/va_x11.h>
#include <va/va_drm.h>

#define DEFAULT_SURFACES 20
typedef struct FilteringContext {
        AVFilterContext *buffersink_ctx;
        AVFilterContext *buffersrc_ctx;
        AVFilterGraph *filter_graph;
} FilteringContext;
int width = 1920, height = 1080;
AVBufferRef *hw_frames_ctx = NULL;
AVBufferRef *hw_device_ctx = NULL;
int frame_cnt = 0;

int set_frames_ctx()
{
    const char *dev_name = "/dev/dri/renderD128";
    AVHWFramesContext *frames;
    int err;
    AVBufferRef *frames_ref;

    err = av_hwdevice_ctx_create(&hw_device_ctx,
          AV_HWDEVICE_TYPE_VAAPI, dev_name, NULL, 0);
    if (err < 0) {
        fprintf(stderr, "Failed to create a VAAPI device.\n");
        return err;
    }
    if (!(frames_ref = av_hwframe_ctx_alloc(hw_device_ctx))) {
        fprintf(stderr, "Failed to create VAAPI frame context.\n");
        return -1;
    }
    frames = (AVHWFramesContext *)frames_ref->data;
    frames->format    = AV_PIX_FMT_VAAPI;
    frames->sw_format = AV_PIX_FMT_NV12;
    frames->width     = width;
    frames->height    = height;
    frames->initial_pool_size = DEFAULT_SURFACES;
    if ((err = av_hwframe_ctx_init(frames_ref)) < 0) {
        fprintf(stderr, "Failed to initialize VAAPI frame context.\n");
        return err;
    }
    hw_frames_ctx = frames_ref;
    return 0;
}

int open_codec_context(const char *str_output_file, AVCodecContext **avctx)
{
    AVCodec *codec;
    const char *enc_name = "h264_vaapi";
    int ret;
    AVStream *out_stream;
    AVFormatContext *out_fmt_ctx = NULL;

    avformat_alloc_output_context2(&out_fmt_ctx, NULL, NULL, str_output_file);
    if (!out_fmt_ctx) {
        fprintf(stderr, "Could not create output context.\n");
        return -1;
    }

    if (!(codec = avcodec_find_encoder_by_name(enc_name))) {
        fprintf(stderr, "Could not find encoder.\n");
        ret = AVERROR(ENOMEM);
        goto fail;
    }

    if (!(out_stream = avformat_new_stream(out_fmt_ctx, codec))) {
        fprintf(stderr, "Failed allocating output stream.\n");
        ret = -1;
        goto fail;
    }

    if (!(*avctx = avcodec_alloc_context3(codec))) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }
    if (avcodec_parameters_to_context(*avctx, out_stream->codecpar) < 0) {
        fprintf(stderr, "Couldn't copy codec context\n");
        ret = -1;
        goto fail;
    }

    (*avctx)->width = width;
    (*avctx)->height = height;
    (*avctx)->time_base.num = 1;
    (*avctx)->time_base.den = 25;
    (*avctx)->gop_size = 10;
    (*avctx)->pix_fmt = codec->pix_fmts[0];
    (*avctx)->slices = 1;
    if ((ret = set_frames_ctx()) < 0) {
        fprintf(stderr, "Failed to initialize VAAPI frame context.\n");
        goto fail;
    } else {
        (*avctx)->hw_frames_ctx = hw_frames_ctx;
    }

    if ((ret = avcodec_open2(*avctx, codec, NULL)) < 0) {
        fprintf(stderr, "Cannot open video encoder codec.\n");
        goto fail;
    }
fail:
    avformat_close_input(&out_fmt_ctx);
    return ret;
}

int init_filter(FilteringContext **filter_ctx, char *args)
{
    char filter_spec[] = "format=nv12|vaapi,hwupload";
    int  ret = 0, i = 0;
    AVFilter *buffersrc, *buffersink;
    AVFilterContext *buffersrc_ctx, *buffersink_ctx;
    AVFilterInOut *outputs = avfilter_inout_alloc();
    AVFilterInOut *inputs  = avfilter_inout_alloc();
    AVFilterGraph *filter_graph = avfilter_graph_alloc();

    buffersrc = avfilter_get_by_name("buffer");
    buffersink = avfilter_get_by_name("buffersink");
    if (!buffersrc || !buffersink) {
        av_log(NULL, AV_LOG_ERROR, "filtering source or sink element not found\n");
        ret = AVERROR_UNKNOWN;
        goto fail;
    }
    ret = avfilter_graph_create_filter(&buffersrc_ctx, buffersrc, "in",
                                       args, NULL, filter_graph);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot create buffer source\n");
        goto fail;
    }
    ret = avfilter_graph_create_filter(&buffersink_ctx, buffersink, "out",
                                       NULL, NULL, filter_graph);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot create buffer sink.\n");
        goto fail;
    }
    outputs->name       = av_strdup("in");
    outputs->filter_ctx = buffersrc_ctx;
    outputs->pad_idx    = 0;
    outputs->next       = NULL;
    inputs->name        = av_strdup("out");
    inputs->filter_ctx  = buffersink_ctx;
    inputs->pad_idx     = 0;
    inputs->next        = NULL;
    if (!outputs->name || !inputs->name) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }

    if ((ret = avfilter_graph_parse_ptr(filter_graph, filter_spec,
               &inputs, &outputs, NULL)) < 0)
        goto fail;
    if (hw_device_ctx) {
        for (i = 0; i < filter_graph->nb_filters; i++) {
            filter_graph->filters[i]->hw_device_ctx = av_buffer_ref(hw_device_ctx);
        }
    }

    if ((ret = avfilter_graph_config(filter_graph, NULL)) < 0)
        goto fail;

    (*filter_ctx)->buffersrc_ctx = buffersrc_ctx;
    (*filter_ctx)->buffersink_ctx = buffersink_ctx;
    (*filter_ctx)->filter_graph = filter_graph;
fail:
    if (inputs)
        avfilter_inout_free(&inputs);
    if (outputs)
        avfilter_inout_free(&outputs);
    return ret;
}

int encode_write(AVCodecContext *avctx, AVFrame *filt_frame, FILE **fout)
{
    int ret = 0;
    AVPacket enc_pkt;

    av_init_packet(&enc_pkt);
    enc_pkt.data = NULL;
    enc_pkt.size = 0;
    
    /* replace avcodec_encode_video2 function with new API*/
    if ((ret = avcodec_send_frame(avctx, filt_frame)) < 0)
        goto end;
    while (1) {
        ret = avcodec_receive_packet(avctx, &enc_pkt);
        if (!ret) {
            enc_pkt.stream_index = 0;
            ret = fwrite(enc_pkt.data, enc_pkt.size, 1, *fout);
        } else {
            break;
        }
    }
end:
    av_packet_unref(&enc_pkt);
    ret = ((ret==AVERROR(EAGAIN)) ? 0:-1);
    return ret;
}

int main(int argc, char *argv[])
{
    int ret, size;
    FILE *fin, *fout;
    AVFrame *src_frame, *filt_frame;
    AVCodecContext *avctx = NULL;
    FilteringContext *filter_ctx;
    uint8_t *frame_buf;
    const char *str_input_file = "test.yuv";
    const char *str_output_file = "out.h264";
    char args[512];

    size = width * height;
    frame_buf = malloc((size * 3) / 2); /* size for nv12 */
    fin = fopen(str_input_file, "r");
    fout = fopen(str_output_file, "w+b");
    if (!fin || !fout) {
        fprintf(stderr, "Fail to open input or output file.\n");
        ret = -1;
        goto close;
    }

    av_register_all();
    avfilter_register_all();

    if ((ret = open_codec_context(str_output_file, &avctx)) < 0) { /*Init the AVCodecContext for encoding*/
        fprintf(stderr, "Failed to open codec context.\n");
        goto close;
    }

    snprintf(args, sizeof(args),
            "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=%d/%d:frame_rate=%d/%d",
            avctx->width, avctx->height, AV_PIX_FMT_NV12,
            avctx->time_base.num, avctx->time_base.den,
            avctx->sample_aspect_ratio.num, avctx->sample_aspect_ratio.den,
            avctx->framerate.num, avctx->framerate.den);

    /*Bound HW to upload and pull data by building a filtergraph*/
    if (!(filter_ctx = (FilteringContext *)av_malloc(sizeof(*filter_ctx)))) {
        ret = AVERROR(ENOMEM);
        goto close;
    }
    filter_ctx->buffersrc_ctx  = NULL;
    filter_ctx->buffersink_ctx = NULL;
    filter_ctx->filter_graph   = NULL;
    if ((ret = init_filter(&filter_ctx, args)) < 0) {
        fprintf(stderr, "Failed to initialize the filtering context.\n");
        goto close;
    }

    while (fread(frame_buf, size*3/2, 1, fin) > 0) {
        if (!(src_frame = av_frame_alloc())) {
            ret = AVERROR(ENOMEM);
            goto close;
        }
        src_frame->data[0] = frame_buf;
        src_frame->data[1] = frame_buf + size;
        src_frame->linesize[0] = width;
        src_frame->linesize[1] = width;
        src_frame->width = width;
        src_frame->height = height;
        src_frame->format = AV_PIX_FMT_NV12;
        /* push the source frame into the filtergraph */
        ret = av_buffersrc_add_frame_flags(filter_ctx->buffersrc_ctx,
                                           src_frame, 0);
        if (ret < 0) {
            fprintf(stderr, "Error while feeding the filtergraph.\n");
            goto close;
        }
        /* pull filtered frames from the filtergraph */
        while (1) {
            if (!(filt_frame = av_frame_alloc())) {
                ret = AVERROR(ENOMEM);
                goto close;
            }
            if ((ret = (av_buffersink_get_frame(filter_ctx->buffersink_ctx, filt_frame))) < 0) {
                /* if no more frames for output - returns AVERROR(EAGAIN)
                 * if flushed and no more frames for output - returns AVERROR_EOF
                 * rewrite retcode to 0 to show it as normal procedure completion
                 */
                if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF)
                    ret = 0;
                av_frame_free(&filt_frame);
                break;
            }
            frame_cnt++;
            if (frame_cnt % 10 == 1) {
                filt_frame->pict_type = AV_PICTURE_TYPE_I;
            } else {
                filt_frame->pict_type = AV_PICTURE_TYPE_B;
            }

            if ((ret = (encode_write(avctx, filt_frame, &fout))) < 0) {
                fprintf(stderr, "Failed to encode.\n");
                goto close;
            }
            av_frame_free(&filt_frame);
        }
        av_frame_free(&src_frame);
    }
    while (1) {        /*Flush encode*/
        if ((ret = encode_write(avctx, NULL, &fout)) < 0)
            break;
    }
close:
    fclose(fin);
    fclose(fout);
    av_frame_free(&src_frame);
    av_frame_free(&filt_frame);
    avcodec_free_context(&avctx);
    if (filter_ctx) {
        avfilter_free(filter_ctx->buffersrc_ctx);
        avfilter_free(filter_ctx->buffersink_ctx);
        avfilter_graph_free(&(filter_ctx->filter_graph));
        av_free(filter_ctx);
        filter_ctx = NULL;
    }
    av_buffer_unref(&hw_device_ctx);
    av_buffer_unref(&hw_frames_ctx);
    if (frame_buf) {
        free(frame_buf);
        frame_buf = NULL;
    }
    return ret;
}
