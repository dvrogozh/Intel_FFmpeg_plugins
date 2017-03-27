/*
 * Video Acceleration API (video decoding) decode sample
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

/**
 * @file
 * Intel VAAPI-accelerated decoding example.
 *
 * @example vaapi_dec.c
 * This example shows how to do VAAPI-accelerated decoding with output
 * frames from the VAAPI video surfaces.
 */

#include <stdio.h>
#include <libavcodec/avcodec.h>
#include <libavcodec/vaapi.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>
#include <libavutil/pixdesc.h>
#include <libavutil/hwcontext.h>
#include <libavutil/opt.h>
#include <libavutil/hwcontext_vaapi.h>
#include <libavutil/avassert.h>
#include <va/va.h>
#include <va/va_x11.h>
#include <va/va_drm.h>

#define DEFAULT_SURFACES 20
static enum AVPixelFormat decode_format = AV_PIX_FMT_NV12;
static enum AVPixelFormat output_format = AV_PIX_FMT_NV12; /* default output format nv12 */
static AVBufferRef *frames_ref = NULL;
static AVBufferRef *device_ref = NULL;
FILE *output_file = NULL;

void decoder_uninit_vaapi(AVCodecContext *avctx, AVVAAPIDeviceContext *hwctx)
{
    if (hwctx) {
        av_buffer_unref(&frames_ref);
        av_buffer_unref(&device_ref);
    }
    return;
}

int decoder_init_vaapi(AVCodecContext *ctx)
{
    AVBufferRef *hw_device_ctx = NULL;
    AVHWFramesContext *frames;
    int err = 0;
    char *dev_name = "/dev/dri/renderD128";
    AVBufferRef *device_ref = NULL;
    AVHWDeviceContext *device = NULL;
    AVVAAPIDeviceContext *hwctx = NULL;

    if ((err = av_hwdevice_ctx_create(&hw_device_ctx, AV_HWDEVICE_TYPE_VAAPI,
                                      dev_name, NULL, 0)) < 0) {
        fprintf(stderr, "Failed to create a VAAPI device.\n");
        return err;
    }

    device_ref = av_buffer_ref(hw_device_ctx);
    device = (AVHWDeviceContext *)device_ref->data;
    hwctx = (AVVAAPIDeviceContext *)device->hwctx;

    ctx->pix_fmt = output_format;
    if (!(frames_ref = av_hwframe_ctx_alloc(device_ref))) {
        fprintf(stderr, "Failed to create VAAPI frame context.\n");
        err = AVERROR(ENOMEM);
        goto fail;
    }
    frames = (AVHWFramesContext *)frames_ref->data;
    frames->format    = AV_PIX_FMT_VAAPI;
    frames->sw_format = decode_format;
    frames->width     = ctx->coded_width;
    frames->height    = ctx->coded_height;
    frames->initial_pool_size = DEFAULT_SURFACES;
    if ((err = av_hwframe_ctx_init(frames_ref)) < 0) {
        fprintf(stderr, "Failed to initialize VAAPI frame.\n");
        goto fail;
    }

    ctx->hw_frames_ctx = av_buffer_ref(frames_ref);
    if (!ctx->hw_frames_ctx) {
        err = AVERROR(ENOMEM);
        fprintf(stderr, "Failed to ref VAAPI frame.\n");
        goto fail;
    }

fail:
    if (err < 0)
        decoder_uninit_vaapi(ctx, hwctx);
    return err;
}

static enum AVPixelFormat get_hw_vaapi_format(AVCodecContext *ctx,
                                              const enum AVPixelFormat *pix_fmts)
{
    const enum AVPixelFormat *p;
    int ret;

    for (p = pix_fmts; *p != -1; p++) {
        const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(*p);
        if (!(desc->flags & AV_PIX_FMT_FLAG_HWACCEL))
            break;

        if (!(strstr(desc->name, "vaapi")))
            continue;

        if ((ret = decoder_init_vaapi(ctx)) < 0) {
            fprintf(stderr, "hwaccel for decoder(VAAPI) cannot be initialized.\n");
            return AV_PIX_FMT_NONE;
        }
        break;
    }
    return *p;
}

static int get_hw_vaapi_buffer(AVCodecContext *ctx, AVFrame *frame, int flags)
{
    int err;
    if ((err = av_hwframe_get_buffer(frames_ref, frame, 0)) < 0)
        av_log(NULL, AV_LOG_ERROR, "Failed to allocate decoder surface.\n");

    return err;
}

int retrieve_data(AVFrame *input)
{
    AVFrame *output = 0;
    int err;
    av_assert0(input->format == AV_PIX_FMT_VAAPI);

    if (output_format == AV_PIX_FMT_VAAPI)
        return 0;

    if (!(output = av_frame_alloc()))
        return AVERROR(ENOMEM);
    output->format = output_format;
    if ((err = av_hwframe_transfer_data(output, input, 0)) < 0) {
        fprintf(stderr, "Failed to transfer data to output frame: %d.\n", err);
        goto fail;
    }

    if ((err = av_frame_copy_props(output, input)) < 0) {
        av_frame_unref(output);
        goto fail;
    }

    av_frame_unref(input);
    av_frame_move_ref(input, output);
    av_frame_free(&output);
    return 0;

fail:
    av_frame_free(&output);
    return err;
}

int write_frame(AVFrame *frame)
{
    int idx, size;
    int width = frame->width;
    int height = frame->height;

    av_assert0(frame && frame->data[0] && output_file);

    for (idx = 0; idx < height; idx++) {
        if ((size = fwrite(frame->data[0] + idx*frame->linesize[0],
                           1, width, output_file)) < 0) {
                fprintf(stderr, "Dump Y to file error.\n");
                return -1;
        }
    }

    height >>= 1;
    for (idx = 0; idx < height; idx++) {
        if ((size = fwrite(frame->data[1] + idx*frame->linesize[1],
                           1, width, output_file)) < 0) {
            fprintf(stderr, "Dump UV to file error.\n");
            return -1;
        }
    }

    return 0;
}

int decode_write(AVCodecContext *avctx, AVPacket packet, int flush)
{
    AVFrame *frame = NULL;
    int ret = 0, got_frame = 0;

    if (!(frame = av_frame_alloc()))
        return AVERROR(ENOMEM);

    if ((ret = avcodec_decode_video2(avctx, frame, &got_frame, &packet)) < 0) {
        fprintf(stderr, "Error during decoding\n");
        goto fail;
    }

    if (got_frame) {
        if (AV_PIX_FMT_VAAPI == frame->format) {
            /* retrieve data from GPU to CPU */
            if ((ret = retrieve_data(frame)) < 0)
                goto fail;

            if ((ret = write_frame(frame)) < 0)
                goto fail;
        }
    } else if (flush == 1) {
            ret = -1;
    }

fail:
    av_frame_free(&frame);
    return ret;
}

/* flush the decoder */
int flush(AVCodecContext *avctx)
{
    AVPacket packet;
    int ret = 0;

    av_init_packet(&packet);
    packet.data = NULL;
    packet.size = 0;

    while (1) {
        if ((ret = decode_write(avctx, packet, 1)) < 0)
            break;
    }
    av_packet_unref(&packet);

    return 0;
}

int main(int argc, char *argv[])
{
    AVFormatContext *input_ctx = NULL;
    int i,video_stream,ret;
    AVCodecContext *decoder_ctx = NULL;
    AVCodec *decoder = NULL;
    AVPacket packet;

    if (argc < 3) {
        fprintf(stderr, "Usage: %s <input file> <output file>\n", argv[0]);
        return -1;
    }

    av_register_all();

    /* open the input file */
    if (avformat_open_input(&input_ctx, argv[1], NULL, NULL) != 0) {
        fprintf(stderr, "Cannot open input file '%s'\n", argv[1]);
        return -1;
    }

    if (avformat_find_stream_info(input_ctx, NULL) < 0) {
        fprintf(stderr, "Couldn't find input stream information.\n");
        return -1;
    }

    /* find the video stream information */
    video_stream = -1;
    for (i = 0; i < input_ctx->nb_streams; i++) {
        AVStream *st = input_ctx->streams[i];
        if (st->codecpar->codec_type == AVMEDIA_TYPE_VIDEO) {
            /* NOTE: just handle the first video stream */
            video_stream = i;
            break;
        }
    }

    if (video_stream == -1) {
        fprintf(stderr, "No find a video stream in the input file.\n");
        return -1;
    }

    /* initialize the video decoder */
    if (!(decoder = avcodec_find_decoder(input_ctx->streams[video_stream]->codecpar->codec_id))) {
        fprintf(stderr, "Could not find input codec\n");
        avformat_close_input(&input_ctx);
        return -1;
    }

    if (!(decoder_ctx = avcodec_alloc_context3(decoder)))
        return AVERROR(ENOMEM);

    if (avcodec_parameters_to_context(decoder_ctx,
                                      input_ctx->streams[video_stream]->codecpar) < 0) {
        avcodec_free_context(&decoder_ctx);
        avformat_close_input(&input_ctx);
        return -1;
    }

    decoder_ctx->get_format  = get_hw_vaapi_format;
    decoder_ctx->get_buffer2 = get_hw_vaapi_buffer;
    av_opt_set_int(decoder_ctx, "refcounted_frames", 1, 0);

    if ((ret = avcodec_open2(decoder_ctx, decoder, NULL)) < 0) {
        fprintf(stderr, "Failed to open codec for stream #%u\n", i);
        return ret;
    }

    /* open the output file to dump YUV */
    output_file = fopen(argv[2], "w+");

    /* actual decoding and dump the YUV data */
    while (av_read_frame(input_ctx, &packet) >= 0) {
        if (video_stream == packet.stream_index)
                ret = decode_write(decoder_ctx, packet, 0);
        av_packet_unref(&packet);

        if (ret < 0)
                break;
    }

    /* flush the decoder */
    flush(decoder_ctx);

    if (output_file)
        fclose(output_file);
    avcodec_free_context(&decoder_ctx);
    avformat_close_input(&input_ctx);

    av_buffer_unref(&frames_ref);
    av_buffer_unref(&device_ref);

    return ret;
}

