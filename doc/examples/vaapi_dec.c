/*
 * Video Acceleration API (video decoding) decode sample
 *
 * You can compile it like "gcc -I/usr/lib64 -o vaapi_dec \
 * vaapi_dec.c -lavformat -lavcodec -lavutil -lva"
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

/*compatibility with newer API*/
#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(55,28,1)
#define av_frame_alloc avcodec_alloc_frame
#define av_frame_free avcodec_free_frame
#endif

#define DEFAULT_SURFACES 20
static VAConfigID   va_config = VA_INVALID_ID;
static VAContextID  va_context = VA_INVALID_ID;
static enum AVPixelFormat decode_format = AV_PIX_FMT_NONE;
static enum AVPixelFormat output_format = AV_PIX_FMT_NV12;
static AVBufferRef *frm_ref = NULL;
static AVBufferRef *device_ref = NULL;
static AVBufferRef *hw_frames_ctx = NULL;
FILE* outputFile;

static const struct {
    enum AVCodecID codec_id;
    int codec_profile;
    VAProfile va_profile;
} vaapi_profile_map[] = {
#define MAP(c, p, v) { AV_CODEC_ID_ ## c, FF_PROFILE_ ## p, VAProfile ## v }
    MAP(MPEG2VIDEO,  MPEG2_SIMPLE,    MPEG2Simple ),
    MAP(MPEG2VIDEO,  MPEG2_MAIN,      MPEG2Main   ),
    MAP(H263,        UNKNOWN,         H263Baseline),
    MAP(MPEG4,       MPEG4_SIMPLE,    MPEG4Simple ),
    MAP(MPEG4,       MPEG4_ADVANCED_SIMPLE,
                               MPEG4AdvancedSimple),
    MAP(MPEG4,       MPEG4_MAIN,      MPEG4Main   ),
    MAP(H264,        H264_CONSTRAINED_BASELINE,
                           H264ConstrainedBaseline),
    MAP(H264,        H264_BASELINE,   H264Baseline),
    MAP(H264,        H264_MAIN,       H264Main    ),
    MAP(H264,        H264_HIGH,       H264High    ),
#if VA_CHECK_VERSION(0, 37, 0)
    MAP(HEVC,        HEVC_MAIN,       HEVCMain    ),
#endif
    MAP(WMV3,        VC1_SIMPLE,      VC1Simple   ),
    MAP(WMV3,        VC1_MAIN,        VC1Main     ),
    MAP(WMV3,        VC1_COMPLEX,     VC1Advanced ),
    MAP(WMV3,        VC1_ADVANCED,    VC1Advanced ),
    MAP(VC1,         VC1_SIMPLE,      VC1Simple   ),
    MAP(VC1,         VC1_MAIN,        VC1Main     ),
    MAP(VC1,         VC1_COMPLEX,     VC1Advanced ),
    MAP(VC1,         VC1_ADVANCED,    VC1Advanced ),
#if VA_CHECK_VERSION(0, 35, 0)
    MAP(VP8,         UNKNOWN,       VP8Version0_3 ),
#endif
#if VA_CHECK_VERSION(0, 37, 1)
    MAP(VP9,         VP9_0,           VP9Profile0 ),
#endif
#undef MAP
};

int build_decoder_config(AVCodecContext *avctx,AVVAAPIDeviceContext *hwctx,AVBufferRef *dev_ref)
{
    AVVAAPIHWConfig *hwconfig = NULL;
    AVHWFramesConstraints *constraints = NULL;
    VAStatus vas;
    int err, i, j;
    const AVCodecDescriptor *codec_desc;
    const AVPixFmtDescriptor *pix_desc;
    enum AVPixelFormat pix_fmt;
    VAProfile profile, *profile_list = NULL;
    VAEntrypoint va_entrypoint;
    int profile_count, exact_match, alt_profile;
    int decode_width, decode_height;
    enum AVPixelFormat output_format;

    codec_desc = avcodec_descriptor_get(avctx->codec_id);
    if (!codec_desc) {
        err = AVERROR(EINVAL);
        goto fail;
    }
    profile_count = vaMaxNumProfiles(hwctx->display);
    profile_list = (VAProfile*)av_malloc(profile_count * sizeof(VAProfile));
    if (!profile_list) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    vas = vaQueryConfigProfiles(hwctx->display,profile_list, &profile_count);
    if (vas != VA_STATUS_SUCCESS) {
        printf("Failed to query profiles: %d (%s).\n",vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail;
    }
    profile = VAProfileNone;
    exact_match = 0;
    for (i = 0; i < FF_ARRAY_ELEMS(vaapi_profile_map); i++) {
        int profile_match = 0;
        if (avctx->codec_id != vaapi_profile_map[i].codec_id)
            continue;
        if (avctx->profile == vaapi_profile_map[i].codec_profile)
            profile_match = 1;
        profile = vaapi_profile_map[i].va_profile;
        for (j = 0; j < profile_count; j++) {
            if (profile == profile_list[j]) {
                exact_match = profile_match;
                break;
            }
        }
        if (j < profile_count) {
            if (exact_match)
                break;
            alt_profile = vaapi_profile_map[i].codec_profile;
        }
    }
    av_freep(&profile_list);
    if (profile == VAProfileNone) {
        printf("No VAAPI support for codec %s.\n",codec_desc->name);
        err = AVERROR(ENOSYS);
        goto fail;
    }
    if (!exact_match) {
        printf("No VAAPI support for codec %s "
               "profile %d: trying instead with profile %d.\n",
               codec_desc->name, avctx->profile, alt_profile);
    }
    va_entrypoint = VAEntrypointVLD;
    vas = vaCreateConfig(hwctx->display, profile,va_entrypoint, 0, 0, &va_config);
    if (vas != VA_STATUS_SUCCESS) {
        printf("Failed to create decode pipeline "
               "configuration: %d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail;
    }
    hwconfig = (AVVAAPIHWConfig*)av_hwdevice_hwconfig_alloc(dev_ref);
    if (!hwconfig) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    hwconfig->config_id = va_config;
    constraints = av_hwdevice_get_hwframe_constraints(dev_ref,hwconfig);
    if (!constraints)
        goto fail;
    /* Decide on the decoder target format.*/
    if (output_format != AV_PIX_FMT_NONE &&
        output_format != AV_PIX_FMT_VAAPI) {
        for (i = 0; constraints->valid_sw_formats[i] != AV_PIX_FMT_NONE; i++) {
            if (constraints->valid_sw_formats[i] == output_format) {
                decode_format = output_format;
                printf("Using decode format %s (output "
                       "format).\n", av_get_pix_fmt_name(decode_format));
                break;
            }
        }
    }
    /* Otherwise, we would like to try to choose something which matches the
       decoder output, but there isn't enough information available here to
       do so.  Assume for now that we are always dealing with YUV 4:2:0, so
       pick a format which does that.*/
    if (decode_format == AV_PIX_FMT_NONE) {
        for (i = 0; constraints->valid_sw_formats[i] != AV_PIX_FMT_NONE; i++) {
            pix_fmt  = constraints->valid_sw_formats[i];
            pix_desc = av_pix_fmt_desc_get(pix_fmt);
            if (pix_desc->nb_components == 3 &&
                pix_desc->log2_chroma_w == 1 &&
                pix_desc->log2_chroma_h == 1) {
                decode_format = pix_fmt;
                printf("Using decode format %s (format "
                       "matched).\n", av_get_pix_fmt_name(decode_format));
                break;
            }
        }
    }
    /* Otherwise pick the first in the list and hope for the best.*/
    if (decode_format == AV_PIX_FMT_NONE) {
        decode_format = constraints->valid_sw_formats[0];
        printf("Using decode format %s (first in list).\n",
               av_get_pix_fmt_name(decode_format));
        if (i > 1) {
            printf("Using randomly chosen decode "
                   "format %s.\n", av_get_pix_fmt_name(decode_format));
        }
    }
    /* Ensure the picture size is supported by the hardware.*/
    decode_width  = avctx->coded_width;
    decode_height = avctx->coded_height;
    if (decode_width  < constraints->min_width  ||
        decode_height < constraints->min_height ||
        decode_width  > constraints->max_width  ||
        decode_height >constraints->max_height) {
        printf("VAAPI hardware does not support image.\n ");
        err = AVERROR(EINVAL);
        goto fail;
    }
    av_hwframe_constraints_free(&constraints);
    av_freep(&hwconfig);
    return 0;
fail:
    av_hwframe_constraints_free(&constraints);
    av_freep(&hwconfig);
    vaDestroyConfig(hwctx->display, va_config);
    av_freep(&profile_list);
    return err;
}

void decode_uninit_vaapi(AVCodecContext *avctx, AVVAAPIDeviceContext *hwctx)
{
    if (hwctx) {
        if (va_context != VA_INVALID_ID) {
            vaDestroyContext(hwctx->display, va_context);
            va_context = VA_INVALID_ID;
        }
        if (va_config != VA_INVALID_ID) {
            vaDestroyConfig(hwctx->display, va_config);
            va_config = VA_INVALID_ID;
        }
        av_buffer_unref(&frm_ref);
        av_buffer_unref(&device_ref);
    }
    av_buffer_unref(&hw_frames_ctx);
    return;
}

int init_decoder_vaapi(AVCodecContext *ctx)
{
    AVBufferRef *hw_device_ctx = NULL;
    AVHWFramesContext *frames;
    int err = 0;
    char *dev_name = "/dev/dri/renderD128";
    AVBufferRef *device_ref = NULL;
    AVHWDeviceContext *device = NULL;
    AVVAAPIFramesContext *avfc = NULL;
    AVVAAPIDeviceContext *hwctx = NULL;
    VAStatus vas;
    struct vaapi_context decoder_vaapi_context;

    err = av_hwdevice_ctx_create(&hw_device_ctx, AV_HWDEVICE_TYPE_VAAPI,
                                 dev_name, NULL, 0);
    if (err < 0) {
        printf("Failed to create a VAAPI device.\n");
        return err;
    }
    device_ref = av_buffer_ref(hw_device_ctx);
    device = (AVHWDeviceContext *)device_ref->data;
    hwctx = (AVVAAPIDeviceContext *)device->hwctx;
    err = build_decoder_config(ctx,hwctx,device_ref);
    if (err < 0) {
        printf("No supported configuration for this codec.\n");
        goto fail;
    }
    ctx->pix_fmt = output_format;
    frm_ref = av_hwframe_ctx_alloc(device_ref);
    if (!frm_ref) {
        printf("Failed to create VAAPI frame context.\n");
        err = AVERROR(ENOMEM);
        goto fail;
    }
    frames = (AVHWFramesContext *)frm_ref->data;
    frames->format    = AV_PIX_FMT_VAAPI;
    frames->sw_format = decode_format;
    frames->width     = ctx->coded_width;
    frames->height    = ctx->coded_height;
    frames->initial_pool_size = DEFAULT_SURFACES;
    err = av_hwframe_ctx_init(frm_ref);
    if (err < 0) {
        printf("Failed to initialize VAAPI frame.\n");
        goto fail;
    }
    avfc = (AVVAAPIFramesContext*)frames->hwctx;
    vas = vaCreateContext(hwctx->display, va_config,
                          ctx->coded_width, ctx->coded_height,
                          VA_PROGRESSIVE,
                          avfc->surface_ids, avfc->nb_surfaces,
                          &va_context);
    if (vas != VA_STATUS_SUCCESS) {
        printf("Failed to create decode pipeline.\n");
        err = AVERROR(EINVAL);
        goto fail;
    }
    ctx->hw_frames_ctx = av_buffer_ref(frm_ref);
    if (!ctx->hw_frames_ctx) {
        err = AVERROR(ENOMEM);
        printf( "Failed to ref VAAPI frame.\n");
        goto fail;
    }
    decoder_vaapi_context.display    = hwctx->display;
    decoder_vaapi_context.config_id  = va_config;
    decoder_vaapi_context.context_id = va_context;
    ctx->hwaccel_context = &decoder_vaapi_context;
fail:
    if (err < 0)
        decode_uninit_vaapi(ctx,hwctx);
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
        ret = init_decoder_vaapi(ctx);
        if (ret < 0) {
            printf("hwaccel for decoder(VAAPI) cannot be initialized.\n");
            return AV_PIX_FMT_NONE;
        }
        if (hw_frames_ctx) {
            ctx->hw_frames_ctx = av_buffer_ref(hw_frames_ctx);
            if (!ctx->hw_frames_ctx)
                return AV_PIX_FMT_NONE;
        }
        break;
    }
    return *p;
}

static int get_hw_vaapi_buffer(AVCodecContext *ctx, AVFrame *frame, int flags)
{
    int err;
    err = av_hwframe_get_buffer(frm_ref, frame, 0);
    if (err < 0)
        av_log(NULL, AV_LOG_ERROR, "Failed to allocate decoder surface.\n");
    return err;
}

int retrieve_data(AVFrame *input)
{
    AVFrame *output = 0;
    int err;
    av_assert0(input->format == AV_PIX_FMT_VAAPI);
    if (output_format == AV_PIX_FMT_VAAPI)
        return 0;      /* Nothing to do. */
    output = av_frame_alloc();
    if (!output)
        return AVERROR(ENOMEM);
    output->format = output_format;
    err = av_hwframe_transfer_data(output, input, 0);
    if (err < 0) {
        printf("Failed to transfer data to output frame: %d.\n", err);
        goto fail;
    }
    err = av_frame_copy_props(output, input);
    if (err < 0) {
        av_frame_unref(output);
        goto fail;
    }
    av_frame_unref(input);
    av_frame_move_ref(input, output);
    av_frame_free(&output);
    return 0;
fail:
    if (output)
        av_frame_free(&output);
    return err;
}

int write_frame(AVFrame *frame)
{
    /* Default is NV12 */
    int idx, nWritten;
    int width = frame->width;
    int height = frame->height;
    if(NULL==frame || NULL==frame->data[0] || NULL==outputFile) {
        printf("Failed to write the docoded frame to outputFile.\n");
        return -1;
    }
    for (idx = 0; idx < height; idx++) {
        nWritten = fwrite(frame->data[0] + idx*frame->linesize[0], 1, width, outputFile);
        if(nWritten < 0) {
            printf("Write Y to file error.\n");
            return -1;
        }
    }
    height >>= 1;
    for (idx = 0; idx < height; idx++) {
        nWritten = fwrite(frame->data[1] + idx*frame->linesize[1], 1, width, outputFile);
        if(nWritten < 0) {
            printf("Write U to file error.\n");
            return -1;
        }
    }
    return 0;
}

int flush(AVFormatContext *mInputFormatContext,unsigned int stream_index)
{
    AVPacket packet;
    int ret = 0;
    int flushFlag = 1;
    enum AVMediaType type;
    AVCodecContext* avctx;

    avctx = mInputFormatContext->streams[stream_index]->codec;
    type = avctx->codec_type;
    av_init_packet(&packet);
    packet.data = NULL;
    packet.size = 0;
    while (1) {
        if (type == AVMEDIA_TYPE_VIDEO) {
            ret = decode_write(avctx, packet, flushFlag);
            if (ret < 0)
                break;
        }
    }
    av_free_packet(&packet);
    return 0;
}

int decode_write(AVCodecContext* pCodecCtx, AVPacket packet, int flushFlag)
{
    AVFrame *frame = NULL;
    int ret = 0, frameFinished = 0;
    frame = av_frame_alloc();
    if (!frame)
        return AVERROR(ENOMEM);
    ret = avcodec_decode_video2(pCodecCtx, frame, &frameFinished, &packet);
    if (ret < 0) {
        printf("Decoding failed\n");
        goto fail;
    }
    if (frameFinished) {
        if (AV_PIX_FMT_VAAPI == frame->format) {
            ret = retrieve_data(frame);
            if (ret < 0)
                goto fail;
            ret = write_frame(frame);   /*Default pix_fmt is NV12*/
            if (ret < 0)
                goto fail;
        }
    } else {
        if (flushFlag == 1)
            ret = -1;
    }
fail:
    av_frame_free(&frame);
    return ret;
}

int main(int argc, char *argv[])
{
    AVFormatContext *pFormatCtx = NULL;
    int i,videoStream,ret,frameFinished,strm_idx;
    AVCodecContext *pCodecCtx = NULL;
    AVCodec *pCodec = NULL;
    AVFrame *pFrame = NULL;
    AVPacket packet;
    char *strInputFile,*strOutputFile;

    if(argc != 3){
        printf("Please provide an input and output video file.\n");
        return -1;
    }
    strInputFile = argv[1];
    strOutputFile = argv[2];
    outputFile = fopen(strOutputFile, "w+");
    av_register_all();
    if (avformat_open_input(&pFormatCtx, strInputFile, NULL, NULL) != 0) {
        printf("Cannot open input file.\n");
        return -1;
    }
    if (avformat_find_stream_info(pFormatCtx, NULL)<0) {
        printf("Couldn't find input stream information.\n");
        return -1;
    }
    /* Find the video stream */
    videoStream = -1;
    for (i=0; i<pFormatCtx->nb_streams; i++) {
        if (pFormatCtx->streams[i]->codec->codec_type == AVMEDIA_TYPE_VIDEO){
            videoStream = i;
            pCodecCtx = pFormatCtx->streams[i]->codec;
            pCodecCtx->get_format = get_hw_vaapi_format;
            pCodecCtx->get_buffer2 = get_hw_vaapi_buffer;
            av_opt_set_int(pCodecCtx, "refcounted_frames", 1, 0);

            pCodec = avcodec_find_decoder(pCodecCtx->codec_id);
            ret = avcodec_open2(pCodecCtx, pCodec, NULL);
            if (ret < 0) {
                printf("Failed to open codec for stream #%u\n", i);
                return ret;
            }
            while (av_read_frame(pFormatCtx, &packet) >= 0) {
                strm_idx = packet.stream_index;
                if (pFormatCtx->streams[strm_idx]->codec->codec_type==AVMEDIA_TYPE_VIDEO){
                    ret = decode_write(pCodecCtx, packet, 0);
                    if(ret < 0)
                        break;
                }
                av_free_packet(&packet);
            }
            flush(pFormatCtx,i);
        }
    }
    if (videoStream == -1) {
        printf("Didn't find a video stream.\n");
        return -1;
    }
    if (NULL != outputFile) {
        fclose(outputFile);
        outputFile = NULL;
    }
    avcodec_close(pCodecCtx);
    avformat_close_input(&pFormatCtx);
    return ret;
}

