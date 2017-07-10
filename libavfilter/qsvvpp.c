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
 * @file
 * Intel Quick Sync Video VPP base function
 */

#include "config.h"
#include "libavutil/hwcontext.h"
#include "libavutil/hwcontext_qsv.h"
#include "libavutil/time.h"
#include "libavutil/pixdesc.h"
#include "internal.h"
#include "framesync.h"
#include "qsvvpp.h"

typedef struct QSVFrame {
    AVFrame          *frame;
    mfxFrameSurface1 *surface;
    mfxFrameSurface1  surface_internal;
    struct QSVFrame  *next;
} QSVFrame;

struct FFQSVVPPContext {
    AVBufferRef       *device_ctx_ref;
    AVBufferRef       *frames_ctx_ref;
    mfxSession         session;
    qsvvpp_post_proc   cb;
    int                in_video_mem;
    int                out_video_mem;
    QSVFrame          *in_frame_list;
    QSVFrame          *out_frame_list;
    mfxFrameInfo       in_info;
    mfxFrameInfo       out_info;
};

static const AVRational default_tb = {1, 90000};

static int pix_fmt_to_mfx_fourcc(int format)
{
    switch(format) {
        case AV_PIX_FMT_YUV420P:
            return MFX_FOURCC_YV12;
        case AV_PIX_FMT_NV12:
            return MFX_FOURCC_NV12;
        case AV_PIX_FMT_YUYV422:
            return MFX_FOURCC_YUY2;
        case AV_PIX_FMT_RGB32:
            return MFX_FOURCC_RGB4;
    }

    return MFX_FOURCC_NV12;
}

static int map_frame_to_surface(AVFrame *frame, mfxFrameSurface1 *surface)
{
    switch (frame->format) {
        case AV_PIX_FMT_NV12:
            surface->Data.Y  = frame->data[0];
            surface->Data.UV = frame->data[1];
            break;
        case AV_PIX_FMT_YUV420P:
            surface->Data.Y = frame->data[0];
            surface->Data.U = frame->data[1];
            surface->Data.V = frame->data[2];
            break;
        case AV_PIX_FMT_YUYV422:
            surface->Data.Y = frame->data[0];
            surface->Data.U = frame->data[0] + 1;
            surface->Data.V = frame->data[0] + 3;
            break;
        case AV_PIX_FMT_RGB32:
            surface->Data.B = frame->data[0];
            surface->Data.G = frame->data[0] + 1;
            surface->Data.R = frame->data[0] + 2;
            surface->Data.A = frame->data[0] + 3;
            break;
        default:
            return MFX_ERR_UNSUPPORTED;
    }
    surface->Data.Pitch     = frame->linesize[0];

    return 0;
}

static void clear_unused_frames(QSVFrame *list)
{
    while (list) {
        if (list->surface && !list->surface->Data.Locked) {
            list->surface = NULL;
            av_frame_free(&list->frame);
        }
        list = list->next;
    }
}

static void clear_frame_list(QSVFrame *list)
{
    QSVFrame *frame;

    while (list) {
        frame = list;
        list = list->next;
        av_frame_free(&frame->frame);
        av_freep(&frame);
    }
}

static QSVFrame *get_unused_frame(QSVFrame **list)
{
    QSVFrame *out = *list;

    for (; out; out = out->next)
        if (!out->surface)
            break;
    if (!out) {
        out = av_mallocz(sizeof(*out));
        if (!out)
            return NULL;
        out->next  = *list;
        *list      = out;
    }

    return out;
}

static QSVFrame *submit_frame(FFQSVVPPContext *s, AVFilterLink *inlink, AVFrame *picref)
{
    QSVFrame        *qsv_frame;
    AVFilterContext *ctx = inlink->dst;

    clear_unused_frames(s->in_frame_list);
    qsv_frame = get_unused_frame(&s->in_frame_list);
    if (!qsv_frame) {
        av_log(ctx, AV_LOG_ERROR, "Can't alloc new frame.\n");
        return NULL;
    }
    qsv_frame->frame = av_frame_clone(picref);

    /*
     * Turn AVFrame into mfxFrameSurface1.
     * For video memory mode, pix_fmt is AV_PIX_FMT_QSV, and
     * mfxFrameSurface1 is stored in AVFrame->data[3];
     * for system memory mode, raw video data is stored in
     * AVFrame, we should map it into mfxFrameSurface1.
     */
    if (s->in_video_mem) {
        if (qsv_frame->frame->format != AV_PIX_FMT_QSV) {
            av_log(ctx, AV_LOG_ERROR, "QSVVPP gets a wrong frame.\n");
            return NULL;
        }
        qsv_frame->surface = (mfxFrameSurface1*)qsv_frame->frame->data[3];
    } else {
        if (map_frame_to_surface(qsv_frame->frame,
                                &qsv_frame->surface_internal) < 0) {
            av_log(ctx, AV_LOG_ERROR, "Unsupported frame.\n");
            return NULL;
        }
        qsv_frame->surface = &qsv_frame->surface_internal;
    }

    if (FF_INLINK_IDX(inlink) == 0)
        qsv_frame->surface->Info = s->in_info;
    else
        ff_qsvvpp_frameinfo_fill(&qsv_frame->surface->Info, inlink, 0);

    qsv_frame->surface->Data.TimeStamp = av_rescale_q(qsv_frame->frame->pts,
            inlink->time_base, default_tb);

    return qsv_frame;
}

static QSVFrame *query_frame(FFQSVVPPContext *s, AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    QSVFrame        *out_frame;
    int              ret;

    clear_unused_frames(s->out_frame_list);
    out_frame = get_unused_frame(&s->out_frame_list);
    if (!out_frame) {
        av_log(ctx, AV_LOG_ERROR, "Can't alloc new output frame.\n");
        return NULL;
    }

    /*
     * For video memory, get a hw frame;
     * for system memory, get a sw frame and map it into a mfx_surface.
     */
    if (s->out_video_mem) {
        out_frame->frame = av_frame_alloc();
        if (!out_frame->frame)
            return NULL;

        ret = av_hwframe_get_buffer(s->frames_ctx_ref, out_frame->frame, 0);
        if (ret < 0) {
            av_log(ctx, AV_LOG_ERROR, "Can't allocate a surface.\n");
            return NULL;
        }
        out_frame->surface = (mfxFrameSurface1*)out_frame->frame->data[3];
    } else {
        /*
         * Get a frame with aligned dimensions.
         * Libmfx need system memory being 128x64 aligned
         */
        out_frame->frame = ff_get_video_buffer(outlink,
                                               FFALIGN(outlink->w, 128),
                                               FFALIGN(outlink->h, 64));
        if (!out_frame->frame)
            return NULL;
        out_frame->frame->width  = outlink->w;
        out_frame->frame->height = outlink->h;

        ret = map_frame_to_surface(out_frame->frame,
                                  &out_frame->surface_internal);
        if (ret < 0)
            return NULL;
        out_frame->surface = &out_frame->surface_internal;
    }
    out_frame->surface->Info = s->out_info;

    return out_frame;
}

int ff_qsvvpp_frameinfo_fill(mfxFrameInfo *frameinfo, AVFilterLink *link, int out)
{
    enum AVPixelFormat        pix_fmt;
    AVHWFramesContext        *frames_ctx;
    const AVPixFmtDescriptor *desc;

    if (out)
        pix_fmt = AV_PIX_FMT_NV12;
    else {
        if (link->format == AV_PIX_FMT_QSV) {
            if (!link->hw_frames_ctx) {
                av_log(link->dst, AV_LOG_ERROR, "HW format detected, but hw_frames_ctx is NULL.\n");
                return AVERROR(EINVAL);
            }
            frames_ctx = (AVHWFramesContext *)link->hw_frames_ctx->data;
            pix_fmt = frames_ctx->sw_format;
        } else
            pix_fmt = link->format;
    }

    desc = av_pix_fmt_desc_get(pix_fmt);
    if (!desc)
        return AVERROR_BUG;

    frameinfo->CropX          = 0;
    frameinfo->CropY          = 0;
    frameinfo->CropW          = link->w;
    frameinfo->CropH          = link->h;
    frameinfo->Width          = FFALIGN(link->w, 32);
    frameinfo->Height         = FFALIGN(link->h, 32);
    frameinfo->PicStruct      = MFX_PICSTRUCT_PROGRESSIVE;
    frameinfo->FrameRateExtN  = link->frame_rate.num;
    frameinfo->FrameRateExtD  = link->frame_rate.den;
    frameinfo->FourCC         = pix_fmt_to_mfx_fourcc(pix_fmt);
    frameinfo->BitDepthLuma   = desc->comp[0].depth;
    frameinfo->BitDepthChroma = desc->comp[0].depth;
    frameinfo->Shift          = desc->comp[0].depth > 8;
    if (desc->log2_chroma_w && desc->log2_chroma_h)
        frameinfo->ChromaFormat = MFX_CHROMAFORMAT_YUV420;
    else if (desc->log2_chroma_w)
        frameinfo->ChromaFormat = MFX_CHROMAFORMAT_YUV422;
    else
        frameinfo->ChromaFormat = MFX_CHROMAFORMAT_YUV444;
    frameinfo->AspectRatioW   = link->sample_aspect_ratio.num ? link->sample_aspect_ratio.num : 1;
    frameinfo->AspectRatioH   = link->sample_aspect_ratio.den ? link->sample_aspect_ratio.den : 1;

    return 0;
}

int ff_qsvvpp_create(AVFilterContext *avctx, FFQSVVPPContext **vpp, FFQSVVPPParam *param)
{
    int                 ret = 0;
    FFQSVVPPContext    *s;
    AVHWDeviceContext  *device_ctx;
    AVHWFramesContext  *frames_ctx;
    AVQSVDeviceContext *qsv_dev;
    AVQSVFramesContext *qsv_frames_ctx;

    if (!avctx || !vpp)
        return AVERROR(EINVAL);

    s = av_mallocz(sizeof(FFQSVVPPContext));
    if (!s) {
        ret = AVERROR(ENOMEM);
        goto failed;
    }

    s->cb            = param->cb;
    s->in_video_mem  = !!(param->vpp_param.IOPattern & MFX_IOPATTERN_IN_VIDEO_MEMORY);
    s->out_video_mem = !!(param->vpp_param.IOPattern & MFX_IOPATTERN_OUT_VIDEO_MEMORY);
    s->in_info       = param->vpp_param.vpp.In;
    s->out_info      = param->vpp_param.vpp.Out;

    if (avctx->hw_device_ctx) {
        s->device_ctx_ref = av_buffer_ref(avctx->hw_device_ctx);
        if (!s->device_ctx_ref)
            return AVERROR(ENOMEM);
    } else {
        ret = av_hwdevice_ctx_create(&s->device_ctx_ref,
                                     AV_HWDEVICE_TYPE_QSV, NULL, NULL, 0);
        if (ret < 0) {
            av_log(avctx, AV_LOG_ERROR, "Fail to create QSV hwdevice.\n");
            goto failed;
        }

        device_ctx = (AVHWDeviceContext*)s->device_ctx_ref->data;
        qsv_dev    = device_ctx->hwctx;
        s->session = qsv_dev->session;
    }

    if (s->out_video_mem) {
        /*
         * Create an internal frames_ctx, to get a mfxSession and a buffer pool of
         * QSV format.
         */
        s->frames_ctx_ref = av_hwframe_ctx_alloc(s->device_ctx_ref);
        if (!s->frames_ctx_ref) {
            ret = AVERROR(ENOMEM);
            goto failed;
        }
        frames_ctx = (AVHWFramesContext*)s->frames_ctx_ref->data;
        qsv_frames_ctx                = frames_ctx->hwctx;
        frames_ctx->width             = s->out_info.CropW;
        frames_ctx->height            = s->out_info.CropH;
        frames_ctx->format            = AV_PIX_FMT_QSV;
        frames_ctx->sw_format         = param->sw_format;
        frames_ctx->initial_pool_size = 0;
        qsv_frames_ctx->frame_type    = MFX_MEMTYPE_VIDEO_MEMORY_DECODER_TARGET;
        ret = av_hwframe_ctx_init(s->frames_ctx_ref);
        if (ret < 0) {
            av_log(avctx, AV_LOG_ERROR, "Can't init a frames_ctx.\n");
            goto failed;
        }

        avctx->outputs[0]->hw_frames_ctx = av_buffer_ref(s->frames_ctx_ref);
        s->session = qsv_frames_ctx->child_session;
    }

    ret = MFXVideoVPP_Init(s->session, &param->vpp_param);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create a qsvvpp, ret = %d.\n", ret);
        goto failed;
    }

    *vpp = s;
    s    = NULL;
failed:
    if (s) {
        av_buffer_unref(&s->frames_ctx_ref);
        av_buffer_unref(&s->device_ctx_ref);
        av_freep(&s);
    }

    return ret;
}

int ff_qsvvpp_free(FFQSVVPPContext **vpp)
{
    FFQSVVPPContext *s = *vpp;

    if (!s)
        return 0;

    MFXVideoVPP_Close(s->session);
    clear_frame_list(s->in_frame_list);
    clear_frame_list(s->out_frame_list);
    av_buffer_unref(&s->frames_ctx_ref);
    av_buffer_unref(&s->device_ctx_ref);
    av_freep(vpp);

    return 0;
}

int ff_qsvvpp_filter_frame(FFQSVVPPContext *s, AVFilterLink *inlink, AVFrame *picref)
{
    AVFilterContext  *ctx     = inlink->dst;
    AVFilterLink     *outlink = ctx->outputs[0];
    mfxSyncPoint      sync;
    QSVFrame         *in_frame, *out_frame;
    int               ret, filter_ret;

    do {
        in_frame = submit_frame(s, inlink, picref);
        if (!in_frame) {
            av_log(ctx, AV_LOG_ERROR, "Fail to submit frame. on input[%d]\n",
                    FF_INLINK_IDX(inlink));
            return AVERROR(EINVAL);
        }

        out_frame = query_frame(s, outlink);
        if(!out_frame) {
            av_log(ctx, AV_LOG_ERROR, "Fail to query an output frame.\n");
            return AVERROR(ENOMEM);
        }

        do {
            ret = MFXVideoVPP_RunFrameVPPAsync(s->session, in_frame->surface,
                                               out_frame->surface, NULL, &sync);
            if (ret == MFX_WRN_DEVICE_BUSY)
                av_usleep(500);
        } while (ret == MFX_WRN_DEVICE_BUSY);

        if (ret < 0 && ret != MFX_ERR_MORE_SURFACE) {
            /*Ignore more_data error*/
            if (ret == MFX_ERR_MORE_DATA)
                ret = 0;
            break;
        }

        if (MFXVideoCORE_SyncOperation(s->session, sync, 1000) < 0)
            av_log(ctx, AV_LOG_WARNING, "Sync failed.\n");

        out_frame->frame->pts = av_rescale_q(out_frame->surface->Data.TimeStamp,
                                    default_tb, outlink->time_base);
        if (s->cb)
            filter_ret = s->cb(outlink, out_frame->frame);
        else
            filter_ret = ff_filter_frame(outlink, out_frame->frame);
        out_frame->frame = NULL;
        if (filter_ret < 0) {
            ret = filter_ret;
            break;
        }
    } while(ret == MFX_ERR_MORE_SURFACE);

    return ret;
}
