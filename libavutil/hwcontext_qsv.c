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

#include <stdint.h>
#include <string.h>

#include <mfx/mfxvideo.h>

#include "config.h"

#if CONFIG_VAAPI
#include "hwcontext_vaapi.h"
#endif
#if CONFIG_DXVA2
#include "hwcontext_dxva2.h"
#endif

#include "buffer.h"
#include "common.h"
#include "hwcontext.h"
#include "hwcontext_internal.h"
#include "hwcontext_qsv.h"
#include "mem.h"
#include "pixfmt.h"
#include "pixdesc.h"
#include "time.h"

typedef struct QSVDevicePriv {
    AVBufferRef *child_device_ctx;
} QSVDevicePriv;

typedef struct QSVDeviceContext {
    mfxHDL              handle;
    mfxHandleType       handle_type;
    mfxVersion          ver;
    mfxIMPL             impl;

    enum AVHWDeviceType child_device_type;
    enum AVPixelFormat  child_pix_fmt;
} QSVDeviceContext;

typedef struct QSVFramesContext {
    mfxSession session_download;
    mfxSession session_upload;

    AVBufferRef *child_frames_ref;
    mfxFrameSurface1 *surfaces_internal;
    int             nb_surfaces_used;

    // used in the frame allocator for non-opaque surfaces
    mfxMemId *mem_ids;
    // used in the opaque alloc request for opaque surfaces
    mfxFrameSurface1 **surface_ptrs;

    mfxExtOpaqueSurfaceAlloc opaque_alloc;
    mfxExtBuffer *ext_buffers[1];
} QSVFramesContext;

typedef struct QSVMemId {
    /*
     * A buffer refer to VASurfaceID.
     */
    AVBufferRef *va_surf_ref;
    uint32_t fourcc;
} QSVMemId;

static const struct {
    mfxHandleType handle_type;
    enum AVHWDeviceType device_type;
    enum AVPixelFormat  pix_fmt;
} supported_handle_types[] = {
#if CONFIG_VAAPI
    { MFX_HANDLE_VA_DISPLAY,          AV_HWDEVICE_TYPE_VAAPI, AV_PIX_FMT_VAAPI },
#endif
#if CONFIG_DXVA2
    { MFX_HANDLE_D3D9_DEVICE_MANAGER, AV_HWDEVICE_TYPE_DXVA2, AV_PIX_FMT_DXVA2_VLD },
#endif
    { 0 },
};

static const struct {
    enum AVPixelFormat pix_fmt;
    uint32_t           fourcc;
} supported_pixel_formats[] = {
    { AV_PIX_FMT_NV12, MFX_FOURCC_NV12 },
    { AV_PIX_FMT_RGB32,MFX_FOURCC_RGB4 },
    { AV_PIX_FMT_P010, MFX_FOURCC_P010 },
};

static int qsv_device_init(AVHWDeviceContext *ctx)
{
    AVQSVDeviceContext *hwctx = ctx->hwctx;
    QSVDeviceContext       *s = ctx->internal->priv;

    mfxStatus err;
    int i;

    for (i = 0; supported_handle_types[i].handle_type; i++) {
        err = MFXVideoCORE_GetHandle(hwctx->session, supported_handle_types[i].handle_type,
                                     &s->handle);
        if (err == MFX_ERR_NONE) {
            s->handle_type       = supported_handle_types[i].handle_type;
            s->child_device_type = supported_handle_types[i].device_type;
            s->child_pix_fmt     = supported_handle_types[i].pix_fmt;
            break;
        }
    }
    if (!s->handle) {
        av_log(ctx, AV_LOG_VERBOSE, "No supported hw handle could be retrieved "
               "from the session\n");
    }

    err = MFXQueryIMPL(hwctx->session, &s->impl);
    if (err == MFX_ERR_NONE)
        err = MFXQueryVersion(hwctx->session, &s->ver);
    if (err != MFX_ERR_NONE) {
        av_log(ctx, AV_LOG_ERROR, "Error querying the session attributes\n");
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static void qsv_frames_uninit(AVHWFramesContext *ctx)
{
    QSVFramesContext *s = ctx->internal->priv;
    AVQSVFramesContext *frame_ctx = ctx->hwctx;
    int i;

    if (s->session_download) {
        MFXVideoVPP_Close(s->session_download);
        MFXClose(s->session_download);
    }
    s->session_download = NULL;

    if (s->session_upload) {
        MFXVideoVPP_Close(s->session_upload);
        MFXClose(s->session_upload);
    }
    s->session_upload = NULL;

    if (frame_ctx->child_session) {
        MFXClose(frame_ctx->child_session);
        frame_ctx->child_session = NULL;
    }

    /*
     * Release cached VASurfaceID and QSVMemId
     */
    if (  CONFIG_VAAPI &&
        !(frame_ctx->frame_type & MFX_MEMTYPE_OPAQUE_FRAME))
        for (i = 0; i < frame_ctx->nb_surfaces; i++) {
            QSVMemId *memid = (QSVMemId*)s->mem_ids[i];
            av_buffer_unref(&memid->va_surf_ref);
            av_freep(&memid);
        }

    av_freep(&s->mem_ids);
    av_freep(&s->surface_ptrs);
    av_freep(&s->surfaces_internal);
    av_buffer_unref(&s->child_frames_ref);
}

static void qsv_pool_release_dummy(void *opaque, uint8_t *data)
{
}

static AVBufferRef *qsv_pool_alloc(void *opaque, int size)
{
    AVHWFramesContext    *ctx = (AVHWFramesContext*)opaque;
    QSVFramesContext       *s = ctx->internal->priv;
    AVQSVFramesContext *hwctx = ctx->hwctx;
#if CONFIG_VAAPI
    QSVMemId           *memid = NULL;
    AVHWFramesContext *child_ctx = (AVHWFramesContext*)s->child_frames_ref->data;
#endif

    if (s->nb_surfaces_used < hwctx->nb_surfaces) {
        /*
         * Note that when child device is VAAPI, VASurfaceID isnot allocated yet.
         * Get one from child_ctx->pool.
         */
        if (  CONFIG_VAAPI &&
            !(hwctx->frame_type & MFX_MEMTYPE_OPAQUE_FRAME)) {
            memid = s->surfaces_internal[s->nb_surfaces_used].Data.MemId;
            memid->va_surf_ref = av_buffer_pool_get(child_ctx->pool);
            if (!memid->va_surf_ref)
                return NULL;
            memid->fourcc      = s->surfaces_internal[0].Info.FourCC;
        }

        s->nb_surfaces_used++;
        return av_buffer_create((uint8_t*)(s->surfaces_internal + s->nb_surfaces_used - 1),
                                sizeof(*hwctx->surfaces), qsv_pool_release_dummy, NULL, 0);
    }

    return NULL;
}

static int qsv_init_child_ctx(AVHWFramesContext *ctx)
{
#if CONFIG_DXVA2
    AVQSVFramesContext     *hwctx = ctx->hwctx;
    int                         i = 0;
#endif
    QSVFramesContext           *s = ctx->internal->priv;
    QSVDeviceContext *device_priv = ctx->device_ctx->internal->priv;

    AVBufferRef *child_device_ref = NULL;
    AVBufferRef *child_frames_ref = NULL;

    AVHWDeviceContext *child_device_ctx;
    AVHWFramesContext *child_frames_ctx;

    int ret = 0;

    if (!device_priv->handle) {
        av_log(ctx, AV_LOG_ERROR,
               "Cannot create a non-opaque internal surface pool without "
               "a hardware handle\n");
        return AVERROR(EINVAL);
    }

    child_device_ref = av_hwdevice_ctx_alloc(device_priv->child_device_type);
    if (!child_device_ref)
        return AVERROR(ENOMEM);
    child_device_ctx   = (AVHWDeviceContext*)child_device_ref->data;

#if CONFIG_VAAPI
    if (child_device_ctx->type == AV_HWDEVICE_TYPE_VAAPI) {
        AVVAAPIDeviceContext *child_device_hwctx = child_device_ctx->hwctx;
        child_device_hwctx->display = (VADisplay)device_priv->handle;
    }
#endif
#if CONFIG_DXVA2
    if (child_device_ctx->type == AV_HWDEVICE_TYPE_DXVA2) {
        AVDXVA2DeviceContext *child_device_hwctx = child_device_ctx->hwctx;
        child_device_hwctx->devmgr = (IDirect3DDeviceManager9*)device_priv->handle;
    }
#endif

    ret = av_hwdevice_ctx_init(child_device_ref);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error initializing a child device context\n");
        goto fail;
    }

    child_frames_ref = av_hwframe_ctx_alloc(child_device_ref);
    if (!child_frames_ref) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }
    child_frames_ctx = (AVHWFramesContext*)child_frames_ref->data;

    child_frames_ctx->format            = device_priv->child_pix_fmt;
    child_frames_ctx->sw_format         = ctx->sw_format;
#if CONFIG_DXVA2
    child_frames_ctx->initial_pool_size = ctx->initial_pool_size;
#endif
#if CONFIG_VAAPI
    /*
     * Allocate VASurfaceID dynamically.
     */
    child_frames_ctx->initial_pool_size = 0;
#endif
    child_frames_ctx->width             = ctx->width;
    child_frames_ctx->height            = ctx->height;

#if CONFIG_DXVA2
    if (child_device_ctx->type == AV_HWDEVICE_TYPE_DXVA2) {
        AVDXVA2FramesContext *child_frames_hwctx = child_frames_ctx->hwctx;
        if (hwctx->frame_type & MFX_MEMTYPE_VIDEO_MEMORY_PROCESSOR_TARGET)
            child_frames_hwctx->surface_type = DXVA2_VideoProcessorRenderTarget;
        else
            child_frames_hwctx->surface_type = DXVA2_VideoDecoderRenderTarget;
    }
#endif

    ret = av_hwframe_ctx_init(child_frames_ref);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error initializing a child frames context\n");
        goto fail;
    }

#if CONFIG_DXVA2
    if (child_device_ctx->type == AV_HWDEVICE_TYPE_DXVA2) {
        AVDXVA2FramesContext *child_frames_hwctx = child_frames_ctx->hwctx;
        for (i = 0; i < ctx->initial_pool_size; i++)
            s->surfaces_internal[i].Data.MemId = (mfxMemId)child_frames_hwctx->surfaces[i];
        if (child_frames_hwctx->surface_type == DXVA2_VideoProcessorRenderTarget)
            hwctx->frame_type = MFX_MEMTYPE_VIDEO_MEMORY_PROCESSOR_TARGET;
        else
            hwctx->frame_type = MFX_MEMTYPE_VIDEO_MEMORY_DECODER_TARGET;
    }
#endif

    s->child_frames_ref       = child_frames_ref;
    child_frames_ref          = NULL;

fail:
    av_buffer_unref(&child_device_ref);
    av_buffer_unref(&child_frames_ref);
    return ret;
}

static int qsv_init_pool(AVHWFramesContext *ctx, uint32_t fourcc)
{
    QSVFramesContext              *s = ctx->internal->priv;
    AVQSVFramesContext *frames_hwctx = ctx->hwctx;
    const AVPixFmtDescriptor *desc;

    int i, ret = 0, nb_surfaces;
    int opaque = !!(frames_hwctx->frame_type & MFX_MEMTYPE_OPAQUE_FRAME);

    desc = av_pix_fmt_desc_get(ctx->sw_format);
    if (!desc)
        return AVERROR_BUG;

#if CONFIG_DXVA2
    if (ctx->initial_pool_size <= 0) {
        av_log(ctx, AV_LOG_ERROR, "QSV requires a fixed frame pool size\n");
        return AVERROR(EINVAL);
    }
    nb_surfaces = ctx->initial_pool_size;
#endif
#if CONFIG_VAAPI
    /*
     * We preallocate 128 mfxSurfaces at least.
     * why 128: decoder 14 + encoder 14 + encoder look_ahead_depth 100.
     * As VASurface is not allocated yet, it won't waste too many memory.
     */
    nb_surfaces = 128;
    if (ctx->initial_pool_size)
        nb_surfaces = ctx->initial_pool_size;
#endif

    s->surfaces_internal = av_mallocz_array(nb_surfaces,
                                            sizeof(*s->surfaces_internal));
    if (!s->surfaces_internal)
        return AVERROR(ENOMEM);

    for (i = 0; i < nb_surfaces; i++) {
        mfxFrameSurface1 *surf = &s->surfaces_internal[i];

        surf->Info.BitDepthLuma   = desc->comp[0].depth;
        surf->Info.BitDepthChroma = desc->comp[0].depth;
        surf->Info.Shift          = desc->comp[0].depth > 8;

        if (desc->log2_chroma_w && desc->log2_chroma_h)
            surf->Info.ChromaFormat   = MFX_CHROMAFORMAT_YUV420;
        else if (desc->log2_chroma_w)
            surf->Info.ChromaFormat   = MFX_CHROMAFORMAT_YUV422;
        else
            surf->Info.ChromaFormat   = MFX_CHROMAFORMAT_YUV444;

        surf->Info.FourCC         = fourcc;
        /*
         * WxH being aligned with 32x32 is needed by MSDK.
         * CropW and CropH are the real size of the frame.
         */
        surf->Info.Width          = FFALIGN(ctx->width, 32);
        surf->Info.CropW          = ctx->width;
        surf->Info.Height         = FFALIGN(ctx->height, 32);
        surf->Info.CropH          = ctx->height;
        surf->Info.FrameRateExtN  = 25;
        surf->Info.FrameRateExtD  = 1;
        surf->Info.PicStruct      = MFX_PICSTRUCT_PROGRESSIVE;
#if CONFIG_VAAPI
        if (!opaque) {
            surf->Data.MemId      = av_mallocz(sizeof(QSVMemId));
            if (!surf->Data.MemId)
                return AVERROR(ENOMEM);
        }
#endif
    }

    if (!opaque) {
        ret = qsv_init_child_ctx(ctx);
        if (ret < 0)
            return ret;
    }

    ctx->internal->pool_internal = av_buffer_pool_init2(sizeof(mfxFrameSurface1),
                                                        ctx, qsv_pool_alloc, NULL);
    if (!ctx->internal->pool_internal)
        return AVERROR(ENOMEM);

    frames_hwctx->surfaces    = s->surfaces_internal;
    frames_hwctx->nb_surfaces = nb_surfaces;

    return 0;
}

#if CONFIG_VAAPI
/*
 * A wrapper to free buffer-packaged VABufferID.
 */
static void release_va_buffer(void *opaque, uint8_t *data)
{
    AVHWFramesContext    *ctx = opaque;
    QSVDeviceContext*dev_priv = ctx->device_ctx->internal->priv;
    VADisplay             dpy = dev_priv->handle;
    VABufferID            bid = (VABufferID)data;

    vaDestroyBuffer(dpy, bid);
}

/*
 * @func alloc_internal_frame
 * @desc Deal with situation that qsvenc requests internal frames.
 */
static int alloc_internal_frame(AVHWFramesContext *ctx, mfxFrameAllocRequest *req,
                             mfxFrameAllocResponse *resp)
{
    QSVDeviceContext*dev_priv = ctx->device_ctx->internal->priv;
    QSVFramesContext       *s = ctx->internal->priv;
    AVHWFramesContext *child_ctx = (AVHWFramesContext*)s->child_frames_ref->data;
    mfxFrameInfo           *i = &req->Info;
    VAContextID           cid = req->AllocId;
    VABufferType         type = VAEncCodedBufferType;
    VADisplay             dpy = (VADisplay)dev_priv->handle;
    uint32_t         buf_size;
    int                   ret;
    VABufferID            bid;
    QSVMemId           *memid;

    /*
     * Allocate 1 more to store the type of these memids.
     */
    resp->mids = av_calloc(req->NumFrameSuggested + 1, sizeof(*resp->mids));
    if(NULL == resp->mids){
        return MFX_ERR_MEMORY_ALLOC;
    }
    
    if (i->FourCC == MFX_FOURCC_P8) {
        buf_size = FFALIGN(i->Width, 32) * FFALIGN(i->Height, 32) * 400LL / (16 * 16);
        for (resp->NumFrameActual = 0;
             resp->NumFrameActual < req->NumFrameSuggested;
             resp->NumFrameActual++) {
            resp->mids[resp->NumFrameActual] = memid = av_mallocz(sizeof(*memid));
            if (!memid)
                break;

            ret = vaCreateBuffer(dpy, cid, type, buf_size, 1, NULL, &bid);
            if (ret != VA_STATUS_SUCCESS) {
                av_log(ctx, AV_LOG_ERROR, "Create Buffer failed with %s.\n",
                        vaErrorStr(ret));
                av_freep(&resp->mids[resp->NumFrameActual]);
                break;
            }
            memid->va_surf_ref = av_buffer_create((uint8_t*)(uintptr_t)bid,
                    sizeof(bid), release_va_buffer, ctx, AV_BUFFER_FLAG_READONLY);
            memid->fourcc      = i->FourCC;
        }
    } else {
        for (resp->NumFrameActual = 0;
             resp->NumFrameActual < req->NumFrameSuggested;
             resp->NumFrameActual++) {
            resp->mids[resp->NumFrameActual] = memid = av_mallocz(sizeof(*memid));
            if (!memid)
                break;

            memid->va_surf_ref = av_buffer_pool_get(child_ctx->pool);
            if (!memid->va_surf_ref) {
                av_freep(&resp->mids[resp->NumFrameActual]);
                break;
            }
            memid->fourcc      = i->FourCC;
        }
    }
    resp->mids[resp->NumFrameActual] = (mfxMemId)MFX_MEMTYPE_INTERNAL_FRAME;

    return 0;
}
#endif

static mfxStatus frame_alloc(mfxHDL pthis, mfxFrameAllocRequest *req,
                             mfxFrameAllocResponse *resp)
{
    AVHWFramesContext    *ctx = pthis;
    QSVFramesContext       *s = ctx->internal->priv;
    AVQSVFramesContext *hwctx = ctx->hwctx;
    mfxFrameInfo           *i = &req->Info;
    mfxFrameInfo          *i1 = &hwctx->surfaces[0].Info;

    if (!(req->Type & MFX_MEMTYPE_VIDEO_MEMORY_PROCESSOR_TARGET) ||
        !(req->Type & (MFX_MEMTYPE_FROM_VPPIN | MFX_MEMTYPE_FROM_VPPOUT)) ||
        !(req->Type & MFX_MEMTYPE_EXTERNAL_FRAME))
        /*
         * Check if the request comes from decoder.
         */
        if (!(req->Type & MFX_MEMTYPE_VIDEO_MEMORY_DECODER_TARGET) ||
            !(req->Type & (MFX_MEMTYPE_FROM_DECODE | MFX_MEMTYPE_FROM_ENCODE)) ||
            !(req->Type & (MFX_MEMTYPE_EXTERNAL_FRAME | MFX_MEMTYPE_INTERNAL_FRAME)))
            return MFX_ERR_UNSUPPORTED;

    if (i->Width  > i1->Width || i->Height > i1->Height ||
        i->FourCC != i1->FourCC || i->ChromaFormat != i1->ChromaFormat) {
        av_log(ctx, AV_LOG_WARNING, "Mismatching surface properties in an "
               "allocation request: %dx%d %d %d vs %dx%d %d %d\n",
               i->Width,  i->Height,  i->FourCC,  i->ChromaFormat,
               i1->Width, i1->Height, i1->FourCC, i1->ChromaFormat);
#if CONFIG_VAAPI
        /*
         * For hevc_enc, as a plugin is loaded, we should allocate internal frames
         * for it.
         */
        if (i->FourCC != MFX_FOURCC_P8 || !(req->Type & MFX_MEMTYPE_INTERNAL_FRAME))
#endif
            return MFX_ERR_UNSUPPORTED;
    }

    if (req->Type & MFX_MEMTYPE_INTERNAL_FRAME)
        return alloc_internal_frame(ctx, req, resp);
    else {
        resp->mids           = s->mem_ids;
        resp->NumFrameActual = hwctx->nb_surfaces;
    }

    return MFX_ERR_NONE;
}

static mfxStatus frame_free(mfxHDL pthis, mfxFrameAllocResponse *resp)
{
#if CONFIG_VAAPI
    QSVMemId *mid;
    int i;
    mfxU32 mem_type = (mfxU32)resp->mids[resp->NumFrameActual];

    if (mem_type & MFX_MEMTYPE_INTERNAL_FRAME) {
        for (i = 0; i < resp->NumFrameActual; i++) {
            mid = (QSVMemId *)resp->mids[i];
            av_buffer_unref(&mid->va_surf_ref);
            av_freep(&mid);
        }
        av_freep(&resp->mids);
    }
#endif
    return MFX_ERR_NONE;
}

static mfxStatus frame_lock(mfxHDL pthis, mfxMemId mid, mfxFrameData *ptr)
{
#if CONFIG_VAAPI
    AVHWFramesContext     *ctx = pthis;
    QSVDeviceContext *dev_priv = ctx->device_ctx->internal->priv;
    QSVMemId            *memid = mid;
    VABufferID             bid = (VABufferID)memid->va_surf_ref->data;
    VADisplay              dpy = dev_priv->handle;
    VACodedBufferSegment *coded_buffer_segment;
    VAStatus  va_res;

    if (memid->fourcc == MFX_FOURCC_P8) {
        va_res = vaMapBuffer(dpy, bid, (void **)&coded_buffer_segment);
        if (va_res == 0) {
            ptr->Y = (mfxU8*)coded_buffer_segment->buf;
            return MFX_ERR_NONE;
        }
    }
#endif

    return MFX_ERR_UNSUPPORTED;
}

static mfxStatus frame_unlock(mfxHDL pthis, mfxMemId mid, mfxFrameData *ptr)
{
#if CONFIG_VAAPI
    AVHWFramesContext     *ctx = pthis;
    QSVDeviceContext *dev_priv = ctx->device_ctx->internal->priv;
    QSVMemId            *memid = mid;
    VABufferID             bid = (VABufferID)memid->va_surf_ref->data;
    VADisplay              dpy = dev_priv->handle;

    if (memid->fourcc == MFX_FOURCC_P8) {
        vaUnmapBuffer(dpy, bid);
        return 0;
    }
#endif

    return MFX_ERR_UNSUPPORTED;
}

static mfxStatus frame_get_hdl(mfxHDL pthis, mfxMemId mid, mfxHDL *hdl)
{
#if CONFIG_DXVA2
    *hdl = mid;
#endif
#if CONFIG_VAAPI
    QSVMemId *memid = (QSVMemId *)mid;
    *hdl = (mfxHDL)&memid->va_surf_ref->data;
#endif
    return MFX_ERR_NONE;
}

/*
 * Param @upload: 0 - download session
 *                1 - upload session
 *                -1- none
 */
static int qsv_init_internal_session(AVHWFramesContext *ctx,
                                     mfxSession *session, int upload)
{
    QSVFramesContext              *s = ctx->internal->priv;
    AVQSVFramesContext *frames_hwctx = ctx->hwctx;
    AVQSVDeviceContext *device_hwctx = ctx->device_ctx->hwctx;
    QSVDeviceContext   *device_priv  = ctx->device_ctx->internal->priv;
    int opaque = !!(frames_hwctx->frame_type & MFX_MEMTYPE_OPAQUE_FRAME);

    mfxFrameAllocator frame_allocator = {
        .pthis  = ctx,
        .Alloc  = frame_alloc,
        .Lock   = frame_lock,
        .Unlock = frame_unlock,
        .GetHDL = frame_get_hdl,
        .Free   = frame_free,
    };

    mfxVideoParam par;
    mfxStatus err;

    /*
     * MFXCloneSession is light-weigt for MFXInit and MFXJoinSession.
     * It's recommended to join together sessions that will work in ONE pipeline.
     * This will benefit MSDK to manage tasks.
     */
    err = MFXCloneSession(device_hwctx->session, session);
    if (err != MFX_ERR_NONE) {
        av_log(ctx, AV_LOG_ERROR, "Error initializing an internal session\n");
        return AVERROR_UNKNOWN;
    }

    if (device_priv->handle) {
        err = MFXVideoCORE_SetHandle(*session, device_priv->handle_type,
                                     device_priv->handle);
        if (err != MFX_ERR_NONE)
            return AVERROR_UNKNOWN;
    }

    if (!opaque) {
        err = MFXVideoCORE_SetFrameAllocator(*session, &frame_allocator);
        if (err != MFX_ERR_NONE)
            return AVERROR_UNKNOWN;
    }

    if (upload == -1)
        return 0;

    memset(&par, 0, sizeof(par));

    if (opaque) {
        par.ExtParam    = s->ext_buffers;
        par.NumExtParam = FF_ARRAY_ELEMS(s->ext_buffers);
        par.IOPattern   = upload ? MFX_IOPATTERN_OUT_OPAQUE_MEMORY :
                                   MFX_IOPATTERN_IN_OPAQUE_MEMORY;
    } else {
        par.IOPattern = upload ? MFX_IOPATTERN_OUT_VIDEO_MEMORY :
                                 MFX_IOPATTERN_IN_VIDEO_MEMORY;
    }

    par.IOPattern |= upload ? MFX_IOPATTERN_IN_SYSTEM_MEMORY :
                              MFX_IOPATTERN_OUT_SYSTEM_MEMORY;
    par.AsyncDepth = 1;

    par.vpp.In = frames_hwctx->surfaces[0].Info;

    /* Apparently VPP requires the frame rate to be set to some value, otherwise
     * init will fail (probably for the framerate conversion filter). Since we
     * are only doing data upload/download here, we just invent an arbitrary
     * value */
    par.vpp.In.FrameRateExtN = 25;
    par.vpp.In.FrameRateExtD = 1;
    par.vpp.Out = par.vpp.In;

    err = MFXVideoVPP_Init(*session, &par);
    if (err != MFX_ERR_NONE) {
        av_log(ctx, AV_LOG_ERROR, "Error opening the internal VPP session\n");
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static int qsv_frames_init(AVHWFramesContext *ctx)
{
    QSVFramesContext              *s = ctx->internal->priv;
    AVQSVFramesContext *frames_hwctx = ctx->hwctx;

    int opaque = !!(frames_hwctx->frame_type & MFX_MEMTYPE_OPAQUE_FRAME);

    uint32_t fourcc = 0;
    int i, ret;

    for (i = 0; i < FF_ARRAY_ELEMS(supported_pixel_formats); i++) {
        if (supported_pixel_formats[i].pix_fmt == ctx->sw_format) {
            fourcc = supported_pixel_formats[i].fourcc;
            break;
        }
    }
    if (!fourcc) {
        av_log(ctx, AV_LOG_ERROR, "Unsupported pixel format\n");
        return AVERROR(ENOSYS);
    }

    if (!ctx->pool) {
        ret = qsv_init_pool(ctx, fourcc);
        if (ret < 0) {
            av_log(ctx, AV_LOG_ERROR, "Error creating an internal frame pool\n");
            return ret;
        }
    }

    if (opaque) {
        s->surface_ptrs = av_mallocz_array(frames_hwctx->nb_surfaces,
                                           sizeof(*s->surface_ptrs));
        if (!s->surface_ptrs)
            return AVERROR(ENOMEM);

        for (i = 0; i < frames_hwctx->nb_surfaces; i++)
            s->surface_ptrs[i] = frames_hwctx->surfaces + i;

        s->opaque_alloc.In.Surfaces   = s->surface_ptrs;
        s->opaque_alloc.In.NumSurface = frames_hwctx->nb_surfaces;
        s->opaque_alloc.In.Type       = frames_hwctx->frame_type;

        s->opaque_alloc.Out = s->opaque_alloc.In;

        s->opaque_alloc.Header.BufferId = MFX_EXTBUFF_OPAQUE_SURFACE_ALLOCATION;
        s->opaque_alloc.Header.BufferSz = sizeof(s->opaque_alloc);

        s->ext_buffers[0] = (mfxExtBuffer*)&s->opaque_alloc;
    } else {
        s->mem_ids = av_mallocz_array(frames_hwctx->nb_surfaces + 1, sizeof(*s->mem_ids));
        if (!s->mem_ids)
            return AVERROR(ENOMEM);

        for (i = 0; i < frames_hwctx->nb_surfaces; i++)
            s->mem_ids[i] = frames_hwctx->surfaces[i].Data.MemId;
        s->mem_ids[i] = (mfxMemId)MFX_MEMTYPE_EXTERNAL_FRAME;
        frames_hwctx->frame_type = MFX_MEMTYPE_VIDEO_MEMORY_DECODER_TARGET;
    }

    ret = qsv_init_internal_session(ctx, &s->session_download, 0);
    if (ret < 0)
        return ret;

    ret = qsv_init_internal_session(ctx, &s->session_upload, 1);
    if (ret < 0)
        return ret;

    /*
     * Create a session for outside use.
     * User can use this session to create QSV modules.
     */
    ret = qsv_init_internal_session(ctx, &frames_hwctx->child_session, -1);
    if (ret < 0)
        return ret;

    return 0;
}

static int qsv_get_buffer(AVHWFramesContext *ctx, AVFrame *frame)
{
    frame->buf[0] = av_buffer_pool_get(ctx->pool);
    if (!frame->buf[0])
        return AVERROR(ENOMEM);

    frame->data[3] = frame->buf[0]->data;
    frame->format  = AV_PIX_FMT_QSV;
    frame->width   = ctx->width;
    frame->height  = ctx->height;

    return 0;
}

static int qsv_transfer_get_formats(AVHWFramesContext *ctx,
                                    enum AVHWFrameTransferDirection dir,
                                    enum AVPixelFormat **formats)
{
    enum AVPixelFormat *fmts;

    fmts = av_malloc_array(2, sizeof(*fmts));
    if (!fmts)
        return AVERROR(ENOMEM);

    fmts[0] = ctx->sw_format;
    fmts[1] = AV_PIX_FMT_NONE;

    *formats = fmts;

    return 0;
}

static int map_frame_to_surface(const AVFrame *frame, mfxFrameSurface1 *surface)
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
    surface->Data.TimeStamp = frame->pts;

    return 0;
}

static int qsv_transfer_data_from(AVHWFramesContext *ctx, AVFrame *dst,
                                  const AVFrame *src)
{
    QSVFramesContext  *s = ctx->internal->priv;
    mfxFrameSurface1 out = {{ 0 }};
    mfxFrameSurface1 *in = (mfxFrameSurface1*)src->data[3];

    mfxSyncPoint sync = NULL;
    mfxStatus err;

    out.Info = in->Info;
    map_frame_to_surface(dst, &out);

    do {
        err = MFXVideoVPP_RunFrameVPPAsync(s->session_download, in, &out, NULL, &sync);
        if (err == MFX_WRN_DEVICE_BUSY)
            av_usleep(1);
    } while (err == MFX_WRN_DEVICE_BUSY);

    if (err < 0 || !sync) {
        av_log(ctx, AV_LOG_ERROR, "Error downloading the surface\n");
        return AVERROR_UNKNOWN;
    }

    do {
        err = MFXVideoCORE_SyncOperation(s->session_download, sync, 1000);
    } while (err == MFX_WRN_IN_EXECUTION);
    if (err < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error synchronizing the operation: %d\n", err);
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static int qsv_transfer_data_to(AVHWFramesContext *ctx, AVFrame *dst,
                                const AVFrame *src)
{
    QSVFramesContext   *s = ctx->internal->priv;
    mfxFrameSurface1   in = {{ 0 }};
    mfxFrameSurface1 *out = (mfxFrameSurface1*)dst->data[3];

    mfxSyncPoint sync = NULL;
    mfxStatus err;

    in.Info = out->Info;
    map_frame_to_surface(src, &in);

    do {
        err = MFXVideoVPP_RunFrameVPPAsync(s->session_upload, &in, out, NULL, &sync);
        if (err == MFX_WRN_DEVICE_BUSY)
            av_usleep(1);
    } while (err == MFX_WRN_DEVICE_BUSY);

    if (err < 0 || !sync) {
        av_log(ctx, AV_LOG_ERROR, "Error uploading the surface\n");
        return AVERROR_UNKNOWN;
    }

    do {
        err = MFXVideoCORE_SyncOperation(s->session_upload, sync, 1000);
    } while (err == MFX_WRN_IN_EXECUTION);
    if (err < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error synchronizing the operation\n");
        return AVERROR_UNKNOWN;
    }

    return 0;
}

static int qsv_frames_get_constraints(AVHWDeviceContext *ctx,
                                      const void *hwconfig,
                                      AVHWFramesConstraints *constraints)
{
    int i;

    constraints->valid_sw_formats = av_malloc_array(FF_ARRAY_ELEMS(supported_pixel_formats) + 1,
                                                    sizeof(*constraints->valid_sw_formats));
    if (!constraints->valid_sw_formats)
        return AVERROR(ENOMEM);

    for (i = 0; i < FF_ARRAY_ELEMS(supported_pixel_formats); i++)
        constraints->valid_sw_formats[i] = supported_pixel_formats[i].pix_fmt;
    constraints->valid_sw_formats[FF_ARRAY_ELEMS(supported_pixel_formats)] = AV_PIX_FMT_NONE;

    constraints->valid_hw_formats = av_malloc_array(2, sizeof(*constraints->valid_hw_formats));
    if (!constraints->valid_hw_formats)
        return AVERROR(ENOMEM);

    constraints->valid_hw_formats[0] = AV_PIX_FMT_QSV;
    constraints->valid_hw_formats[1] = AV_PIX_FMT_NONE;

    return 0;
}

static void qsv_device_free(AVHWDeviceContext *ctx)
{
    AVQSVDeviceContext *hwctx = ctx->hwctx;
    QSVDevicePriv       *priv = ctx->user_opaque;

    if (hwctx->session)
        MFXClose(hwctx->session);

    av_buffer_unref(&priv->child_device_ctx);
    av_freep(&priv);
}

static mfxIMPL choose_implementation(const char *device)
{
    static const struct {
        const char *name;
        mfxIMPL     impl;
    } impl_map[] = {
        { "auto",     MFX_IMPL_AUTO         },
        { "sw",       MFX_IMPL_SOFTWARE     },
        { "hw",       MFX_IMPL_HARDWARE     },
        { "auto_any", MFX_IMPL_AUTO_ANY     },
        { "hw_any",   MFX_IMPL_HARDWARE_ANY },
        { "hw2",      MFX_IMPL_HARDWARE2    },
        { "hw3",      MFX_IMPL_HARDWARE3    },
        { "hw4",      MFX_IMPL_HARDWARE4    },
    };

    mfxIMPL impl = MFX_IMPL_AUTO_ANY;
    int i;

    if (device) {
        for (i = 0; i < FF_ARRAY_ELEMS(impl_map); i++)
            if (!strcmp(device, impl_map[i].name)) {
                impl = impl_map[i].impl;
                break;
            }
        if (i == FF_ARRAY_ELEMS(impl_map))
            impl = strtol(device, NULL, 0);
    }

    return impl;
}

static int create_proper_child_device(AVBufferRef **ctx, const char *device, int flags)
{
    enum AVHWDeviceType child_device_type;
    char adapter[256];
    int  adapter_num;

    if (CONFIG_VAAPI)
        child_device_type = AV_HWDEVICE_TYPE_VAAPI;
    else if (CONFIG_DXVA2)
        child_device_type = AV_HWDEVICE_TYPE_DXVA2;
    else
        return AVERROR(ENOSYS);

    if (device || CONFIG_DXVA2)
        return av_hwdevice_ctx_create(ctx, child_device_type, device, NULL, flags);

    for (adapter_num = 0; adapter_num < 6; adapter_num++) {
        if (adapter_num < 3)
            snprintf(adapter,sizeof(adapter),
                "/dev/dri/renderD%d", adapter_num+128);
        else
            snprintf(adapter,sizeof(adapter),
                "/dev/dri/card%d", adapter_num-3);
        if (av_hwdevice_ctx_create(ctx, child_device_type, adapter, NULL, flags) == 0)
            return 0;
    }

    return AVERROR(ENOSYS);
}

static int qsv_device_create(AVHWDeviceContext *ctx, const char *device,
                             AVDictionary *opts, int flags)
{
    AVQSVDeviceContext *hwctx = ctx->hwctx;
    QSVDevicePriv *priv;
    AVDictionaryEntry *e;

    mfxVersion    ver = { { 3, 1 } };
    mfxIMPL       impl;
    mfxHDL        handle;
    mfxHandleType handle_type;
    mfxStatus     err;
    int ret;

    priv = av_mallocz(sizeof(*priv));
    if (!priv)
        return AVERROR(ENOMEM);

    ctx->user_opaque = priv;
    ctx->free        = qsv_device_free;

    e = av_dict_get(opts, "child_device", NULL, 0);
    ret = create_proper_child_device(&priv->child_device_ctx, e ? e->value : NULL, 0);
    if (ret < 0)
        return ret;

    {
        AVHWDeviceContext      *child_device_ctx = (AVHWDeviceContext*)priv->child_device_ctx->data;
#if CONFIG_VAAPI
        AVVAAPIDeviceContext *child_device_hwctx = child_device_ctx->hwctx;
        handle_type = MFX_HANDLE_VA_DISPLAY;
        handle = (mfxHDL)child_device_hwctx->display;
#elif CONFIG_DXVA2
        AVDXVA2DeviceContext *child_device_hwctx = child_device_ctx->hwctx;
        handle_type = MFX_HANDLE_D3D9_DEVICE_MANAGER;
        handle = (mfxHDL)child_device_hwctx->devmgr;
#endif
    }

    impl = choose_implementation(device);

    err = MFXInit(impl, &ver, &hwctx->session);
    if (err != MFX_ERR_NONE) {
        av_log(ctx, AV_LOG_ERROR, "Error initializing an MFX session\n");
        return AVERROR_UNKNOWN;
    }

    err = MFXVideoCORE_SetHandle(hwctx->session, handle_type, handle);
    if (err != MFX_ERR_NONE)
        return AVERROR_UNKNOWN;

    return 0;
}

const HWContextType ff_hwcontext_type_qsv = {
    .type                   = AV_HWDEVICE_TYPE_QSV,
    .name                   = "QSV",

    .device_hwctx_size      = sizeof(AVQSVDeviceContext),
    .device_priv_size       = sizeof(QSVDeviceContext),
    .frames_hwctx_size      = sizeof(AVQSVFramesContext),
    .frames_priv_size       = sizeof(QSVFramesContext),

    .device_create          = qsv_device_create,
    .device_init            = qsv_device_init,
    .frames_get_constraints = qsv_frames_get_constraints,
    .frames_init            = qsv_frames_init,
    .frames_uninit          = qsv_frames_uninit,
    .frames_get_buffer      = qsv_get_buffer,
    .transfer_get_formats   = qsv_transfer_get_formats,
    .transfer_data_to       = qsv_transfer_data_to,
    .transfer_data_from     = qsv_transfer_data_from,

    .pix_fmts = (const enum AVPixelFormat[]){ AV_PIX_FMT_QSV, AV_PIX_FMT_NONE },
};
