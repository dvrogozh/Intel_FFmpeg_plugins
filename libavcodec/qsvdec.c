/*
 * Intel MediaSDK QSV codec-independent code
 *
 * copyright (c) 2013 Luca Barbato
 * copyright (c) 2015 Anton Khirnov <anton@khirnov.net>
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

#include <string.h>
#include <sys/types.h>

#include <mfx/mfxvideo.h>

#include "libavutil/common.h"
#include "libavutil/mem.h"
#include "libavutil/log.h"
#include "libavutil/pixfmt.h"
#include "libavutil/time.h"

#include "avcodec.h"
#include "internal.h"
#include "qsv.h"
#include "qsv_internal.h"
#include "qsvenc.h"
#include "qsvdec.h"
#include "vaapi_allocator.h"

int ff_qsv_map_pixfmt(enum AVPixelFormat format)
{
    switch (format) {
    case AV_PIX_FMT_YUV420P:
    case AV_PIX_FMT_YUVJ420P:
        return AV_PIX_FMT_NV12;
    default:
        return AVERROR(ENOSYS);
    }
}

static void free_surfaces(QSVContext *q, mfxFrameAllocResponse *resp)
{
    //surface will be freed when decoder closed
    QSVFrame *cur = NULL;
    if(q->work_frames){
        cur = q->work_frames;
        free(cur->surface);
        while(cur){
            av_frame_free(&cur->frame);
            cur = cur->next;
        }
        free( q->work_frames );
        q->work_frames = NULL;
    }
	
	q->nb_surfaces = 0;
}

static int alloc_frame(AVCodecContext *avctx, QSVFrame *frame)
{
    int ret;

    /*GPUCopy need that width aligned with 128 and height aligned with 64*/
//    ret = ff_get_buffer(avctx, frame->frame, AV_GET_BUFFER_FLAG_REF);
    frame->frame->width  = avctx->width;
    frame->frame->height = FFALIGN(avctx->height, 64);
    frame->frame->format = avctx->pix_fmt;
    ret = av_frame_get_buffer(frame->frame, 128);
    if (ret < 0)
        return ret;
    frame->frame->height = avctx->height;

    if (frame->frame->format == AV_PIX_FMT_QSV) {
        frame->surface = (mfxFrameSurface1*)frame->frame->data[3];
	    av_log(NULL, AV_LOG_DEBUG, "Pixel format is AV_PIX_FMT_QSV\n" );
    } else {
        frame->surface_internal.Data.PitchLow = frame->frame->linesize[0];
        frame->surface_internal.Data.Y        = frame->frame->data[0];
        frame->surface_internal.Data.UV       = frame->frame->data[1];
    }

    return 0;
}

static int codec_connect( QSVContext* qsv_dec_ctx, AVCodecContext* av_dec_ctx, AVCodecContext* av_enc_ctx, int vpp_type )
{
	int ret = 0;
    
    VADisplay va_dpy;
    QSVEncContext* qsv_enc_ctx = NULL;
    QSVContext* q =  qsv_dec_ctx;

	if( (NULL == av_dec_ctx ) || (NULL == av_enc_ctx) || (NULL == qsv_dec_ctx) )
		return -1;
   
	if(NULL == av_enc_ctx->priv_data)
		return ret;

	if( AVFILTER_MORE == vpp_type ) return ret;

	av_log(NULL, AV_LOG_INFO, "source: width = %d, height=%d \n output: width = %d, height = %d\n",
			                 av_dec_ctx->width, av_dec_ctx->height,
							 av_enc_ctx->width, av_enc_ctx->height);
   	if( strcmp(av_enc_ctx->codec->name, "h264_qsv")==0 ){
     	QSVH264EncContext* h264_enc_ctx;
     	h264_enc_ctx = (QSVH264EncContext*) av_enc_ctx->priv_data;
		qsv_enc_ctx = &(h264_enc_ctx->qsv);
	}

   	if( strcmp(av_enc_ctx->codec->name, "mpeg2_qsv")==0 ){
     	QSVMpeg2EncContext* mpeg2_enc_ctx;
     	mpeg2_enc_ctx = (QSVMpeg2EncContext*) av_enc_ctx->priv_data;
		qsv_enc_ctx = &(mpeg2_enc_ctx->qsv);
	}

  	if( strcmp(av_enc_ctx->codec->name, "hevc_qsv")==0 ){
     	QSVHEVCEncContext* hevc_enc_ctx;
     	hevc_enc_ctx = (QSVHEVCEncContext*) av_enc_ctx->priv_data;
		qsv_enc_ctx = &(hevc_enc_ctx->qsv);
	}

	if( strcmp(av_enc_ctx->codec->name, "mjpeg_qsv")==0 ){
	QSVMJPEGEncContext* mjpeg_enc_ctx;
	mjpeg_enc_ctx = (QSVMJPEGEncContext*) av_enc_ctx->priv_data;
		qsv_enc_ctx = &(mjpeg_enc_ctx->qsv);
	}

    q->enc_ctx = qsv_enc_ctx;

	//video memory is used when decoder and encoder are all supported with HW
	if( NULL != qsv_enc_ctx ){

        q->iopattern = MFX_IOPATTERN_OUT_VIDEO_MEMORY;
		ret = MFXVideoCORE_GetHandle(q->session, MFX_HANDLE_VA_DISPLAY, (mfxHDL*)&va_dpy);
		
		q->frame_allocator.pthis = q;
		q->frame_allocator.Alloc = ff_qsv_frame_alloc;
		q->frame_allocator.Lock = ff_qsv_frame_lock;
		q->frame_allocator.Unlock = ff_qsv_frame_unlock;
		q->frame_allocator.GetHDL = ff_qsv_frame_get_hdl;
		q->frame_allocator.Free = ff_qsv_frame_free;
		
		MFXVideoCORE_SetFrameAllocator( q->session, &q->frame_allocator );
		av_log(NULL, AV_LOG_INFO, "DECODE: session=%p SetFrameAllocator dpy=%p\n",q->session, va_dpy);

     	qsv_enc_ctx->session = q->session;
        qsv_enc_ctx->iopattern = q->iopattern;
	}

	return ret;
}

int av_qsv_pipeline_insert_vpp( AVCodecContext *av_dec_ctx, AVFilterContext* vpp_ctx )
{
    VPPContext *vpp = NULL;
	QSVContext *qsv = NULL;

	if(NULL == vpp_ctx) return 0;

    vpp = (VPPContext*) vpp_ctx->priv;
    
	if( (strcmp(av_dec_ctx->codec->name, "h264_qsv")==0) ||  (strcmp(av_dec_ctx->codec->name, "hevc_qsv")==0 ) ){
		QSVH2645Context* qsv_ctx = (QSVH2645Context*) av_dec_ctx->priv_data;
		qsv = &(qsv_ctx->qsv);//.pCodecConnect(&(qsv_ctx->qsv), av_dec_ctx, av_enc_ctx);
    }
        
	if( strcmp(av_dec_ctx->codec->name, "mpeg2_qsv")==0 ){
		QSVMPEG2Context* qsv_ctx = (QSVMPEG2Context*) av_dec_ctx->priv_data;
		qsv = &(qsv_ctx->qsv);//.pCodecConnect(&(qsv_ctx->qsv), av_dec_ctx, av_enc_ctx);
    }
        
	if( strcmp(av_dec_ctx->codec->name, "vc1_qsv")==0 )
    {
    	QSVVC1Context* qsv_ctx = (QSVVC1Context*) av_dec_ctx->priv_data;
		qsv = &(qsv_ctx->qsv);//.pCodecConnect(&(qsv_ctx->qsv), av_dec_ctx, av_enc_ctx);
    }

	if( strcmp(av_dec_ctx->codec->name, "mjpeg_qsv")==0 )
    {
	QSVMJPEGContext* qsv_ctx = (QSVMJPEGContext*) av_dec_ctx->priv_data;
		qsv = &(qsv_ctx->qsv);//.pCodecConnect(&(qsv_ctx->qsv), av_dec_ctx, av_enc_ctx);
    }

    if ( strcmp(av_dec_ctx->codec->name, "vp8_qsv")==0 )
    {
        QSVVP8Context* qsv_ctx = (QSVVP8Context*) av_dec_ctx->priv_data;
        qsv = &(qsv_ctx->qsv);
    }

	if(NULL == qsv->enc_ctx) return 0;
	qsv->vpp = vpp;
	vpp->session = qsv->session;
	vpp->pFrameAllocator = &(qsv->frame_allocator);
	vpp->enc_ctx = qsv->enc_ctx;

	return 0;
}

int av_qsv_pipeline_connect_codec( AVCodecContext *av_dec_ctx, AVCodecContext *av_enc_ctx, int vpp_type )
{
    QSVContext *qsv = NULL;
    if( (strcmp(av_dec_ctx->codec->name, "h264_qsv")==0) ||  (strcmp(av_dec_ctx->codec->name, "hevc_qsv")==0 ) ){
		QSVH2645Context* qsv_ctx = (QSVH2645Context*) av_dec_ctx->priv_data;
		qsv = &(qsv_ctx->qsv);//.pCodecConnect(&(qsv_ctx->qsv), av_dec_ctx, av_enc_ctx);
    }
        
    if( strcmp(av_dec_ctx->codec->name, "mjpeg_qsv") ==0 ) {
		QSVMJPEGContext* qsv_ctx = (QSVMJPEGContext*) av_dec_ctx->priv_data;
		qsv = &(qsv_ctx->qsv);//.pCodecConnect(&(qsv_ctx->qsv), av_dec_ctx, av_enc_ctx);
    }

	if( strcmp(av_dec_ctx->codec->name, "mpeg2_qsv")==0 ){
		QSVMPEG2Context* qsv_ctx = (QSVMPEG2Context*) av_dec_ctx->priv_data;
		qsv = &(qsv_ctx->qsv);//.pCodecConnect(&(qsv_ctx->qsv), av_dec_ctx, av_enc_ctx);
    }
        
	if( strcmp(av_dec_ctx->codec->name, "vc1_qsv")==0 )
    {
    	QSVVC1Context* qsv_ctx = (QSVVC1Context*) av_dec_ctx->priv_data;
		qsv = &(qsv_ctx->qsv);//.pCodecConnect(&(qsv_ctx->qsv), av_dec_ctx, av_enc_ctx);
    }

    if ( strcmp(av_dec_ctx->codec->name, "vp8_qsv") ==0 ) {
        QSVVP8Context* qsv_ctx = (QSVVP8Context*) av_dec_ctx->priv_data;
        qsv = &(qsv_ctx->qsv);
    }
    
	if(NULL == qsv) return -1;

	return codec_connect(qsv, av_dec_ctx, av_enc_ctx, vpp_type);
}

int ff_qsv_decode_init_session(AVCodecContext *avctx, QSVContext *q)
{
	int ret = 0;
    av_log(avctx, AV_LOG_INFO, "DECODE: ff_qsv_decode_init_session\n");
    if (!q->session) {
        av_log(avctx, AV_LOG_DEBUG, "QSVDEC: GPUCopy %s.\n",
                q->internal_qs.gpu_copy == MFX_GPUCOPY_ON ? "enabled":"disabled");
		ret = ff_qsv_init_internal_session(avctx, &q->internal_qs);
		if (ret < 0){
			av_log(avctx, AV_LOG_ERROR, "ff_qsv_init_internal_session failed\n");		
            return ret;
		}

        q->session = q->internal_qs.session;			
    }

    if (q->load_plugins) {
        ret = ff_qsv_load_plugins(q->session, q->load_plugins);
        if (ret < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to load plugins %s, ret = %s\n",
                    q->load_plugins, av_err2str(ret));
            return ret;
        }
    }

	//q->pCodecConnect = codec_connect;
    q->enc_ctx = NULL;
	q->vpp = NULL;

	return ret;
}

static int qsv_decode_init_sysmem(AVCodecContext *avctx, QSVContext *q, mfxBitstream *bs)
{
    mfxVideoParam param = { 0 };
    int ret, idx = 0;
    enum AVPixelFormat pix_fmts[3] = { AV_PIX_FMT_QSV,
                                       AV_PIX_FMT_NV12,
                                       AV_PIX_FMT_NONE };
    QSVFrame *cur = NULL, *first = NULL;

    av_log(avctx, AV_LOG_INFO,"*******ff_qsv_decode_init_sysmem**********\n");
    q->iopattern  = MFX_IOPATTERN_OUT_SYSTEM_MEMORY;

    if (!bs || !bs->DataLength)
        return AVERROR_INVALIDDATA;

    ret = ff_qsv_codec_id_to_mfx(avctx->codec_id);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Unsupported codec_id %08x\n", avctx->codec_id);
        return ret;
    }

    param.mfx.CodecId = ret;

    ret = MFXVideoDECODE_DecodeHeader(q->session, bs, &param);
    if (MFX_ERR_MORE_DATA==ret) {
        /* this code means that header not found so we return packet size to skip
           a current packet
         */
        return bs->DataOffset + bs->DataLength;
    } else if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Decode header error %d\n", ret);
        return ff_qsv_error(ret);
    }
    param.IOPattern   = q->iopattern;
    param.AsyncDepth  = q->async_depth;
    param.ExtParam    = q->ext_buffers;
    param.NumExtParam = q->nb_ext_buffers;
    param.mfx.FrameInfo.BitDepthLuma   = 8;
    param.mfx.FrameInfo.BitDepthChroma = 8;

    if(!q->request)
        q->request = av_mallocz(sizeof(mfxFrameAllocRequest));

    ret = MFXVideoDECODE_QueryIOSurf( q->session, &param, q->request );
    if( ret < 0 ){
        av_log(avctx, AV_LOG_ERROR, "QueryIOSurf failed with return %d\n", ret);
        return ff_qsv_error( ret );
    }

	av_log(avctx, AV_LOG_INFO, "DECODE: QueryIOSurf ret=%d W=%d H=%d FourCC=%08x "
            "NumFrameSuggested=%d\n", ret, q->request->Info.Width, q->request->Info.Height,
            q->request->Info.FourCC, q->request->NumFrameSuggested);

    while(idx++ < q->request->NumFrameSuggested){
        cur = av_mallocz(sizeof(*cur));
        cur->surface = &cur->surface_internal;
        memcpy(&cur->surface->Info, &param.mfx.FrameInfo, sizeof(cur->surface->Info));
        cur->frame = av_frame_alloc();
        cur->next = first;
        first = cur;
    }
    q->work_frames = first;

    ret = MFXVideoDECODE_Init(q->session, &param);
    if (ret < 0) {
        if (MFX_ERR_INVALID_VIDEO_PARAM==ret) {
            av_log(avctx, AV_LOG_ERROR,
                   "Error initializing the MFX video decoder, unsupported video\n");
        } else {
            av_log(avctx, AV_LOG_ERROR,
                   "Error initializing the MFX video decoder %d\n", ret);
        }
        return ff_qsv_error(ret);
    }

    ret = ff_get_format(avctx, pix_fmts);
    if (ret < 0)
        return ret;

    avctx->pix_fmt      = ret;
    avctx->profile      = param.mfx.CodecProfile;
    avctx->level        = param.mfx.CodecLevel;
    avctx->coded_width  = param.mfx.FrameInfo.Width;
    avctx->coded_height = param.mfx.FrameInfo.Height;
    avctx->width        = param.mfx.FrameInfo.CropW - param.mfx.FrameInfo.CropX;
    avctx->height       = param.mfx.FrameInfo.CropH - param.mfx.FrameInfo.CropY;
    avctx->framerate.num= param.mfx.FrameInfo.FrameRateExtN;
    avctx->framerate.den= param.mfx.FrameInfo.FrameRateExtD;
    avctx->time_base.num= param.mfx.FrameInfo.FrameRateExtD;
    avctx->time_base.den= param.mfx.FrameInfo.FrameRateExtN;
    avctx->sample_aspect_ratio.num = param.mfx.FrameInfo.AspectRatioW;
    avctx->sample_aspect_ratio.den = param.mfx.FrameInfo.AspectRatioH;
    if (avctx->codec_id == AV_CODEC_ID_H264)
        avctx->ticks_per_frame = 2;

    /* maximum decoder latency should be not exceed max DPB size for h.264 and
       HEVC which is 16 for both cases.
       So weare  pre-allocating fifo big enough for 17 elements:
     */
    if (!q->async_fifo) {
        q->async_fifo = av_fifo_alloc((1 + 16) * sizeof(QSVFrame*));
        if (!q->async_fifo)
            return AVERROR(ENOMEM);
    }

    if (!q->input_fifo) {
        q->input_fifo = av_fifo_alloc(1024*16);
        if (!q->input_fifo)
            return AVERROR(ENOMEM);
    }

    if (!q->pkt_fifo) {
        q->pkt_fifo = av_fifo_alloc( sizeof(AVPacket) * (1 + 16) );
        if (!q->pkt_fifo)
            return AVERROR(ENOMEM);
    }
    q->engine_ready = 1;

    return 0;
}

static int qsv_decode_init_vidmem(AVCodecContext *avctx, QSVContext *q, mfxBitstream *bs)
{
    mfxVideoParam param = { 0 };
    int ret;
    enum AVPixelFormat pix_fmts[3] = { AV_PIX_FMT_QSV,
                                       AV_PIX_FMT_NV12,
                                       AV_PIX_FMT_NONE };

    av_log(avctx, AV_LOG_INFO,"*******ff_qsv_decode_init_vidmem**********\n");
    q->iopattern = MFX_IOPATTERN_OUT_VIDEO_MEMORY;

    if (!bs || !bs->DataLength)
        return AVERROR_INVALIDDATA;

    ret = ff_qsv_codec_id_to_mfx(avctx->codec_id);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Unsupported codec_id %08x\n", avctx->codec_id);
        return ret;
    }

    param.mfx.CodecId = ret;

    ret = MFXVideoDECODE_DecodeHeader(q->session, bs, &param);
    if (MFX_ERR_MORE_DATA==ret) {
        /* this code means that header not found so we return packet size to skip
           a current packet
         */
        return bs->DataOffset + bs->DataLength;
    } else if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Decode header error %d\n", ret);
        return ff_qsv_error(ret);
    }
    av_log(avctx, AV_LOG_INFO, "DECODE: Decode Header\n");
	
    param.IOPattern   = q->iopattern;
    param.AsyncDepth  = q->async_depth;
    param.ExtParam    = q->ext_buffers;
    param.NumExtParam = q->nb_ext_buffers;
    param.mfx.FrameInfo.BitDepthLuma   = 8;
    param.mfx.FrameInfo.BitDepthChroma = 8;
    
    //Added by GY: alloc surfaces for decoder
    if(!q->request)
        q->request = av_mallocz(sizeof(mfxFrameAllocRequest));
    
    if(!q->response)
        q->response = av_mallocz(sizeof(mfxFrameAllocResponse));

    ret = MFXVideoDECODE_QueryIOSurf( q->session, &param, q->request );
    if( ret < 0 ){
        av_log(avctx, AV_LOG_ERROR, "QueryIOSurf failed with return %d\n", ret);
        return ff_qsv_error( ret );
    }

	av_log(avctx, AV_LOG_INFO, "DECODE: QueryIOSurf ret=%d W=%d H=%d FourCC=%08x "
            "NumFrameSuggested=%d, vpp->NumFrameSuggested=%d\n",
            ret, q->request->Info.Width, q->request->Info.Height,
            q->request->Info.FourCC, q->request->NumFrameSuggested,
            q->vpp ? q->vpp->inter_vpp[0].req[0].NumFrameSuggested : -1);

	if( NULL != q->enc_ctx ){
		if( NULL == q->vpp ){
			q->request->NumFrameSuggested += q->enc_ctx->req.NumFrameSuggested;
	    	q->request->NumFrameMin += q->enc_ctx->req.NumFrameSuggested;
		}else{
			q->request->NumFrameSuggested += q->vpp->inter_vpp[0].req[0].NumFrameSuggested;
	    	q->request->NumFrameMin += q->vpp->inter_vpp[0].req[0].NumFrameSuggested;
		}
			
	}

    av_log(avctx, AV_LOG_INFO, "DECODE: start  DECODE_Init\n");
   // q->frame_allocator.Alloc( q, q->request, q->response);
    ret = MFXVideoDECODE_Init(q->session, &param);
    if (ret < 0) {
        if (MFX_ERR_INVALID_VIDEO_PARAM==ret) {
            av_log(avctx, AV_LOG_ERROR,
                   "Error initializing the MFX video decoder, unsupported video\n");
        } else {
            av_log(avctx, AV_LOG_ERROR,
                   "Error initializing the MFX video decoder %d\n", ret);
        }
        return ff_qsv_error(ret);
    }
    av_log(avctx, AV_LOG_INFO, "DECODE: DECODE_Init ret=%d\n",ret);

    ret = ff_get_format(avctx, pix_fmts);
    if (ret < 0)
        return ret;

    avctx->pix_fmt      = ret;
    avctx->profile      = param.mfx.CodecProfile;
    avctx->level        = param.mfx.CodecLevel;
    avctx->coded_width  = param.mfx.FrameInfo.Width;
    avctx->coded_height = param.mfx.FrameInfo.Height;
    avctx->width        = param.mfx.FrameInfo.CropW - param.mfx.FrameInfo.CropX;
    avctx->height       = param.mfx.FrameInfo.CropH - param.mfx.FrameInfo.CropY;
    avctx->framerate.num= param.mfx.FrameInfo.FrameRateExtN;
    avctx->framerate.den= param.mfx.FrameInfo.FrameRateExtD;
    avctx->time_base.num= param.mfx.FrameInfo.FrameRateExtD;
    avctx->time_base.den= param.mfx.FrameInfo.FrameRateExtN;
    avctx->sample_aspect_ratio.num = param.mfx.FrameInfo.AspectRatioW;
    avctx->sample_aspect_ratio.den = param.mfx.FrameInfo.AspectRatioH;
    if (avctx->codec_id == AV_CODEC_ID_H264)
        avctx->ticks_per_frame = 2;

    //set info to enc
	if( NULL != q->enc_ctx )
		q->enc_ctx->work_frames = q->work_frames;

    /* maximum decoder latency should be not exceed max DPB size for h.264 and
       HEVC which is 16 for both cases.
       So weare  pre-allocating fifo big enough for 17 elements:
     */
    if (!q->async_fifo) {
        q->async_fifo = av_fifo_alloc((1 + 16) * sizeof(QSVFrame*));
        if (!q->async_fifo)
            return AVERROR(ENOMEM);
    }

    if (!q->input_fifo) {
        q->input_fifo = av_fifo_alloc(1024*16);
        if (!q->input_fifo)
            return AVERROR(ENOMEM);
    }

    if (!q->pkt_fifo) {
        q->pkt_fifo = av_fifo_alloc( sizeof(AVPacket) * (1 + 16) );
        if (!q->pkt_fifo)
            return AVERROR(ENOMEM);
    }
    q->engine_ready = 1;

	av_log(avctx, AV_LOG_INFO, "qsv_decode_init_vidmem is done\n");

    return 0;
}

static int qsv_decode_init(AVCodecContext *avctx, QSVContext *q, mfxBitstream *bs)
{
    if (NULL != q->enc_ctx)
        return qsv_decode_init_vidmem(avctx, q, bs);

    return qsv_decode_init_sysmem(avctx, q, bs);
}

static int get_surface(AVCodecContext *avctx, QSVContext *q, mfxFrameSurface1 **surf)
{
    QSVFrame *frame;
    int ret;

    frame = q->work_frames;
    while (frame) {
        /*av_log(avctx, AV_LOG_DEBUG, "frame status: locked = %d, queued = %d.\n",
                frame->surface->Data.Locked, frame->queued);*/
        if (!frame->surface->Data.Locked && !frame->queued) {
            av_frame_unref(frame->frame);
            ret = alloc_frame(avctx, frame);
            if (ret < 0)
                return ret;
            *surf = frame->surface;
            frame->surface->Data.NumExtParam = 1;
            frame->surface->Data.ExtParam = frame->ext_buf;
            frame->ext_buf[0] = (mfxExtBuffer *)&frame->dec_info;
            frame->dec_info.Header.BufferId = MFX_EXTBUFF_DECODED_FRAME_INFO;
            frame->dec_info.Header.BufferSz = sizeof(frame->dec_info);
            return 0;
        }

        frame = frame->next;
    }

    return -1;
}

static int get_free_surface(AVCodecContext *avctx, QSVContext *q, mfxFrameSurface1 **surf)
{
   	QSVFrame *cur_frames = NULL;
	do {
        *surf = NULL;
	
		cur_frames = q->work_frames;
		while(NULL != cur_frames){
			if( !(cur_frames->surface->Data.Locked) && !cur_frames->queued ){
                *surf = (cur_frames->surface);
				break;
			}
			cur_frames = cur_frames->next;
		}

        if (*surf) {
            cur_frames->surface->Data.NumExtParam = 1;
            cur_frames->surface->Data.ExtParam = cur_frames->ext_buf;
            cur_frames->ext_buf[0] = (mfxExtBuffer *)&cur_frames->dec_info;
            cur_frames->dec_info.Header.BufferId = MFX_EXTBUFF_DECODED_FRAME_INFO;
            cur_frames->dec_info.Header.BufferSz = sizeof(cur_frames->dec_info);
			break;
		} else {
			av_log( avctx, AV_LOG_ERROR, "waiting until there are free surface\n" );
			av_usleep(1000);
		}

    } while(1);

    return 0;
}

static int find_free_surface(AVCodecContext *avctx, QSVContext *q, mfxFrameSurface1 **surf)
{
    int ret = 0;
    switch(q->iopattern){
	case MFX_IOPATTERN_OUT_VIDEO_MEMORY:
	     ret = get_free_surface(avctx, q, surf);
	     break;
	case MFX_IOPATTERN_OUT_SYSTEM_MEMORY:
	     default:
	     ret = get_surface(avctx, q, surf);
	     break;
    }

    return ret;
}

static int do_sync_operation(AVCodecContext *avctx, QSVContext *q, AVFrame *frame, int* got_frame)
{
    int ret = 0;
    int n_out_frames = 0;
    QSVFrame *out_frame = NULL;
    n_out_frames = av_fifo_size(q->async_fifo) / sizeof(out_frame);

    if( n_out_frames != 0 ) {
	AVFrame *src_frame;
	av_fifo_generic_read(q->async_fifo, &(out_frame), sizeof(out_frame), NULL);
	out_frame->queued = 0;
    out_frame->surface->Data.NumExtParam = 0;
    out_frame->surface->Data.ExtParam = NULL;

        MFXVideoCORE_SyncOperation(q->session, out_frame->sync_point, 60000);

	switch(q->iopattern){
            case MFX_IOPATTERN_OUT_VIDEO_MEMORY:
                frame->data[3] = (void*) out_frame->surface;
                break;
            case MFX_IOPATTERN_OUT_SYSTEM_MEMORY:
            default:
                src_frame = out_frame->frame;
                ret = av_frame_ref(frame, src_frame);
                if( ret < 0 ) return ret;
	}

        frame->width       = avctx->width;
        frame->height      = avctx->height;
        frame->format      = avctx->pix_fmt;
        frame->pkt_pts     = frame->pts = out_frame->surface->Data.TimeStamp;
        frame->repeat_pict =
            out_frame->surface->Info.PicStruct & MFX_PICSTRUCT_FRAME_TRIPLING ? 4 :
            out_frame->surface->Info.PicStruct & MFX_PICSTRUCT_FRAME_DOUBLING ? 2 :
            out_frame->surface->Info.PicStruct & MFX_PICSTRUCT_FIELD_REPEATED ? 1 : 0;
        frame->top_field_first =
            out_frame->surface->Info.PicStruct & MFX_PICSTRUCT_FIELD_TFF;
        frame->interlaced_frame =
            !(out_frame->surface->Info.PicStruct & MFX_PICSTRUCT_PROGRESSIVE);
        frame->pict_type = ff_qsv_map_pictype(out_frame->dec_info.FrameType);
        frame->key_frame = !!(out_frame->dec_info.FrameType & MFX_FRAMETYPE_IDR);

        *got_frame = 1;
    }
    return ret;
}
static QSVFrame *find_frame(QSVContext *q, mfxFrameSurface1 *surf)
{
    QSVFrame *cur;
	cur = q->work_frames;
   
	while (cur) {
        	if (surf == cur->surface)
            	return cur;
        	cur = cur->next;
    	}

    return NULL;
}

/*  This function uses for 'smart' releasing of consumed data
    from the input bitstream fifo.
    Since the input fifo mapped to mfxBitstream which does not understand
    a wrapping of data over fifo end, we should also to relocate a possible
    data rest to fifo begin. If rest of data is absent then we just reset fifo's
    pointers to initial positions.
    NOTE the case when fifo does contain unconsumed data is rare and typical
    amount of such data is 1..4 bytes.
*/
static void qsv_fifo_relocate(AVFifoBuffer *f, int bytes_to_free)
{
    int data_size;
    int data_rest = 0;

    av_fifo_drain(f, bytes_to_free);

    data_size = av_fifo_size(f);
    if (data_size > 0) {
        if (f->buffer!=f->rptr) {
            if ( (f->end - f->rptr) < data_size) {
                data_rest = data_size - (f->end - f->rptr);
                data_size-=data_rest;
                memmove(f->buffer+data_size, f->buffer, data_rest);
            }
            memmove(f->buffer, f->rptr, data_size);
            data_size+= data_rest;
        }
    }
    f->rptr = f->buffer;
    f->wptr = f->buffer + data_size;
    f->wndx = data_size;
    f->rndx = 0;
}

static void close_decoder_vidmem(QSVContext *q)
{
    free_surfaces( q, q->response );
	
    MFXVideoDECODE_Close(q->session);

    q->engine_ready   = 0;
    q->reinit_pending = 0;

    //av_log(NULL, AV_LOG_INFO,"close decoder done!\n");
}

static void close_decoder_sysmem(QSVContext *q)
{
    QSVFrame *cur;

    MFXVideoDECODE_Close(q->session);

    cur = q->work_frames;
    while (cur) {
        q->work_frames = cur->next;
        av_frame_free(&cur->frame);
        av_freep(&cur);
        cur = q->work_frames;
    }

    q->engine_ready   = 0;
    q->reinit_pending = 0;
}


static void close_decoder(QSVContext *q)
{
	switch(q->iopattern){
		case MFX_IOPATTERN_OUT_VIDEO_MEMORY:
			close_decoder_vidmem(q);
			break;
		case MFX_IOPATTERN_OUT_SYSTEM_MEMORY:			
		default:
			close_decoder_sysmem(q);
			break;
	}
}

static int do_decode_frame_async(AVCodecContext *avctx, QSVContext *q, int flush, mfxBitstream *bs)
{
    QSVFrame *out_frame = NULL;
    mfxFrameSurface1* insurface = NULL;
    mfxFrameSurface1* outsurface = NULL;
    mfxSyncPoint sync = NULL;
    int ret =0;

    while (1) {
        ret = find_free_surface(avctx, q, &insurface);
        if (ret < 0 || !insurface) {
            av_log(avctx, AV_LOG_DEBUG, "get_surface() failed.\n");
            ret = 0;
            break;
        }

        do {
            ret = MFXVideoDECODE_DecodeFrameAsync(q->session, flush ? NULL : bs,
                                                  insurface, &(outsurface), &(sync));
            if (ret != MFX_WRN_DEVICE_BUSY)
                break;
            av_usleep(500);
        } while (1);

        if (MFX_WRN_VIDEO_PARAM_CHANGED==ret) {
        /* TODO: handle here minor sequence header changing */
        } else if (MFX_ERR_INCOMPATIBLE_VIDEO_PARAM == ret) {
            flush = q->reinit_pending = 1;
        } else if (MFX_ERR_UNDEFINED_BEHAVIOR == ret)
            ff_qsv_decode_reset(avctx, q);

        if (sync) {
            out_frame = find_frame( q, outsurface );
            if (!out_frame) {
                av_log(avctx, AV_LOG_ERROR,"The returned surface does not correspond to any frame\n");
                return AVERROR_BUG;
            }

            out_frame->queued = 1;
            out_frame->sync_point = sync;
            av_fifo_generic_write(q->async_fifo, &(out_frame), sizeof(out_frame), NULL);
        }

        if (MFX_ERR_MORE_SURFACE != ret && ret < 0)
            break;
    }

    return ret;
}

static int do_qsv_decode(AVCodecContext *avctx, QSVContext *q,
                  AVFrame *frame, int *got_frame,
                  AVPacket *avpkt)
{
    mfxBitstream bs = { { { 0 } } };
    int ret;
    int buffered = 0;
    int flush = 0;
    int rest_data = 0;

    if (q->input_fifo)
        rest_data = av_fifo_size(q->input_fifo);

    /* do flush must after consume all useful data in the fifo*/
    flush = ((!avpkt || !avpkt->size) && (!rest_data)) || q->reinit_pending;
    if (!flush) {
        if (q->input_fifo && av_fifo_size(q->input_fifo)) {
            /* we have got rest of previous packet into buffer */
            if (av_fifo_space(q->input_fifo) < avpkt->size) {
                ret = av_fifo_grow(q->input_fifo, avpkt->size);
                if (ret < 0)
                    return ret;
            }
            av_fifo_generic_write(q->input_fifo, avpkt->data, avpkt->size, NULL);
            bs.Data       = q->input_fifo->rptr;
            bs.DataLength = av_fifo_size(q->input_fifo);
            buffered = 1;
        } else {
            bs.Data       = avpkt->data;
            bs.DataLength = avpkt->size;
        }
        bs.MaxLength  = bs.DataLength;
        bs.TimeStamp  = avpkt->pts;
    }

    if (!q->engine_ready) {
        ret = qsv_decode_init(avctx, q, &bs);
        if (ret)
            return ret;
    }

    ret = do_decode_frame_async(avctx, q, flush, &bs);
    /* make sure we do not enter an infinite loop if the SDK
     * did not consume any data and did not return anything */
    if (ret < 0 && !bs.DataOffset && !flush && !q->reinit_pending) {
        av_log(avctx, AV_LOG_WARNING, "A decode call did not consume any data\n");
        /* both packet and fifo scenario should be taken into considertion*/
        bs.DataOffset += bs.DataLength;
        bs.DataLength = 0;

        /* do the flush */
        if (!avpkt || !avpkt->size)
            ret = do_decode_frame_async(avctx, q, 1, NULL);
    }

    if (buffered) {
        qsv_fifo_relocate(q->input_fifo, bs.DataOffset);
    } else if (bs.DataOffset != avpkt->size) {
        /* some data of packet was not consumed. store it to local buffer */
        if (av_fifo_space(q->input_fifo) < avpkt->size - bs.DataOffset) {
            ret = av_fifo_grow(q->input_fifo, avpkt->size - bs.DataOffset);
            if (ret < 0)
                return ret;
        }

        av_fifo_generic_write(q->input_fifo, avpkt->data+bs.DataOffset,
                              avpkt->size - bs.DataOffset, NULL);
    }

    if (MFX_ERR_MORE_DATA != ret && ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Error %d during QSV decoding.\n", ret);
        return ff_qsv_error(ret);
    }

    ret = do_sync_operation(avctx, q, frame, got_frame);
    if(ret < 0)
        return ret;

    av_log(avctx, AV_LOG_DEBUG, "do_qsv_decode: frame=%p size=%dB got_frame=%d\n",
           frame, avpkt->size, *got_frame);

    return avpkt->size;
}

/*
 This function inserts a packet at fifo front.
*/
static void qsv_packet_push_front(QSVContext *q, AVPacket *avpkt)
{
    int fifo_size = av_fifo_size(q->pkt_fifo);
    if (!fifo_size) {
    /* easy case fifo is empty */
        av_fifo_generic_write(q->pkt_fifo, avpkt, sizeof(*avpkt), NULL);
    } else {
    /* realloc necessary */
    	AVPacket pkt;
    	AVFifoBuffer *fifo = av_fifo_alloc( fifo_size + av_fifo_space(q->pkt_fifo) );
    	av_fifo_generic_write( fifo, avpkt, sizeof(*avpkt), NULL );
	
    	while( av_fifo_size(q->pkt_fifo) ) {
    		av_fifo_generic_read( q->pkt_fifo, &pkt, sizeof(pkt), NULL );
    		av_fifo_generic_write( fifo, &pkt, sizeof(pkt), NULL );
     	}

    	av_fifo_free( q->pkt_fifo );
    	q->pkt_fifo = fifo;
	}
}

int ff_qsv_decode(AVCodecContext *avctx, QSVContext *q,
                  AVFrame *frame, int *got_frame,
                  AVPacket *avpkt)
{
    AVPacket pkt_ref = { 0 };
    int ret = 0;

    if (q->pkt_fifo && av_fifo_size(q->pkt_fifo) >= sizeof(AVPacket)) {
        /* we already have got some buffered packets. so add new to tail */
        ret = av_packet_ref(&pkt_ref, avpkt);
        if (ret < 0)
            return ret;
        av_fifo_generic_write(q->pkt_fifo, &pkt_ref, sizeof(pkt_ref), NULL);
    }

    if (q->reinit_pending) {
        ret = do_qsv_decode(avctx, q, frame, got_frame, avpkt);
    	if(!*got_frame)
	    	close_decoder(q);
    }

    if(!q->reinit_pending) {
    	if( q->pkt_fifo && av_fifo_size(q->pkt_fifo) >= sizeof(AVPacket) ) {
        	while(!*got_frame && av_fifo_size(q->pkt_fifo) >= sizeof(AVPacket)) {
        		av_fifo_generic_read(q->pkt_fifo, &pkt_ref, sizeof(pkt_ref), NULL);
        		ret = do_qsv_decode( avctx, q, frame, got_frame, &pkt_ref );

         		if(q->reinit_pending) {
                    qsv_packet_push_front(q, &pkt_ref);
                } else {
                    av_packet_unref(&pkt_ref);
                }
           }
        } else {
            ret = do_qsv_decode(avctx, q, frame, got_frame, avpkt);

            if (q->reinit_pending) {
                ret = av_packet_ref(&pkt_ref, avpkt);
                if (ret < 0)
                    return ret;
                av_fifo_generic_write(q->pkt_fifo, &pkt_ref, sizeof(pkt_ref), NULL);
            }
        }
    }
	
    return ret;
}

/*
 This function resets decoder and corresponded buffers before seek operation
*/
void ff_qsv_decode_reset(AVCodecContext *avctx, QSVContext *q)
{
//    QSVFrame *cur;
    AVPacket pkt;
    int ret = 0;
    mfxVideoParam param = { 0 };

    if (q->reinit_pending) {
        close_decoder(q);
    } else if (q->engine_ready) {
        ret = MFXVideoDECODE_GetVideoParam(q->session, &param);
        if (ret < 0) {
            av_log(avctx, AV_LOG_ERROR, "MFX decode get param error %d\n", ret);
        }

        ret = MFXVideoDECODE_Reset(q->session, &param);
        if (ret < 0) {
            av_log(avctx, AV_LOG_ERROR, "MFX decode reset error %d\n", ret);
        }
    }

    /* Reset output surfaces */
    if(q->async_fifo != NULL){
        av_fifo_reset(q->async_fifo);
    }

    /* Reset input packets fifo */
    while (q->pkt_fifo != NULL && av_fifo_size(q->pkt_fifo)) {
        av_fifo_generic_read(q->pkt_fifo, &pkt, sizeof(pkt), NULL);
        av_packet_unref(&pkt);
    }

    /* Reset input bitstream fifo */
    if(q->input_fifo != NULL){
        av_fifo_reset(q->input_fifo);
    }
}

int ff_qsv_decode_close(QSVContext *q)
{
	av_log(NULL, AV_LOG_INFO, "Close h264_qsv decoder now\n");

    close_decoder(q);

    q->session = NULL;

    ff_qsv_close_internal_session(&q->internal_qs);

    av_fifo_free(q->async_fifo);
    q->async_fifo = NULL;

    av_fifo_free(q->input_fifo);
    q->input_fifo = NULL;

    av_fifo_free(q->pkt_fifo);
    q->pkt_fifo = NULL;

    if( NULL != q->request )
        av_freep(&q->request);

    if( NULL != q->response)
        av_freep(&q->response);
	
    return 0;
}


