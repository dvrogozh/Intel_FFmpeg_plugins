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

#include <inttypes.h>
#include <string.h>

#include "libavutil/avassert.h"
#include "libavutil/common.h"
#include "libavutil/log.h"
#include "libavutil/pixdesc.h"

#include "vaapi_encode.h"
#include "avcodec.h"

static const char *picture_type_name[] = { "IDR", "I", "P", "B" };
static int vaapi_encode_truncate_gop(AVCodecContext *avctx);
static int vaapi_encode_make_packed_header(AVCodecContext *avctx,
                                           VAAPIEncodePicture *pic,
                                           int type, char *data, size_t bit_len)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAStatus vas;
    VABufferID param_buffer, data_buffer;
    VABufferID *tmp;
    VAEncPackedHeaderParameterBuffer params = {
        .type = type,
        .bit_length = bit_len,
        .has_emulation_bytes = 1,
    };

#ifdef VPG_DRIVER
    // Found emulation byte is inserted in start code by vpg driver
    if (ctx->bipyramid && type == VAEncPackedHeaderSlice) {
        params.has_emulation_bytes = 0;
        av_log(avctx, AV_LOG_WARNING, "change slice has_emulation_bytes to 0.\n");
    }
#endif

    tmp = av_realloc_array(pic->param_buffers, sizeof(*tmp), (pic->nb_param_buffers + 2));
    if (!tmp) {
        return AVERROR(ENOMEM);
    }
    pic->param_buffers = tmp;

    vas = vaCreateBuffer(ctx->hwctx->display, ctx->va_context,
                         VAEncPackedHeaderParameterBufferType,
                         sizeof(params), 1, &params, &param_buffer);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create parameter buffer "
               "for packed header (type %d): %d (%s).\n",
               type, vas, vaErrorStr(vas));
        return AVERROR(EIO);
    }
    pic->param_buffers[pic->nb_param_buffers++] = param_buffer;

    vas = vaCreateBuffer(ctx->hwctx->display, ctx->va_context,
                         VAEncPackedHeaderDataBufferType,
                         (bit_len + 7) / 8, 1, data, &data_buffer);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create data buffer "
               "for packed header (type %d): %d (%s).\n",
               type, vas, vaErrorStr(vas));
        return AVERROR(EIO);
    }
    pic->param_buffers[pic->nb_param_buffers++] = data_buffer;

    av_log(avctx, AV_LOG_DEBUG, "Packed header buffer (%d) is %#x/%#x "
           "(%zu bits).\n", type, param_buffer, data_buffer, bit_len);
    return 0;
}

static int vaapi_encode_make_param_buffer(AVCodecContext *avctx,
                                          VAAPIEncodePicture *pic,
                                          int type, char *data, size_t len)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAStatus vas;
    VABufferID *tmp;
    VABufferID buffer;

    tmp = av_realloc_array(pic->param_buffers, sizeof(*tmp), (pic->nb_param_buffers + 1));
    if (!tmp) {
        return AVERROR(ENOMEM);
    }
    pic->param_buffers = tmp;

    vas = vaCreateBuffer(ctx->hwctx->display, ctx->va_context,
                         type, len, 1, data, &buffer);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create parameter buffer "
               "(type %d): %d (%s).\n", type, vas, vaErrorStr(vas));
        return AVERROR(EIO);
    }
    pic->param_buffers[pic->nb_param_buffers++] = buffer;

    av_log(avctx, AV_LOG_DEBUG, "Param buffer (%d) is %#x.\n",
           type, buffer);
    return 0;
}

static int vaapi_encode_wait(AVCodecContext *avctx,
                             VAAPIEncodePicture *pic)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAStatus vas;

    av_assert0(pic->encode_issued);

    if (pic->encode_complete) {
        // Already waited for this picture.
        return 0;
    }

    av_log(avctx, AV_LOG_DEBUG, "Sync to pic %"PRId64"/%"PRId64" "
           "(input surface %#x).\n", pic->display_order,
           pic->encode_order, pic->input_surface);

    vas = vaSyncSurface(ctx->hwctx->display, pic->input_surface);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to sync to picture completion: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        return AVERROR(EIO);
    }

    // Input is definitely finished with now.
    av_frame_free(&pic->input_image);

    av_freep(&pic->param_buffers);

    pic->encode_complete = 1;
#ifdef VPG_DRIVER
    {
        int i;
        for (i = 0; i < pic->nb_refs; i++) {
            pic->refs[i]->ref_count --;
        }
        if (pic->second_field)
           av_freep(&pic->second_field->param_buffers);
    }
#endif
    return 0;
}
#ifdef VPG_DRIVER
static int vaapi_encode_one_field(AVCodecContext *avctx,
                              VAAPIEncodePicture *pic)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodeSlice *slice;
    VAStatus vas;
    int err, i;
    char data[MAX_PARAM_BUFFER_SIZE];
    size_t bit_len;

    av_log(avctx, AV_LOG_DEBUG, "Issuing encode for field %"PRId64"/%"PRId64" "
        "as type %s.\n", pic->display_order, pic->encode_order,
        picture_type_name[pic->type]);

    pic->output_buffer_ref = av_buffer_pool_get(ctx->output_buffer_pool);
    if (!pic->output_buffer_ref) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    pic->output_buffer = (VABufferID)(uintptr_t)pic->output_buffer_ref->data;
    av_log(avctx, AV_LOG_DEBUG, "Output buffer is %#x.\n",
           pic->output_buffer);

    if (ctx->codec->picture_params_size > 0) {
        pic->codec_picture_params = av_malloc(ctx->codec->picture_params_size);
        if (!pic->codec_picture_params)
            goto fail;
        memcpy(pic->codec_picture_params, ctx->codec_picture_params,
               ctx->codec->picture_params_size);
    } else {
        av_assert0(!ctx->codec_picture_params);
    }

    pic->nb_param_buffers = 0;

    if (pic->type == PICTURE_TYPE_IDR && ctx->codec->init_sequence_params) {
        err = vaapi_encode_make_param_buffer(avctx, pic,
                                             VAEncSequenceParameterBufferType,
                                             ctx->codec_sequence_params,
                                             ctx->codec->sequence_params_size);
        if (err < 0)
            goto fail;
    }

    if (ctx->codec->init_picture_params) {
        err = ctx->codec->init_picture_params(avctx, pic);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to initialise picture "
                "parameters: %d.\n", err);
            goto fail;
        }
        err = vaapi_encode_make_param_buffer(avctx, pic,
                                             VAEncPictureParameterBufferType,
                                             pic->codec_picture_params,
                                             ctx->codec->picture_params_size);
        if (err < 0)
            goto fail;
    }

    // VPG driver need set bitrate hrd, quality misc param every frame.
    for (i = 0; i < ctx->nb_global_params; i++) {
        if (ctx->global_params[i]->type == VAEncMiscParameterTypeRateControl) {
            if (pic->type == PICTURE_TYPE_IDR || pic->type == PICTURE_TYPE_I) {
                ctx->rc_params.rc.min_qp = (int)(avctx->qmin * avctx->i_quant_factor + avctx->i_quant_offset + 0.5);
                ctx->rc_params.rc.initial_qp = (int)(avctx->qmax * avctx->i_quant_factor + avctx->i_quant_offset + 0.5);
                ctx->rc_params.rc.max_qp = (int)(avctx->qmax * avctx->i_quant_factor + avctx->i_quant_offset + 0.5);
            }
            if (pic->type == PICTURE_TYPE_B) {
                ctx->rc_params.rc.min_qp = (int)(avctx->qmin * avctx->b_quant_factor + avctx->b_quant_offset + 0.5);
                ctx->rc_params.rc.initial_qp = (int)(avctx->qmax * avctx->b_quant_factor + avctx->b_quant_offset + 0.5);
                ctx->rc_params.rc.max_qp = (int)(avctx->qmax * avctx->b_quant_factor + avctx->b_quant_offset + 0.5);
            }
            // If either one of min, max, initial qp is out of [0, 51],
            // should set all of them to 0 to let encoder choose the best QP according to rate control.
            // If only the bad qp is calibrated, for example set max qp to 0, then max qp will be less
            // than min qp, and VA driver will generate a very large bit stream.
            if (ctx->rc_params.rc.initial_qp > 51 || ctx->rc_params.rc.max_qp > 51
                || ctx->rc_params.rc.min_qp > 51 || ctx->rc_params.rc.max_qp < ctx->rc_params.rc.min_qp) {
                ctx->rc_params.rc.max_qp = 0;
                ctx->rc_params.rc.initial_qp = 0;
                ctx->rc_params.rc.min_qp = 0;
            }
        }
        err = vaapi_encode_make_param_buffer(avctx, pic,
                                            VAEncMiscParameterBufferType,
                                            (char*)ctx->global_params[i],
                                            ctx->global_params_size[i]);
        if (err < 0)
            goto fail;
    }

    if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_RAW_DATA &&
        ctx->codec->write_aud_header) {
        bit_len = 8 * sizeof(data);
        err = ctx->codec->write_aud_header(avctx, pic, data, &bit_len);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to write aud "
                   "header: %d.\n", err);
            goto fail;
        }
        err = vaapi_encode_make_packed_header(avctx, pic, VAEncPackedHeaderRawData,
                                              data, bit_len);
        if (err < 0)
            goto fail;
    }

    if (pic->type == PICTURE_TYPE_IDR) {
        if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_SEQUENCE &&
            ctx->codec->write_sequence_header) {
            bit_len = 8 * sizeof(data);
            err = ctx->codec->write_sequence_header(avctx, data, &bit_len);
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to write per-sequence "
                       "header: %d.\n", err);
                goto fail;
            }
            err = vaapi_encode_make_packed_header(avctx, pic,
                                                  ctx->codec->sequence_header_type,
                                                  data, bit_len);
            if (err < 0)
                goto fail;
        }
    }

    if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_PICTURE &&
        ctx->codec->write_picture_header) {
        bit_len = 8 * sizeof(data);
        err = ctx->codec->write_picture_header(avctx, pic, data, &bit_len);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to write per-picture "
                   "header: %d.\n", err);
            goto fail;
        }
        err = vaapi_encode_make_packed_header(avctx, pic,
                                              ctx->codec->picture_header_type,
                                              data, bit_len);
        if (err < 0)
            goto fail;
    }

    if (ctx->codec->write_extra_buffer) {
        for (i = 0;; i++) {
            size_t len = sizeof(data);
            int type;
            err = ctx->codec->write_extra_buffer(avctx, pic, i, &type,
                                                 data, &len);
            if (err == AVERROR_EOF)
                break;
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to write extra "
                       "buffer %d: %d.\n", i, err);
                goto fail;
            }

            err = vaapi_encode_make_param_buffer(avctx, pic, type,
                                                 data, len);
            if (err < 0)
                goto fail;
        }
    }

    if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_MISC &&
        ctx->codec->write_extra_header) {
        for (i = 0;; i++) {
            int type;
            bit_len = 8 * sizeof(data);
            err = ctx->codec->write_extra_header(avctx, pic, i, &type,
                                                 data, &bit_len);
            if (err == AVERROR_EOF)
                break;
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to write extra "
                       "header %d: %d.\n", i, err);
                goto fail;
            }

            err = vaapi_encode_make_packed_header(avctx, pic, type,
                                                  data, bit_len);
            if (err < 0)
                goto fail;
        }
    }

    pic->slices = (VAAPIEncodeSlice **)av_malloc(sizeof(VAAPIEncodeSlice *) * pic->nb_slices);
    if (pic->slices == NULL)
        goto fail;

    for (i = 0; i < pic->nb_slices; i++) {
        slice = av_mallocz(sizeof(*slice));
        if (!slice) {
            err = AVERROR(ENOMEM);
            goto fail;
        }
        pic->slices[i] = slice;
        if (ctx->codec->slice_params_size > 0) {
            slice->codec_slice_params = av_mallocz(ctx->codec->slice_params_size);
            if (!slice->codec_slice_params) {
                err = AVERROR(ENOMEM);
                goto fail;
            }
        }

        if (ctx->codec->init_slice_params) {
            err = ctx->codec->init_slice_params(avctx, pic, slice);
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to initialise slice "
                        "parameters: %d.\n", err);
                goto fail;
            }
        }

        if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_SLICE &&
            ctx->codec->write_slice_header) {
            bit_len = 8 * sizeof(data);
            err = ctx->codec->write_slice_header(avctx, pic, slice,
                                                 data, &bit_len);
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to write per-slice "
                       "header: %d.\n", err);
                goto fail;
            }
            err = vaapi_encode_make_packed_header(avctx, pic,
                                                  ctx->codec->slice_header_type,
                                                  data, bit_len);
            if (err < 0)
                goto fail;
        }

        if (ctx->codec->init_slice_params) {
            err = vaapi_encode_make_param_buffer(avctx, pic,
                                                 VAEncSliceParameterBufferType,
                                                 slice->codec_slice_params,
                                                 ctx->codec->slice_params_size);
            if (err < 0)
                goto fail;
        }
    }

    vas = vaBeginPicture(ctx->hwctx->display, ctx->va_context,
                         pic->input_surface);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to begin picture encode issue: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail_with_picture;
    }

    vas = vaRenderPicture(ctx->hwctx->display, ctx->va_context,
                              pic->param_buffers, pic->nb_param_buffers);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to upload encode parameters: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail_with_picture;
    }

    vas = vaEndPicture(ctx->hwctx->display, ctx->va_context);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to end picture encode issue: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        // vaRenderPicture() has been called here, so we should not destroy
        // the parameter buffers unless separate destruction is required.
        if (ctx->hwctx->driver_quirks &
            AV_VAAPI_DRIVER_QUIRK_RENDER_PARAM_BUFFERS)
            goto fail;
        else
            goto fail_at_end;
    }

    if (ctx->hwctx->driver_quirks &
        AV_VAAPI_DRIVER_QUIRK_RENDER_PARAM_BUFFERS) {
        for (i = 0; i < pic->nb_param_buffers; i++) {
            vas = vaDestroyBuffer(ctx->hwctx->display,
                                pic->param_buffers[i]);
            if (vas != VA_STATUS_SUCCESS) {
                av_log(avctx, AV_LOG_ERROR, "Failed to destroy "
                       "param buffer %#x: %d (%s).\n",
                       pic->param_buffers[i], vas, vaErrorStr(vas));
                // And ignore.
            }
        }
    }
    return 0;

fail_with_picture:
    vaEndPicture(ctx->hwctx->display, ctx->va_context);
fail:
    for(i = 0; i < pic->nb_param_buffers; i++)
        vaDestroyBuffer(ctx->hwctx->display, pic->param_buffers[i]);
fail_at_end:
    av_freep(&pic->codec_picture_params);
    av_freep(&pic->param_buffers);
    av_freep(&pic->slices);
    return err;
}


static int vaapi_encode_field_issue(AVCodecContext *avctx,
                                    VAAPIEncodePicture *pic)
{
   int  i, err;
   VAAPIEncodeContext *ctx = avctx->priv_data;

   av_log(avctx, AV_LOG_DEBUG, "Issuing encode for pic for field%"PRId64"/%"PRId64" "
          "as type %s.\n", pic->display_order, pic->encode_order,
          picture_type_name[pic->type]);
   if (pic->nb_refs == 0) {
       av_log(avctx, AV_LOG_DEBUG, "No reference pictures.\n");
   } else {
       av_log(avctx, AV_LOG_DEBUG, "Refers to:");
       for (i = 0; i < pic->nb_refs; i++) {
           av_log(avctx, AV_LOG_DEBUG, " %"PRId64"/%"PRId64,
                  pic->refs[i]->display_order, pic->refs[i]->encode_order);
       }
       av_log(avctx, AV_LOG_DEBUG, ".\n");
   }

   av_assert0(pic->input_available && !pic->encode_issued);
   for (i = 0; i < pic->nb_refs; i++) {
       av_assert0(pic->refs[i]);
       // If we are serialised then the references must have already
       // completed.  If not, they must have been issued but need not
       // have completed yet.
       if (ctx->issue_mode == ISSUE_MODE_SERIALISE_EVERYTHING)
           av_assert0(pic->refs[i]->encode_complete);
       else
           av_assert0(pic->refs[i]->encode_issued);
   }

   av_log(avctx, AV_LOG_DEBUG, "Input surface is %#x.\n", pic->input_surface);

   pic->recon_image = av_frame_alloc();
   if (!pic->recon_image) {
       err = AVERROR(ENOMEM);
       goto fail;
   }

   err = av_hwframe_get_buffer(ctx->recon_frames_ref, pic->recon_image, 0);
   if (err < 0) {
       err = AVERROR(ENOMEM);
       goto fail;
   }

   pic->recon_surface = (VASurfaceID)(uintptr_t)pic->recon_image->data[3];
   av_log(avctx, AV_LOG_DEBUG, "Recon surface is %#x.\n", pic->recon_surface);

   if (avctx->field_order == AV_FIELD_TT || avctx->field_order == AV_FIELD_TB)
       pic->bottom_field = 0;
   else
       pic->bottom_field = 1;
   pic->second_field_flag = 0;
   pic->second_field_be_ref = 0;
   err = vaapi_encode_one_field(avctx, pic);
   if ( err < 0 ) {
       av_log(avctx, AV_LOG_ERROR, "encode top field error = %d\n", err);
       return err;
   }

   pic->second_field = av_mallocz(sizeof(*pic));
   if (!pic->second_field)
       return AVERROR(ENOMEM);

   switch (pic->type) {
       case PICTURE_TYPE_IDR:
       case PICTURE_TYPE_I:
           pic->second_field->type = PICTURE_TYPE_P;
           break;
       case PICTURE_TYPE_P:
           pic->second_field->type = PICTURE_TYPE_P;
           break;
       case PICTURE_TYPE_B:
           pic->second_field->type = PICTURE_TYPE_B;
           break;
   }

   pic->second_field->input_surface = pic->input_surface;
   pic->second_field->recon_surface = pic->recon_surface;
   if (avctx->field_order == AV_FIELD_TT || avctx->field_order == AV_FIELD_TB)
       pic->second_field->bottom_field = 1;
   else
       pic->second_field->bottom_field = 0;
   pic->second_field->second_field_flag = 1;
   pic->second_field->second_field_be_ref = 0;
   pic->second_field->nb_slices = pic->nb_slices;
   pic->second_field->display_order = pic->display_order;
   pic->second_field->encode_order = pic->encode_order;
   pic->second_field->nb_refs = pic->nb_refs;
   pic->second_field->nb_dpbs = pic->nb_dpbs;
   pic->second_field->mini_gop_cnt = pic->mini_gop_cnt;
   pic->second_field->frame_num = pic->frame_num;
   pic->second_field->b_frame_ref_flag = pic->b_frame_ref_flag;
   for (i = 0; i < 2; i++) {
       pic->second_field->adaptive_ref_pic_marking[i][0] = UINT_MAX;
       pic->second_field->adaptive_ref_pic_marking[i][1] = UINT_MAX;
   }
   for (i = 0 ; i < pic->second_field->nb_refs; i++)
       pic->second_field->refs[i] = pic->refs[i];
   for (i = 0 ; i < pic->second_field->nb_dpbs; i++)
       pic->second_field->dpbs[i] = pic->dpbs[i];
   err = vaapi_encode_one_field(avctx, pic->second_field);
   if ( err < 0 ) {
       av_log(avctx, AV_LOG_ERROR, "encode top field error = %d\n", err);
       return err;
   }
   pic->encode_issued = 1;

   if (ctx->issue_mode == ISSUE_MODE_SERIALISE_EVERYTHING)
       return vaapi_encode_wait(avctx, pic);
   else
       return 0;
fail:
   av_frame_free(&pic->recon_image);
   return err;
}
#endif
static int vaapi_encode_issue(AVCodecContext *avctx,
                              VAAPIEncodePicture *pic)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodeSlice *slice;
    VAStatus vas;
    int err, i;
    char data[MAX_PARAM_BUFFER_SIZE];
    size_t bit_len;

    av_log(avctx, AV_LOG_DEBUG, "Issuing encode for pic %"PRId64"/%"PRId64" "
           "as type %s.\n", pic->display_order, pic->encode_order,
           picture_type_name[pic->type]);
    if (pic->nb_refs == 0) {
        av_log(avctx, AV_LOG_DEBUG, "No reference pictures.\n");
    } else {
        av_log(avctx, AV_LOG_DEBUG, "Refers to:");
        for (i = 0; i < pic->nb_refs; i++) {
            av_log(avctx, AV_LOG_DEBUG, " %"PRId64"/%"PRId64,
                   pic->refs[i]->display_order, pic->refs[i]->encode_order);
        }
        av_log(avctx, AV_LOG_DEBUG, ".\n");
    }

    av_assert0(pic->input_available && !pic->encode_issued);
    for (i = 0; i < pic->nb_refs; i++) {
        av_assert0(pic->refs[i]);
        // If we are serialised then the references must have already
        // completed.  If not, they must have been issued but need not
        // have completed yet.
        if (ctx->issue_mode == ISSUE_MODE_SERIALISE_EVERYTHING)
            av_assert0(pic->refs[i]->encode_complete);
        else
            av_assert0(pic->refs[i]->encode_issued);
    }

    av_log(avctx, AV_LOG_DEBUG, "Input surface is %#x.\n", pic->input_surface);

    pic->recon_image = av_frame_alloc();
    if (!pic->recon_image) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    err = av_hwframe_get_buffer(ctx->recon_frames_ref, pic->recon_image, 0);
    if (err < 0) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    pic->recon_surface = (VASurfaceID)(uintptr_t)pic->recon_image->data[3];
    av_log(avctx, AV_LOG_DEBUG, "Recon surface is %#x.\n", pic->recon_surface);

    pic->output_buffer_ref = av_buffer_pool_get(ctx->output_buffer_pool);
    if (!pic->output_buffer_ref) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    pic->output_buffer = (VABufferID)(uintptr_t)pic->output_buffer_ref->data;
    av_log(avctx, AV_LOG_DEBUG, "Output buffer is %#x.\n",
           pic->output_buffer);

    if (ctx->codec->picture_params_size > 0) {
        pic->codec_picture_params = av_malloc(ctx->codec->picture_params_size);
        if (!pic->codec_picture_params)
            goto fail;
        memcpy(pic->codec_picture_params, ctx->codec_picture_params,
               ctx->codec->picture_params_size);
    } else {
        av_assert0(!ctx->codec_picture_params);
    }

    pic->nb_param_buffers = 0;
#ifndef VPG_DRIVER
    if (pic->encode_order == 0) {
        // Global parameter buffers are set on the first picture only.

        for (i = 0; i < ctx->nb_global_params; i++) {
            err = vaapi_encode_make_param_buffer(avctx, pic,
                                                 VAEncMiscParameterBufferType,
                                                 (char*)ctx->global_params[i],
                                                 ctx->global_params_size[i]);
            if (err < 0)
                goto fail;
        }
    }
#endif
    if (pic->type == PICTURE_TYPE_IDR && ctx->codec->init_sequence_params) {
        err = vaapi_encode_make_param_buffer(avctx, pic,
                                             VAEncSequenceParameterBufferType,
                                             ctx->codec_sequence_params,
                                             ctx->codec->sequence_params_size);
        if (err < 0)
            goto fail;
    }

    if (ctx->codec->init_picture_params) {
        err = ctx->codec->init_picture_params(avctx, pic);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to initialise picture "
                   "parameters: %d.\n", err);
            goto fail;
        }
        err = vaapi_encode_make_param_buffer(avctx, pic,
                                             VAEncPictureParameterBufferType,
                                             pic->codec_picture_params,
                                             ctx->codec->picture_params_size);
        if (err < 0)
            goto fail;
    }

#ifdef VPG_DRIVER

    // VPG driver need set bitrate hrd, quality misc param every frame.
    for (i = 0; i < ctx->nb_global_params; i++) {
        if (ctx->global_params[i]->type == VAEncMiscParameterTypeRateControl) {
            if (pic->type == PICTURE_TYPE_IDR || pic->type == PICTURE_TYPE_I) {
                ctx->rc_params.rc.min_qp = (int)(avctx->qmin * avctx->i_quant_factor + avctx->i_quant_offset + 0.5);
                ctx->rc_params.rc.initial_qp = (int)(avctx->qmax * avctx->i_quant_factor + avctx->i_quant_offset + 0.5);
                ctx->rc_params.rc.max_qp = (int)(avctx->qmax * avctx->i_quant_factor + avctx->i_quant_offset + 0.5);
            }
            if (pic->type == PICTURE_TYPE_B) {
                ctx->rc_params.rc.min_qp = (int)(avctx->qmin * avctx->b_quant_factor + avctx->b_quant_offset + 0.5);
                ctx->rc_params.rc.initial_qp = (int)(avctx->qmax * avctx->b_quant_factor + avctx->b_quant_offset + 0.5);
                ctx->rc_params.rc.max_qp = (int)(avctx->qmax * avctx->b_quant_factor + avctx->b_quant_offset + 0.5);
            }
            // If either one of min, max, initial qp is out of [0, 51],
            // should set all of them to 0 to let encoder choose the best QP according to rate control.
            // If only the bad qp is calibrated, for example set max qp to 0, then max qp will be less
            // than min qp, and VA driver will generate a very large bit stream.
            if (ctx->rc_params.rc.initial_qp > 51 || ctx->rc_params.rc.max_qp > 51
                || ctx->rc_params.rc.min_qp > 51 || ctx->rc_params.rc.max_qp < ctx->rc_params.rc.min_qp) {
                ctx->rc_params.rc.max_qp = 0;
                ctx->rc_params.rc.initial_qp = 0;
                ctx->rc_params.rc.min_qp = 0;
            }
        }
        err = vaapi_encode_make_param_buffer(avctx, pic,
                                            VAEncMiscParameterBufferType,
                                            (char*)ctx->global_params[i],
                                            ctx->global_params_size[i]);
        if (err < 0)
            goto fail;
    }
#endif
    if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_RAW_DATA &&
        ctx->codec->write_aud_header) {
        bit_len = 8 * sizeof(data);
        err = ctx->codec->write_aud_header(avctx, pic, data, &bit_len);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to write aud "
                   "header: %d.\n", err);
            goto fail;
        }
        err = vaapi_encode_make_packed_header(avctx, pic, VAEncPackedHeaderRawData,
                                              data, bit_len);
        if (err < 0)
            goto fail;
    }

    if (pic->type == PICTURE_TYPE_IDR) {
        if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_SEQUENCE &&
            ctx->codec->write_sequence_header) {
            bit_len = 8 * sizeof(data);
            err = ctx->codec->write_sequence_header(avctx, data, &bit_len);
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to write per-sequence "
                       "header: %d.\n", err);
                goto fail;
            }
            err = vaapi_encode_make_packed_header(avctx, pic,
                                                  ctx->codec->sequence_header_type,
                                                  data, bit_len);
            if (err < 0)
                goto fail;
        }
    }

    if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_PICTURE &&
        ctx->codec->write_picture_header) {
        bit_len = 8 * sizeof(data);
        err = ctx->codec->write_picture_header(avctx, pic, data, &bit_len);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to write per-picture "
                   "header: %d.\n", err);
            goto fail;
        }
        err = vaapi_encode_make_packed_header(avctx, pic,
                                              ctx->codec->picture_header_type,
                                              data, bit_len);
        if (err < 0)
            goto fail;
    }

    if (ctx->codec->write_extra_buffer) {
        for (i = 0;; i++) {
            size_t len = sizeof(data);
            int type;
            err = ctx->codec->write_extra_buffer(avctx, pic, i, &type,
                                                 data, &len);
            if (err == AVERROR_EOF)
                break;
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to write extra "
                       "buffer %d: %d.\n", i, err);
                goto fail;
            }

            err = vaapi_encode_make_param_buffer(avctx, pic, type,
                                                 data, len);
            if (err < 0)
                goto fail;
        }
    }

    if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_MISC &&
        ctx->codec->write_extra_header) {
        for (i = 0;; i++) {
            int type;
            bit_len = 8 * sizeof(data);
            err = ctx->codec->write_extra_header(avctx, pic, i, &type,
                                                 data, &bit_len);
            if (err == AVERROR_EOF)
                break;
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to write extra "
                       "header %d: %d.\n", i, err);
                goto fail;
            }

            err = vaapi_encode_make_packed_header(avctx, pic, type,
                                                  data, bit_len);
            if (err < 0)
                goto fail;
        }
    }

    pic->slices = (VAAPIEncodeSlice **)av_malloc(sizeof(VAAPIEncodeSlice *) * pic->nb_slices);
    if (pic->slices == NULL)
        goto fail;

    for (i = 0; i < pic->nb_slices; i++) {
        slice = av_mallocz(sizeof(*slice));
        if (!slice) {
            err = AVERROR(ENOMEM);
            goto fail;
        }
        slice->index = i;
        pic->slices[i] = slice;
        if (ctx->codec->slice_params_size > 0) {
            slice->codec_slice_params = av_mallocz(ctx->codec->slice_params_size);
            if (!slice->codec_slice_params) {
                err = AVERROR(ENOMEM);
                goto fail;
            }
        }

        if (ctx->codec->init_slice_params) {
            err = ctx->codec->init_slice_params(avctx, pic, slice);
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to initialise slice "
                       "parameters: %d.\n", err);
                goto fail;
            }
        }

        if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_SLICE &&
            ctx->codec->write_slice_header) {
            bit_len = 8 * sizeof(data);
            err = ctx->codec->write_slice_header(avctx, pic, slice,
                                                 data, &bit_len);
            if (err < 0) {
                av_log(avctx, AV_LOG_ERROR, "Failed to write per-slice "
                       "header: %d.\n", err);
                goto fail;
            }
            err = vaapi_encode_make_packed_header(avctx, pic,
                                                  ctx->codec->slice_header_type,
                                                  data, bit_len);
            if (err < 0)
                goto fail;
        }

        if (ctx->codec->init_slice_params) {
            err = vaapi_encode_make_param_buffer(avctx, pic,
                                                 VAEncSliceParameterBufferType,
                                                 slice->codec_slice_params,
                                                 ctx->codec->slice_params_size);
            if (err < 0)
                goto fail;
        }
    }

    vas = vaBeginPicture(ctx->hwctx->display, ctx->va_context,
                         pic->input_surface);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to begin picture encode issue: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail_with_picture;
    }

    vas = vaRenderPicture(ctx->hwctx->display, ctx->va_context,
                          pic->param_buffers, pic->nb_param_buffers);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to upload encode parameters: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail_with_picture;
    }

    vas = vaEndPicture(ctx->hwctx->display, ctx->va_context);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to end picture encode issue: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        // vaRenderPicture() has been called here, so we should not destroy
        // the parameter buffers unless separate destruction is required.
        if (ctx->hwctx->driver_quirks &
            AV_VAAPI_DRIVER_QUIRK_RENDER_PARAM_BUFFERS)
            goto fail;
        else
            goto fail_at_end;
    }

    if (ctx->hwctx->driver_quirks &
        AV_VAAPI_DRIVER_QUIRK_RENDER_PARAM_BUFFERS) {
        for (i = 0; i < pic->nb_param_buffers; i++) {
            vas = vaDestroyBuffer(ctx->hwctx->display,
                                  pic->param_buffers[i]);
            if (vas != VA_STATUS_SUCCESS) {
                av_log(avctx, AV_LOG_ERROR, "Failed to destroy "
                       "param buffer %#x: %d (%s).\n",
                       pic->param_buffers[i], vas, vaErrorStr(vas));
                // And ignore.
            }
        }
    }

    pic->encode_issued = 1;

    if (ctx->issue_mode == ISSUE_MODE_SERIALISE_EVERYTHING)
        return vaapi_encode_wait(avctx, pic);
    else
        return 0;

fail_with_picture:
    vaEndPicture(ctx->hwctx->display, ctx->va_context);
fail:
    for(i = 0; i < pic->nb_param_buffers; i++)
        vaDestroyBuffer(ctx->hwctx->display, pic->param_buffers[i]);
fail_at_end:
    av_freep(&pic->codec_picture_params);
    av_freep(&pic->param_buffers);
    av_freep(&pic->slices);
    av_frame_free(&pic->recon_image);
    return err;
}

static int vaapi_encode_output(AVCodecContext *avctx,
                               VAAPIEncodePicture *pic, AVPacket *pkt)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VACodedBufferSegment *buf_list, *buf;
    VAStatus vas;
    int err;
#ifdef VPG_DRIVER
    int len = 0;
#endif
    err = vaapi_encode_wait(avctx, pic);
    if (err < 0)
        return err;

    buf_list = NULL;
    vas = vaMapBuffer(ctx->hwctx->display, pic->output_buffer,
                      (void**)&buf_list);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to map output buffers: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail;
    }

    for (buf = buf_list; buf; buf = buf->next) {
        av_log(avctx, AV_LOG_DEBUG, "Output buffer: %u bytes "
               "(status %08x).\n", buf->size, buf->status);

        err = av_new_packet(pkt, buf->size);
        if (err < 0)
            goto fail_mapped;
#ifdef VPG_DRIVER
        len += buf->size;
#endif
        memcpy(pkt->data, buf->buf, buf->size);
    }
    if (pic->type == PICTURE_TYPE_IDR)
        pkt->flags |= AV_PKT_FLAG_KEY;

    pkt->pts = pic->pts;

    vas = vaUnmapBuffer(ctx->hwctx->display, pic->output_buffer);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to unmap output buffers: "
               "%d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail;
    }

    av_buffer_unref(&pic->output_buffer_ref);
    pic->output_buffer = VA_INVALID_ID;

    av_log(avctx, AV_LOG_DEBUG, "Output read for pic %"PRId64"/%"PRId64".\n",
           pic->display_order, pic->encode_order);

#ifdef VPG_DRIVER
    if (pic->second_field) {
        pic = pic->second_field;
        vas = vaMapBuffer(ctx->hwctx->display, pic->output_buffer,
                          (void**)&buf_list);
        if (vas != VA_STATUS_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR, "Failed to map output buffers: "
                   "%d (%s).\n", vas, vaErrorStr(vas));
            err = AVERROR(EIO);
            goto fail;
        }

        for (buf = buf_list; buf; buf = buf->next) {
            av_log(avctx, AV_LOG_DEBUG, "Output buffer: %u bytes "
                   "(status %08x).\n", buf->size, buf->status);

            err = av_grow_packet(pkt, buf->size);
            if (err < 0)
                goto fail_mapped;

            memcpy(pkt->data + len, buf->buf, buf->size);
            len += buf->size;
        }

        vas = vaUnmapBuffer(ctx->hwctx->display, pic->output_buffer);
        if (vas != VA_STATUS_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR, "Failed to unmap output buffers: "
                   "%d (%s).\n", vas, vaErrorStr(vas));
            err = AVERROR(EIO);
            goto fail;
        }

        av_buffer_unref(&pic->output_buffer_ref);
        pic->output_buffer = VA_INVALID_ID;

        av_log(avctx, AV_LOG_DEBUG, "Output read for pic %"PRId64"/%"PRId64".\n",
               pic->display_order, pic->encode_order);
    }
#endif
    return 0;

fail_mapped:
    vaUnmapBuffer(ctx->hwctx->display, pic->output_buffer);
fail:
    av_buffer_unref(&pic->output_buffer_ref);
    pic->output_buffer = VA_INVALID_ID;
    return err;
}

static int vaapi_encode_discard(AVCodecContext *avctx,
                                VAAPIEncodePicture *pic)
{
    vaapi_encode_wait(avctx, pic);

    if (pic->output_buffer_ref) {
        av_log(avctx, AV_LOG_DEBUG, "Discard output for pic "
               "%"PRId64"/%"PRId64".\n",
               pic->display_order, pic->encode_order);

        av_buffer_unref(&pic->output_buffer_ref);
        pic->output_buffer = VA_INVALID_ID;
    }
#ifdef VPG_DRIVER
    if (pic->second_field && pic->second_field->output_buffer_ref) {
        av_log(avctx, AV_LOG_DEBUG, "Discard output for pic "
               "%"PRId64"/%"PRId64".\n",
               pic->display_order, pic->encode_order);

        av_buffer_unref(&pic->second_field->output_buffer_ref);
        pic->second_field->output_buffer = VA_INVALID_ID;
    }
#endif
    return 0;
}

static VAAPIEncodePicture *vaapi_encode_alloc(void)
{
    VAAPIEncodePicture *pic;

    pic = av_mallocz(sizeof(*pic));
    if (!pic)
        return NULL;

    pic->input_surface = VA_INVALID_ID;
    pic->recon_surface = VA_INVALID_ID;
    pic->output_buffer = VA_INVALID_ID;

    return pic;
}

static int vaapi_encode_free(AVCodecContext *avctx,
                             VAAPIEncodePicture *pic)
{
    int i;

    if (pic->encode_issued)
        vaapi_encode_discard(avctx, pic);

    for (i = 0; i < pic->nb_slices; i++) {
        av_freep(&pic->slices[i]->priv_data);
        av_freep(&pic->slices[i]->codec_slice_params);
        av_freep(&pic->slices[i]);
    }
    av_freep(&pic->codec_picture_params);

    av_frame_free(&pic->input_image);
    av_frame_free(&pic->recon_image);

    av_freep(&pic->param_buffers);
    av_freep(&pic->slices);
    // Output buffer should already be destroyed.
    av_assert0(pic->output_buffer == VA_INVALID_ID);

    av_freep(&pic->priv_data);
    av_freep(&pic->codec_picture_params);
#ifdef VPG_DRIVER
    if (pic->second_field) {
        for (i = 0; i < pic->second_field->nb_slices; i++) {
            av_freep(&pic->second_field->slices[i]->priv_data);
            av_freep(&pic->second_field->slices[i]->codec_slice_params);
            av_freep(&pic->second_field->slices[i]);
        }
        av_freep(&pic->second_field->codec_picture_params);

        av_frame_free(&pic->second_field->input_image);
        av_frame_free(&pic->second_field->recon_image);

        av_freep(&pic->second_field->param_buffers);
        av_freep(&pic->second_field->slices);
        // Output buffer should already be destroyed.
        av_assert0(pic->second_field->output_buffer == VA_INVALID_ID);

        av_freep(&pic->second_field->priv_data);
        av_freep(&pic->second_field->codec_picture_params);
        av_free(pic->second_field);
    }
#endif
    av_free(pic);

    return 0;
}

static int vaapi_encode_step(AVCodecContext *avctx,
                             VAAPIEncodePicture *target)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodePicture *pic;
    int i, err;

    if (ctx->issue_mode == ISSUE_MODE_SERIALISE_EVERYTHING ||
        ctx->issue_mode == ISSUE_MODE_MINIMISE_LATENCY) {
        // These two modes are equivalent, except that we wait for
        // immediate completion on each operation if serialised.

        if (!target) {
            // No target, nothing to do yet.
            return 0;
        }

        if (target->encode_complete) {
            // Already done.
            return 0;
        }

        pic = target;
        for (i = 0; i < pic->nb_refs; i++) {
            if (!pic->refs[i]->encode_complete) {
                err = vaapi_encode_step(avctx, pic->refs[i]);
                if (err < 0)
                    return err;
            }
        }
#ifdef VPG_DRIVER
        if (avctx->field_order == AV_FIELD_UNKNOWN || avctx->field_order == AV_FIELD_PROGRESSIVE)
            err = vaapi_encode_issue(avctx, pic);
        else
            err = vaapi_encode_field_issue(avctx, pic);
#else
        err = vaapi_encode_issue(avctx, pic);
#endif
        if (err < 0)
            return err;

    } else if (ctx->issue_mode == ISSUE_MODE_MAXIMISE_THROUGHPUT) {
        int activity;

        // Run through the list of all available pictures repeatedly
        // and issue the first one found which has all dependencies
        // available (including previously-issued but not necessarily
        // completed pictures).
        do {
            activity = 0;
            for (pic = ctx->pic_start; pic; pic = pic->next) {
                if (!pic->input_available || pic->encode_issued)
                    continue;
                for (i = 0; i < pic->nb_refs; i++) {
                    if (!pic->refs[i]->encode_issued)
                        break;
                }
                if (i < pic->nb_refs)
                    continue;
#ifdef VPG_DRIVER
                if (avctx->field_order == AV_FIELD_UNKNOWN || avctx->field_order == AV_FIELD_PROGRESSIVE)
                    err = vaapi_encode_issue(avctx, pic);
                else
                    err = vaapi_encode_field_issue(avctx, pic);
#else
                err = vaapi_encode_issue(avctx, pic);
#endif
                if (err < 0)
                    return err;
                activity = 1;
                // Start again from the beginning of the list,
                // because issuing this picture may have satisfied
                // forward dependencies of earlier ones.
                break;
            }
        } while(activity);

        // If we had a defined target for this step then it will
        // always have been issued by now.
        if (target) {
            av_assert0(target->encode_issued && "broken dependencies?");
        }

    } else {
        av_assert0(0);
    }

    return 0;
}

#ifdef VPG_DRIVER
static void vaapi_encode_add_reference (AVCodecContext *avctx,
        VAAPIEncodePicture *pic)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    int i;

    av_assert0 (pic->type != PICTURE_TYPE_B || pic->b_frame_ref_flag);

    if (pic->type == PICTURE_TYPE_IDR) {
        // clear the reference frame list
        for (i = 0 ; i < ctx->ref_nr; i++) {
           ctx->references[i]->ref_count --;
           ctx->references[i] = NULL;
        }
        ctx->ref_nr = 0;
    }

    if (ctx->ref_nr == ctx->max_ref_nr) {
        // calculate adaptive_ref_pic_marking params
        if (ctx->bipyramid) {
            VAAPIEncodePicture *out_default = NULL;
            unsigned int out_pic_num[2], current_pic_num;

            out_default = ctx->references[0];
            for (i = 0 ; i < ctx->ref_nr; i++) {
                if (ctx->references[i]->encode_order < out_default->encode_order)
                    out_default = ctx->references[i];
            }

            if (out_default != ctx->references[0]) {
                if (avctx->field_order == AV_FIELD_UNKNOWN || avctx->field_order == AV_FIELD_PROGRESSIVE) {
                    current_pic_num = pic->frame_num;
                    out_pic_num[0] = ctx->references[0]->frame_num;
                } else {
                    current_pic_num = 2 * pic->frame_num + 1;
                    out_pic_num[0] = 2 * ctx->references[0]->frame_num + pic->second_field_flag;
                    out_pic_num[1] = 2 * ctx->references[0]->frame_num + !pic->second_field_flag;
                }
                pic->adaptive_ref_pic_marking[0][0] = 1;
                pic->adaptive_ref_pic_marking[0][1] = current_pic_num - out_pic_num[0] - 1;
                if (avctx->field_order != AV_FIELD_UNKNOWN && avctx->field_order != AV_FIELD_PROGRESSIVE) {
                    pic->adaptive_ref_pic_marking[1][0] = 1;
                    pic->adaptive_ref_pic_marking[1][1] = current_pic_num - out_pic_num[1] - 1;
                }
            }
        }
        // remove the old reference frame
        for (i = 0 ; i < ctx->ref_nr - 1; i++) {
           ctx->references[i]->ref_count --;
           ctx->references[i] = ctx->references[i+1];
           ctx->references[i]->ref_count ++;
        }
        ctx->references[ctx->ref_nr-1]->ref_count--;
        ctx->ref_nr --;
    }

    if (pic->type == PICTURE_TYPE_B) {
        av_assert0 (ctx->ref_nr > 0);

        // insert by POC order
        for (i = ctx->ref_nr - 1; i >= 0; i--)
            ctx->references[i + 1] = ctx->references[i];

        for (i = 0; i < ctx->ref_nr; i++) {
            if (i == ctx->ref_nr - 1) {
                if (ctx->references[i]->display_order > pic->display_order)
                    ctx->references[i] = pic;
                else
                    ctx->references[i + 1] = pic;
                break;
            }

            if (ctx->references[i]->display_order > pic->display_order) {
                ctx->references[i] = pic;
                break;
            } else
                ctx->references[i + 1] = ctx->references[i + 2];
        }
        pic->ref_count ++;
        ctx->ref_nr++;
    } else {
        ctx->references[ctx->ref_nr] = pic;
        ctx->references[ctx->ref_nr]->ref_count ++;
        ctx->ref_nr++;
    }

}

static int vaapi_encode_get_encode_order (int display_order, int begin,
                                        int end, int counter,int * ref)
{
    int pivot;

    av_assert0 (display_order >= begin);
    av_assert0 (display_order < end);

    if (end - begin > 1)
        *ref = 1;
    else
        *ref = 0;

    pivot = (begin + end) / 2;
    if (display_order == pivot)
        return counter;
    else if (display_order < pivot)
        return vaapi_encode_get_encode_order (display_order, begin, pivot, counter + 1, ref);
    else
        return vaapi_encode_get_encode_order (display_order, pivot + 1, end,
            counter + 1 + pivot - begin, ref);
}
#endif

static int vaapi_encode_get_next(AVCodecContext *avctx,
                                 VAAPIEncodePicture **pic_out)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodePicture *start, *end, *pic;
    int i;
#ifdef VPG_DRIVER
    int j;
#endif

    if (ctx->force_idr) {
    	vaapi_encode_truncate_gop(avctx);
    } else {
        for (pic = ctx->pic_start; pic; pic = pic->next) {
            if (pic->next)
#ifdef VPG_DRIVER
                av_assert0(pic->display_order < pic->next->display_order);
#else
                av_assert0(pic->display_order + 1 == pic->next->display_order);
#endif
            if (pic->display_order == ctx->input_order) {
                *pic_out = pic;
                return 0;
            }
        }
    }
    if (ctx->input_order == 0) {
        // First frame is always an IDR frame.
        av_assert0(!ctx->pic_start && !ctx->pic_end);

        pic = vaapi_encode_alloc();
        if (!pic)
            return AVERROR(ENOMEM);

        pic->type = PICTURE_TYPE_IDR;
        pic->display_order = 0;
        pic->encode_order  = 0;

        ctx->pic_start = ctx->pic_end = pic;

#ifdef VPG_DRIVER
        pic->ref_count = 0;
        if (ctx->bipyramid) {
            pic->frame_num = ctx->current_frame_num = 0;
            for (i = 0; i < 2; i++) {
                pic->adaptive_ref_pic_marking[i][0] = UINT_MAX;
                pic->adaptive_ref_pic_marking[i][1] = UINT_MAX;
            }
        }
        vaapi_encode_add_reference(avctx, pic);
#endif
        *pic_out = pic;
        return 0;
    }
    pic = vaapi_encode_alloc();
    if (!pic)
        return AVERROR(ENOMEM);

#ifdef VPG_DRIVER
    pic->ref_count = 0;
    if (ctx->bipyramid) {
        for (i = 0; i < 2; i++) {
            pic->adaptive_ref_pic_marking[i][0] = UINT_MAX;
            pic->adaptive_ref_pic_marking[i][1] = UINT_MAX;
        }
    }
#endif
    if (ctx->p_per_i == 0 || ctx->p_counter == ctx->p_per_i || ctx->force_idr) {
        if (ctx->force_idr == 1 || ctx->i_per_idr == 0 || ctx->i_counter == ctx->i_per_idr) {
            pic->type = PICTURE_TYPE_IDR;
            ctx->i_counter = 0;
#ifdef VPG_DRIVER
            if (ctx->bipyramid)
                pic->frame_num = ctx->current_frame_num = 0;
#endif
        } else {
            pic->type = PICTURE_TYPE_I;
            ++ctx->i_counter;
#ifdef VPG_DRIVER
            for (i = 0 ; i < ctx->ref_nr; i++) {
                pic->refs[i] = ctx->references[i];
                pic->refs[i]->ref_count++;
            }
            pic->nb_refs = ctx->ref_nr;

            if (ctx->bipyramid) {
                ctx->current_frame_num++;
                pic->frame_num = ctx->current_frame_num;
            }
#endif
        }
        ctx->force_idr = 0;
        ctx->p_counter = 0;
    } else {
        pic->type = PICTURE_TYPE_P;
#ifdef VPG_DRIVER
        for (i = 0 ; i < ctx->ref_nr; i++) {
            pic->refs[i] = ctx->references[i];
            pic->refs[i]->ref_count++;
        }
        pic->nb_refs = ctx->ref_nr;
        if (ctx->bipyramid) {
            pic->mini_gop_cnt = ctx->p_counter + 1;
            ctx->current_frame_num++;
            pic->frame_num = ctx->current_frame_num;
        }
#else
        pic->refs[0] = ctx->pic_end;
        pic->nb_refs = 1;
#endif
        //++ctx->gop_counter;
        ++ctx->p_counter;
    }
    start = end = pic;

#ifdef VPG_DRIVER
    if (ctx->bipyramid) {
        // update poc info for pyramid-b case
        pic->encode_order = ctx->input_order;
        pic->display_order = ctx->input_order + ctx->b_per_p;
    }
    vaapi_encode_add_reference(avctx, end);
#endif
    if (pic->type != PICTURE_TYPE_IDR) {
        // If that was not an IDR frame, add B-frames display-before and
        // encode-after it, but not exceeding the GOP size.

        for (i = 0; i < ctx->b_per_p; i++) {
            pic = vaapi_encode_alloc();
            if (!pic)
                goto fail;

            pic->type = PICTURE_TYPE_B;
#ifdef VPG_DRIVER
            if (ctx->bipyramid) {
                pic->mini_gop_cnt = ctx->p_counter;
                for (j = 0; j < 2; j++) {
                    pic->adaptive_ref_pic_marking[j][0] = UINT_MAX;
                    pic->adaptive_ref_pic_marking[j][1] = UINT_MAX;
                }
            } else {
                for (j = 0 ; j < ctx->ref_nr; j++) {
                    pic->refs[j] = ctx->references[j];
                    pic->refs[j]->ref_count++;
                }
                pic->nb_refs = ctx->ref_nr;
            }
#else
            pic->refs[0] = ctx->pic_end;
            pic->refs[1] = end;
            pic->nb_refs = 2;
#endif
            pic->next = start;
            pic->display_order = ctx->input_order + ctx->b_per_p - i - 1;
            pic->encode_order  = pic->display_order + 1;
            start = pic;

            //++ctx->gop_counter;
        }
    }

    for (i = 0, pic = start; pic; i++, pic = pic->next) {
        pic->display_order = ctx->input_order + i;
        if (end->type == PICTURE_TYPE_IDR)
            pic->encode_order = ctx->input_order + i;
        else if (pic == end)
            pic->encode_order = ctx->input_order;
#ifdef VPG_DRIVER
        else {
            if (ctx->bipyramid && (pic->type == PICTURE_TYPE_B)) {
                // for Pyramid-B
                pic->encode_order = vaapi_encode_get_encode_order (i, 0, ctx->b_per_p, 0, &pic->b_frame_ref_flag);
                pic->encode_order += ctx->input_order + 1;
            } else
                pic->encode_order = ctx->input_order + i + 1;
        }
#else
        else
            pic->encode_order = ctx->input_order + i + 1;
#endif
    }

#ifdef VPG_DRIVER
    if (ctx->bipyramid && (start->type != PICTURE_TYPE_IDR)) {
        // for Pyamid-B, sort encode window by encode_order
        VAAPIEncodePicture *pic_b[16];
        int j;

        i = 0;

        // pic_b[0] is P frame
        for (pic = start; pic; pic = pic->next)
            pic_b[i++] = pic;
        av_assert0 (i == ctx->b_per_p + 1);

        for (i = 0; i <= ctx->b_per_p; i++) {
            for (j = 0; j <= ctx->b_per_p; j ++) {
                if (pic_b[j]->encode_order == ctx->input_order + i) {
                    if (i == 0)
                        start = pic = pic_b[j];
                    else {
                        pic->next = pic_b[j];
                        pic = pic->next;
                    }
                    break;
                }
            }
            av_assert0 (j <= ctx->b_per_p);
        }

        end = pic;
        end->next = NULL;

        // set B frames ref-list
        for (pic = start; pic; pic = pic->next) {
            if (pic->type == PICTURE_TYPE_B) {
                for (j = 0; j < MAX_PICTURE_REFERENCES; j++)
                    pic->refs[j] = NULL;

                for (j = 0 ; j < ctx->ref_nr; j++)
                    pic->dpbs[j] = ctx->references[j];
                pic->nb_dpbs = ctx->ref_nr;

                for (j = 0 ; j < ctx->ref_nr; j++) {
                    if (j > 1 && ctx->references[j]->display_order > pic->display_order && ctx->references[j - 1]->display_order > pic->display_order)
                        break;
                    pic->refs[j] = ctx->references[j];
                    pic->refs[j]->ref_count++;
                }
                pic->nb_refs = j;

                if (pic->b_frame_ref_flag) {
                    ctx->current_frame_num++;
                    pic->frame_num = ctx->current_frame_num;
                    vaapi_encode_add_reference (avctx, pic);
                } else
                    pic->frame_num = ctx->current_frame_num;
            }
        }

        // resume by display-order
        start = pic = pic_b[0];
        for (i = 0; i <= ctx->b_per_p; i++) {
            pic->next = pic_b[i];
            pic = pic->next;
        }
        end = pic;
        end->next = NULL;
    }
#endif

    av_assert0(ctx->pic_end);
    ctx->pic_end->next = start;
    ctx->pic_end = end;

    *pic_out = start;

    av_log(avctx, AV_LOG_DEBUG, "Pictures:");
    for (pic = ctx->pic_start; pic; pic = pic->next) {
        av_log(avctx, AV_LOG_DEBUG, " %s (%"PRId64"/%"PRId64")",
               picture_type_name[pic->type],
               pic->display_order, pic->encode_order);
    }
    av_log(avctx, AV_LOG_DEBUG, "\n");

    return 0;

fail:
    while (start) {
        pic = start->next;
        vaapi_encode_free(avctx, start);
        start = pic;
    }
    return AVERROR(ENOMEM);
}

static int vaapi_encode_truncate_gop(AVCodecContext *avctx)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodePicture *pic, *last_pic, *next;

    // Find the last picture we actually have input for.
    for (pic = ctx->pic_start; pic; pic = pic->next) {
        if (!pic->input_available)
            break;
        last_pic = pic;
    }

    if (pic) {
        av_assert0(last_pic);

        if (last_pic->type == PICTURE_TYPE_B) {
            // Some fixing up is required.  Change the type of this
            // picture to P, then modify preceding B references which
            // point beyond it to point at it instead.
#ifdef VPG_DRIVER
            int last_ref = last_pic->nb_refs - 1;

            if (!ctx->bipyramid) {
                last_pic->type = PICTURE_TYPE_P;
                last_pic->encode_order = last_pic->refs[last_ref]->encode_order;

                for (pic = ctx->pic_start; pic != last_pic; pic = pic->next) {
                    if (pic->type == PICTURE_TYPE_B &&
                        pic->refs[last_ref] == last_pic->refs[last_ref]) {
                        if (last_pic->refs[last_ref])
                           pic->refs[last_ref]->ref_count --;
                        pic->refs[last_ref] = last_pic;
                        pic->refs[last_ref]->ref_count ++;
                    }
                }
                last_pic->nb_refs = last_pic->refs[last_ref] ?  last_pic->nb_refs - 1 :  last_pic->nb_refs;

                if (last_pic->refs[last_ref])
                    last_pic->refs[last_ref]->ref_count--;
                last_pic->refs[last_ref] = NULL;
            } else {
                // Fix up for Pyramid-B
                VAAPIEncodePicture *pic_p = NULL;
                int i;

                // find P frame
                for (pic = ctx->pic_start; pic; pic = pic->next) {
                    if (pic->mini_gop_cnt == last_pic->mini_gop_cnt && pic->type == PICTURE_TYPE_P)
                        pic_p = pic;
                }
                av_assert0 (pic_p);

                last_pic->encode_order = pic_p->encode_order;
                last_pic->frame_num = pic_p->frame_num;

                // set other B frames
                for (pic = ctx->pic_start; pic != last_pic; pic = pic->next) {
                    if (pic->mini_gop_cnt == last_pic->mini_gop_cnt && pic->type == PICTURE_TYPE_B) {
                        if (pic == last_pic)
                            continue;
                        pic->b_frame_ref_flag = 0;
                        pic->frame_num = pic_p->frame_num;
                        pic->encode_order = ++pic_p->encode_order;
                        for (i = 0; i < 2; i++) {
                            pic->adaptive_ref_pic_marking[i][0] = UINT_MAX;
                            pic->adaptive_ref_pic_marking[i][1] = UINT_MAX;
                        }

                        for (i = 0; i < pic->nb_refs; i++)
                            pic->refs[i]->ref_count --;

                        pic->nb_refs = pic_p->nb_refs;
                        pic->nb_dpbs = pic_p->nb_refs;
                        for (i = 0; i < pic_p->nb_refs - 1; i++) {
                            pic->refs[i] = pic_p->refs[i + 1];
                            pic->dpbs[i] = pic_p->refs[i + 1];
                            pic->refs[i]->ref_count ++;
                        }
                        pic->dpbs[i] = last_pic;
                        pic->refs[i++] = last_pic;
                        last_pic->ref_count ++;
                        for (; i < MAX_PICTURE_REFERENCES; i++)
                            pic->refs[i] = NULL;
                    }
                }

                // process last B
                last_pic->b_frame_ref_flag = 0;
                last_pic->type = PICTURE_TYPE_P;
                for (i = 0; i < 2; i++) {
                    last_pic->adaptive_ref_pic_marking[i][0] = pic_p->adaptive_ref_pic_marking[i][0];
                    last_pic->adaptive_ref_pic_marking[i][1] = pic_p->adaptive_ref_pic_marking[i][1];
                }

                for (i = 0; i < last_pic->nb_refs; i++)
                    last_pic->refs[i]->ref_count --;

                last_pic->nb_refs = pic_p->nb_refs;
                for (i = 0; i < MAX_PICTURE_REFERENCES; i++) {
                    last_pic->refs[i] = pic_p->refs[i];
                    if (last_pic->refs[i])
                        last_pic->refs[i]->ref_count ++;
                }
            }
#else
            last_pic->type = PICTURE_TYPE_P;
            last_pic->encode_order = last_pic->refs[1]->encode_order;

            for (pic = ctx->pic_start; pic != last_pic; pic = pic->next) {
                if (pic->type == PICTURE_TYPE_B &&
                    pic->refs[1] == last_pic->refs[1])
                    pic->refs[1] = last_pic;
            }

            last_pic->nb_refs = 1;
            last_pic->refs[1] = NULL;
#endif
        } else {
            // We can use the current structure (no references point
            // beyond the end), but there are unused pics to discard.
        }
#ifdef VPG_DRIVER
        // Discard all following pics, they will never be used.
        for (pic = last_pic->next; pic; pic = next) {
            int i;
            int ref_nr = ctx->ref_nr;
            next = pic->next;

            for (i = 0; i < pic->nb_refs; i++) {
                pic->refs[i]->ref_count--;
            }
            for (i = 0 ; i < ref_nr; i++) {
                if (ctx->references[i] == pic) {
                    ctx->references[i]->ref_count--;
                    ctx->references[i] = NULL;
                    ctx->ref_nr --;
                }
            }
            vaapi_encode_free(avctx, pic);
        }
#else
        // Discard all following pics, they will never be used.
        for (pic = last_pic->next; pic; pic = next) {
            next = pic->next;
            vaapi_encode_free(avctx, pic);
        }
#endif
        last_pic->next = NULL;
        ctx->pic_end = last_pic;

    } else {
        // Input is available for all pictures, so we don't need to
        // mangle anything.
    }

    av_log(avctx, AV_LOG_DEBUG, "Pictures ending truncated GOP:");
    for (pic = ctx->pic_start; pic; pic = pic->next) {
        av_log(avctx, AV_LOG_DEBUG, " %s (%"PRId64"/%"PRId64")",
               picture_type_name[pic->type],
               pic->display_order, pic->encode_order);
    }
    av_log(avctx, AV_LOG_DEBUG, "\n");

    return 0;
}
#ifdef VPG_DRIVER
static int vaapi_encode_clear_old(AVCodecContext *avctx)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodePicture *pic, *next;
    pic = ctx->pic_start;
    while (pic && pic->next) {
        if (pic->encode_order > ctx->output_order)
            break;

        if (pic->ref_count == 0 && pic == ctx->pic_start) {
            ctx->pic_start = pic->next;
            vaapi_encode_free(avctx, pic);
            pic = ctx->pic_start;
            continue;
        }
        next = pic->next;

        if (next->encode_order > ctx->output_order)
            break;
        if (next->ref_count == 0) {
            pic->next = next->next;
            vaapi_encode_free(avctx, next);
        }
        pic = pic->next;
    }
    return 0;
}
#else
static int vaapi_encode_clear_old(AVCodecContext *avctx)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodePicture *pic, *old;
    int i;

    while (ctx->pic_start != ctx->pic_end) {
        old = ctx->pic_start;
        if (old->encode_order > ctx->output_order)
            break;

        for (pic = old->next; pic; pic = pic->next) {
            if (pic->encode_complete)
                continue;
            for (i = 0; i < pic->nb_refs; i++) {
                if (pic->refs[i] == old) {
                    // We still need this picture because it's referred to
                    // directly by a later one, so it and all following
                    // pictures have to stay.
                    return 0;
                }
            }
        }

        pic = ctx->pic_start;
        ctx->pic_start = pic->next;
        vaapi_encode_free(avctx, pic);
    }

    return 0;
}
#endif
int ff_vaapi_encode2(AVCodecContext *avctx, AVPacket *pkt,
                     const AVFrame *input_image, int *got_packet)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodePicture *pic;
    int err;

    if (input_image) {
        av_log(avctx, AV_LOG_DEBUG, "Encode frame: %ux%u (%"PRId64").\n",
               input_image->width, input_image->height, input_image->pts);

        if (input_image->pict_type == AV_PICTURE_TYPE_I) {
            err = vaapi_encode_truncate_gop(avctx);
            if (err < 0)
                goto fail;
            ctx->force_idr = 1;
        }
        err = vaapi_encode_get_next(avctx, &pic);
        if (err) {
            av_log(avctx, AV_LOG_ERROR, "Input setup failed: %d.\n", err);
            return err;
        }

        pic->input_image = av_frame_alloc();
        if (!pic->input_image) {
            err = AVERROR(ENOMEM);
            goto fail;
        }
        err = av_frame_ref(pic->input_image, input_image);
        if (err < 0)
            goto fail;
        pic->input_surface = (VASurfaceID)(uintptr_t)input_image->data[3];
        pic->pts = input_image->pts;

        if (ctx->input_order == 0)
            ctx->first_pts = pic->pts;
        if (ctx->input_order == ctx->decode_delay)
            ctx->dts_pts_diff = pic->pts - ctx->first_pts;
        if (ctx->output_delay > 0)
            ctx->ts_ring[ctx->input_order % (3 * ctx->output_delay)] = pic->pts;

        pic->input_available = 1;

    } else {
        if (!ctx->end_of_stream) {
            err = vaapi_encode_truncate_gop(avctx);
            if (err < 0)
                goto fail;
            ctx->end_of_stream = 1;
        }
    }

    ++ctx->input_order;
    ++ctx->output_order;
    av_assert0(ctx->output_order + ctx->output_delay + 1 == ctx->input_order);

    for (pic = ctx->pic_start; pic; pic = pic->next)
        if (pic->encode_order == ctx->output_order)
            break;

    // pic can be null here if we don't have a specific target in this
    // iteration.  We might still issue encodes if things can be overlapped,
    // even though we don't intend to output anything.

    err = vaapi_encode_step(avctx, pic);
    if (err < 0) {
        av_log(avctx, AV_LOG_ERROR, "Encode failed: %d.\n", err);
        goto fail;
    }

    if (!pic) {
        *got_packet = 0;
    } else {
        err = vaapi_encode_output(avctx, pic, pkt);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Output failed: %d.\n", err);
            goto fail;
        }

        if (ctx->output_delay == 0) {
            pkt->dts = pkt->pts;
        } else if (ctx->output_order < ctx->decode_delay) {
            if (ctx->ts_ring[ctx->output_order] < INT64_MIN + ctx->dts_pts_diff)
                pkt->dts = INT64_MIN;
            else
                pkt->dts = ctx->ts_ring[ctx->output_order] - ctx->dts_pts_diff;
        } else {
            pkt->dts = ctx->ts_ring[(ctx->output_order - ctx->decode_delay) %
                                    (3 * ctx->output_delay)];
        }
#ifdef VPG_DRIVER
        if (ctx->bipyramid)
            pkt->dts -= avctx->max_b_frames;
#endif
        *got_packet = 1;
    }

    err = vaapi_encode_clear_old(avctx);
    if (err < 0) {
        av_log(avctx, AV_LOG_ERROR, "List clearing failed: %d.\n", err);
        goto fail;
    }

    return 0;

fail:
    // Unclear what to clean up on failure.  There are probably some things we
    // could do usefully clean up here, but for now just leave them for uninit()
    // to do instead.
    return err;
}

static av_cold int vaapi_encode_config_attributes(AVCodecContext *avctx)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAStatus vas;
    int i, n, err;
    VAProfile    *profiles    = NULL;
    VAEntrypoint *entrypoints = NULL;
    VAConfigAttrib attr[] = {
        { VAConfigAttribRTFormat         },
        { VAConfigAttribRateControl      },
        { VAConfigAttribEncMaxRefFrames  },
#ifdef VPG_DRIVER
        { VAConfigAttribEncIntraRefresh  },
        { VAConfigAttribEncROI           },
        { VAConfigAttribEncInterlaced    },
        { VAConfigAttribEncQuantization  },
#endif
#ifndef VPG_DRIVER
        { VAConfigAttribEncPackedHeaders },
#endif
    };

    n = vaMaxNumProfiles(ctx->hwctx->display);
    profiles = av_malloc_array(n, sizeof(VAProfile));
    if (!profiles) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    vas = vaQueryConfigProfiles(ctx->hwctx->display, profiles, &n);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(ctx, AV_LOG_ERROR, "Failed to query profiles: %d (%s).\n",
               vas, vaErrorStr(vas));
        err = AVERROR(ENOSYS);
        goto fail;
    }
    for (i = 0; i < n; i++) {
        if (profiles[i] == ctx->va_profile)
            break;
    }
    if (i >= n) {
        av_log(ctx, AV_LOG_ERROR, "Encoding profile not found (%d).\n",
               ctx->va_profile);
        err = AVERROR(ENOSYS);
        goto fail;
    }

    n = vaMaxNumEntrypoints(ctx->hwctx->display);
    entrypoints = av_malloc_array(n, sizeof(VAEntrypoint));
    if (!entrypoints) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    vas = vaQueryConfigEntrypoints(ctx->hwctx->display, ctx->va_profile,
                                   entrypoints, &n);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(ctx, AV_LOG_ERROR, "Failed to query entrypoints for "
               "profile %u: %d (%s).\n", ctx->va_profile,
               vas, vaErrorStr(vas));
        err = AVERROR(ENOSYS);
        goto fail;
    }
    for (i = 0; i < n; i++) {
        if (entrypoints[i] == ctx->va_entrypoint)
            break;
    }
    if (i >= n) {
        av_log(ctx, AV_LOG_ERROR, "Encoding entrypoint not found "
               "(%d / %d).\n", ctx->va_profile, ctx->va_entrypoint);
        err = AVERROR(ENOSYS);
        goto fail;
    }

    vas = vaGetConfigAttributes(ctx->hwctx->display,
                                ctx->va_profile, ctx->va_entrypoint,
                                attr, FF_ARRAY_ELEMS(attr));
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to fetch config "
               "attributes: %d (%s).\n", vas, vaErrorStr(vas));
        return AVERROR(EINVAL);
    }

    for (i = 0; i < FF_ARRAY_ELEMS(attr); i++) {
        if (attr[i].value == VA_ATTRIB_NOT_SUPPORTED) {
            // Unfortunately we have to treat this as "don't know" and hope
            // for the best, because the Intel MJPEG encoder returns this
            // for all the interesting attributes.
            continue;
        }
        switch (attr[i].type) {
        case VAConfigAttribRTFormat:
            if (!(ctx->va_rt_format & attr[i].value)) {
                av_log(avctx, AV_LOG_ERROR, "Surface RT format %#x "
                       "is not supported (mask %#x).\n",
                       ctx->va_rt_format, attr[i].value);
                err = AVERROR(EINVAL);
                goto fail;
            }
            ctx->config_attributes[ctx->nb_config_attributes++] =
                (VAConfigAttrib) {
                .type  = VAConfigAttribRTFormat,
                .value = ctx->va_rt_format,
            };
            break;
        case VAConfigAttribRateControl:
            // Hack for backward compatibility: CBR was the only
            // usable RC mode for a long time, so old drivers will
            // only have it.  Normal default options may now choose
            // VBR and then fail, however, so override it here with
            // CBR if that is the only supported mode.
#ifdef VPG_DRIVER
            if (ctx->va_rc_mode == VA_RC_VBR &&
                !(attr[i].value & VA_RC_VBR) &&
                (attr[i].value & VA_RC_CBR)  &&
                ctx->va_profile != VAProfileJPEGBaseline) {
#else
            if (ctx->va_rc_mode == VA_RC_VBR &&
                !(attr[i].value & VA_RC_VBR) &&
                (attr[i].value & VA_RC_CBR)) {
#endif
                av_log(avctx, AV_LOG_WARNING, "VBR rate control is "
                       "not supported with this driver version; "
                       "using CBR instead.\n");
                ctx->va_rc_mode = VA_RC_CBR;
            }
#ifdef VPG_DRIVER
            if (ctx->va_profile != VAProfileJPEGBaseline)
#endif
            ctx->config_attributes[ctx->nb_config_attributes++] =
                (VAConfigAttrib) {
                .type  = VAConfigAttribRateControl,
                .value = ctx->va_rc_mode,
            };
            break;
        case VAConfigAttribEncMaxRefFrames:
        {
            unsigned int ref_l0 = attr[i].value & 0xffff;
            unsigned int ref_l1 = (attr[i].value >> 16) & 0xffff;

            if (avctx->gop_size > 1 && ref_l0 < 1) {
                av_log(avctx, AV_LOG_ERROR, "P frames are not "
                       "supported (%#x).\n", attr[i].value);
                err = AVERROR(EINVAL);
                goto fail;
            }
            if (avctx->max_b_frames > 0 && ref_l1 < 1) {
                av_log(avctx, AV_LOG_ERROR, "B frames are not "
                       "supported (%#x).\n", attr[i].value);
                err = AVERROR(EINVAL);
                goto fail;
            }
        }
        break;
        case VAConfigAttribEncPackedHeaders:
            if (ctx->va_packed_headers & ~attr[i].value) {
                // This isn't fatal, but packed headers are always
                // preferable because they are under our control.
                // When absent, the driver is generating them and some
                // features may not work (e.g. VUI or SEI in H.264).
                av_log(avctx, AV_LOG_WARNING, "Warning: some packed "
                       "headers are not supported (want %#x, got %#x).\n",
                       ctx->va_packed_headers, attr[i].value);
                ctx->va_packed_headers &= attr[i].value;
            }
            ctx->config_attributes[ctx->nb_config_attributes++] =
                (VAConfigAttrib) {
                .type  = VAConfigAttribEncPackedHeaders,
                .value = ctx->va_packed_headers,
            };
            break;
#ifdef VPG_DRIVER
        case VAConfigAttribEncIntraRefresh:
            if (attr[i].value & (~VA_ATTRIB_NOT_SUPPORTED)) {
                ctx->support_rir = 1;
            }
            break;
        case VAConfigAttribEncROI:
            if (attr[i].value & (~VA_ATTRIB_NOT_SUPPORTED)) {
                VAConfigAttribValEncROI *VaEncROIValPtr = (VAConfigAttribValEncROI *)(&attr[i].value);
                int maxNumRoi, roiBrcPriorityLevelSupport;

                maxNumRoi = VaEncROIValPtr->bits.num_roi_regions;
                roiBrcPriorityLevelSupport = VaEncROIValPtr->bits.roi_rc_priority_support;
                ctx->support_roi = 1;
                av_log(NULL, AV_LOG_INFO, "roi is supported，maxNumRoi = %d, roiBrcPriorityLevelSupport = %d\n",
                      maxNumRoi, roiBrcPriorityLevelSupport);
            }
            break;
        case VAConfigAttribEncQuantization:
            if (attr[i].value & (~VA_ATTRIB_NOT_SUPPORTED)) {
                ctx->support_trellis = 1;
            }
            break;
        case VAConfigAttribEncInterlaced:
            if (attr[i].value & (~VA_ATTRIB_NOT_SUPPORTED)) {
                ctx->support_interlaced = 1;
            }
            break;
#endif
        default:
            av_assert0(0 && "Unexpected config attribute.");
        }
    }

    err = 0;
fail:
    av_freep(&profiles);
    av_freep(&entrypoints);
    return err;
}

static av_cold int vaapi_encode_init_rate_control(AVCodecContext *avctx)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    int rc_bits_per_second;
    int rc_target_percentage;
    int rc_window_size;
    int hrd_buffer_size;
    int hrd_initial_buffer_fullness;

    if (avctx->bit_rate > INT32_MAX) {
        av_log(avctx, AV_LOG_ERROR, "Target bitrate of 2^31 bps or "
               "higher is not supported.\n");
        return AVERROR(EINVAL);
    }

    if (avctx->rc_buffer_size)
        hrd_buffer_size = avctx->rc_buffer_size;
    else
        hrd_buffer_size = avctx->bit_rate * 2;
    if (avctx->rc_initial_buffer_occupancy)
        hrd_initial_buffer_fullness = avctx->rc_initial_buffer_occupancy;
    else
        hrd_initial_buffer_fullness = hrd_buffer_size / 2;

    if (ctx->va_rc_mode == VA_RC_CBR) {
        avctx->rc_max_rate = avctx->bit_rate;
        rc_bits_per_second   = avctx->bit_rate;
        rc_target_percentage = 100;
        rc_window_size       = 1000;
    } else {
        if (avctx->rc_max_rate < avctx->bit_rate) {
            // Max rate is unset or invalid, just use the normal bitrate.
            avctx->rc_max_rate = avctx->bit_rate * 3 / 2;
            rc_bits_per_second   = avctx->bit_rate;
            rc_target_percentage = 100;
        } else {
            rc_bits_per_second   = avctx->rc_max_rate;
            rc_target_percentage = (avctx->bit_rate * 100) / rc_bits_per_second;
        }
        rc_window_size = (hrd_buffer_size * 1000) / avctx->bit_rate;
    }

    ctx->rc_params.misc.type = VAEncMiscParameterTypeRateControl;
    ctx->rc_params.rc = (VAEncMiscParameterRateControl) {
        .bits_per_second   = rc_bits_per_second,
        .target_percentage = rc_target_percentage,
        .window_size       = rc_window_size,
        .initial_qp        = (avctx->qmax >= 0 ? avctx->qmax : 40),
        .min_qp            = (avctx->qmin >= 0 ? avctx->qmin : 18),
#ifdef VPG_DRIVER
        .max_qp            = (avctx->qmax >= 0 ? avctx->qmax : 51),
#endif
        .basic_unit_size   = 0,
    };
    ctx->global_params[ctx->nb_global_params] =
        &ctx->rc_params.misc;
    ctx->global_params_size[ctx->nb_global_params++] =
        sizeof(ctx->rc_params);

    ctx->hrd_params.misc.type = VAEncMiscParameterTypeHRD;
    ctx->hrd_params.hrd = (VAEncMiscParameterHRD) {
        .initial_buffer_fullness = hrd_initial_buffer_fullness,
        .buffer_size             = hrd_buffer_size,
    };
    ctx->global_params[ctx->nb_global_params] =
        &ctx->hrd_params.misc;
    ctx->global_params_size[ctx->nb_global_params++] =
        sizeof(ctx->hrd_params);

#if VA_CHECK_VERSION(0, 40, 0)
    ctx->fr_params.misc.type = VAEncMiscParameterTypeFrameRate;
    if (avctx->framerate.num > 0 && avctx->framerate.den > 0) {
        ctx->fr_params.fr.framerate = 100 * avctx->framerate.num / avctx->framerate.den;
    } else {
        ctx->fr_params.fr.framerate = 100 * avctx->time_base.den / avctx->time_base.num;
    }
    ctx->global_params[ctx->nb_global_params] =
        &ctx->fr_params.misc;
    ctx->global_params_size[ctx->nb_global_params++] =
        sizeof(ctx->fr_params);
#endif
    return 0;
}

static void vaapi_encode_free_output_buffer(void *opaque,
                                            uint8_t *data)
{
    AVCodecContext   *avctx = opaque;
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VABufferID buffer_id;

    buffer_id = (VABufferID)(uintptr_t)data;

    vaDestroyBuffer(ctx->hwctx->display, buffer_id);

    av_log(avctx, AV_LOG_DEBUG, "Freed output buffer %#x\n", buffer_id);
}

static AVBufferRef *vaapi_encode_alloc_output_buffer(void *opaque,
                                                     int size)
{
    AVCodecContext   *avctx = opaque;
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VABufferID buffer_id;
    VAStatus vas;
    AVBufferRef *ref;

    // The output buffer size is fixed, so it needs to be large enough
    // to hold the largest possible compressed frame.  We assume here
    // that the uncompressed frame plus some header data is an upper
    // bound on that.
    vas = vaCreateBuffer(ctx->hwctx->display, ctx->va_context,
                         VAEncCodedBufferType,
                         3 * ctx->surface_width * ctx->surface_height +
                         (1 << 16), 1, 0, &buffer_id);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create bitstream "
               "output buffer: %d (%s).\n", vas, vaErrorStr(vas));
        return NULL;
    }

    av_log(avctx, AV_LOG_DEBUG, "Allocated output buffer %#x\n", buffer_id);

    ref = av_buffer_create((uint8_t*)(uintptr_t)buffer_id,
                           sizeof(buffer_id),
                           &vaapi_encode_free_output_buffer,
                           avctx, AV_BUFFER_FLAG_READONLY);
    if (!ref) {
        vaDestroyBuffer(ctx->hwctx->display, buffer_id);
        return NULL;
    }

    return ref;
}

static av_cold int vaapi_encode_create_recon_frames(AVCodecContext *avctx)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    AVVAAPIHWConfig *hwconfig = NULL;
    AVHWFramesConstraints *constraints = NULL;
    enum AVPixelFormat recon_format;
    int err, i;

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

    // Probably we can use the input surface format as the surface format
    // of the reconstructed frames.  If not, we just pick the first (only?)
    // format in the valid list and hope that it all works.
    recon_format = AV_PIX_FMT_NONE;
    if (constraints->valid_sw_formats) {
        for (i = 0; constraints->valid_sw_formats[i] != AV_PIX_FMT_NONE; i++) {
            if (ctx->input_frames->sw_format ==
                constraints->valid_sw_formats[i]) {
                recon_format = ctx->input_frames->sw_format;
                break;
            }
        }
        if (recon_format == AV_PIX_FMT_NONE) {
            // No match.  Just use the first in the supported list and
            // hope for the best.
            recon_format = constraints->valid_sw_formats[0];
        }
    } else {
        // No idea what to use; copy input format.
        recon_format = ctx->input_frames->sw_format;
    }
    av_log(avctx, AV_LOG_DEBUG, "Using %s as format of "
           "reconstructed frames.\n", av_get_pix_fmt_name(recon_format));

    if (ctx->surface_width  < constraints->min_width  ||
        ctx->surface_height < constraints->min_height ||
        ctx->surface_width  > constraints->max_width ||
        ctx->surface_height > constraints->max_height) {
        av_log(avctx, AV_LOG_ERROR, "Hardware does not support encoding at "
               "size %dx%d (constraints: width %d-%d height %d-%d).\n",
               ctx->surface_width, ctx->surface_height,
               constraints->min_width,  constraints->max_width,
               constraints->min_height, constraints->max_height);
        err = AVERROR(EINVAL);
        goto fail;
    }

    av_freep(&hwconfig);
    av_hwframe_constraints_free(&constraints);

    ctx->recon_frames_ref = av_hwframe_ctx_alloc(ctx->device_ref);
    if (!ctx->recon_frames_ref) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    ctx->recon_frames = (AVHWFramesContext*)ctx->recon_frames_ref->data;

    ctx->recon_frames->format    = AV_PIX_FMT_VAAPI;
    ctx->recon_frames->sw_format = recon_format;
    ctx->recon_frames->width     = ctx->surface_width;
    ctx->recon_frames->height    = ctx->surface_height;
    // At most three IDR/I/P frames and two runs of B frames can be in
    // flight at any one time.
    ctx->recon_frames->initial_pool_size = 3 + 2 * avctx->max_b_frames;

#ifdef VPG_DRIVER
    ctx->recon_frames->initial_pool_size += ctx->max_ref_nr - 1;
#endif
    err = av_hwframe_ctx_init(ctx->recon_frames_ref);
    if (err < 0) {
        av_log(avctx, AV_LOG_ERROR, "Failed to initialise reconstructed "
               "frame context: %d.\n", err);
        goto fail;
    }

    err = 0;
  fail:
    av_freep(&hwconfig);
    av_hwframe_constraints_free(&constraints);
    return err;
}

av_cold int ff_vaapi_encode_init(AVCodecContext *avctx)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    AVVAAPIFramesContext *recon_hwctx = NULL;
    VAStatus vas;
    int err;

    if (!avctx->hw_frames_ctx) {
        av_log(avctx, AV_LOG_ERROR, "A hardware frames reference is "
               "required to associate the encoding device.\n");
        return AVERROR(EINVAL);
    }

    ctx->codec_options = ctx->codec_options_data;

    ctx->va_config  = VA_INVALID_ID;
    ctx->va_context = VA_INVALID_ID;

    ctx->priv_data = av_mallocz(ctx->codec->priv_data_size);
    if (!ctx->priv_data) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    ctx->input_frames_ref = av_buffer_ref(avctx->hw_frames_ctx);
    if (!ctx->input_frames_ref) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    ctx->input_frames = (AVHWFramesContext*)ctx->input_frames_ref->data;

    ctx->device_ref = av_buffer_ref(ctx->input_frames->device_ref);
    if (!ctx->device_ref) {
        err = AVERROR(ENOMEM);
        goto fail;
    }
    ctx->device = (AVHWDeviceContext*)ctx->device_ref->data;
    ctx->hwctx = ctx->device->hwctx;

    err = vaapi_encode_config_attributes(avctx);
    if (err < 0)
        goto fail;

    vas = vaCreateConfig(ctx->hwctx->display,
                         ctx->va_profile, ctx->va_entrypoint,
                         ctx->config_attributes, ctx->nb_config_attributes,
                         &ctx->va_config);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create encode pipeline "
               "configuration: %d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail;
    }

    err = vaapi_encode_create_recon_frames(avctx);
    if (err < 0)
        goto fail;

    recon_hwctx = ctx->recon_frames->hwctx;
    vas = vaCreateContext(ctx->hwctx->display, ctx->va_config,
                          ctx->surface_width, ctx->surface_height,
                          VA_PROGRESSIVE,
                          recon_hwctx->surface_ids,
                          recon_hwctx->nb_surfaces,
                          &ctx->va_context);
    if (vas != VA_STATUS_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create encode pipeline "
               "context: %d (%s).\n", vas, vaErrorStr(vas));
        err = AVERROR(EIO);
        goto fail;
    }

    ctx->output_buffer_pool =
        av_buffer_pool_init2(sizeof(VABufferID), avctx,
                             &vaapi_encode_alloc_output_buffer, NULL);
    if (!ctx->output_buffer_pool) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    if (ctx->va_rc_mode & ~VA_RC_CQP) {
        err = vaapi_encode_init_rate_control(avctx);
        if (err < 0)
            goto fail;
    }

    if (ctx->codec->configure) {
        err = ctx->codec->configure(avctx);
        if (err < 0)
            goto fail;
    }

    ctx->input_order  = 0;
    ctx->output_delay = avctx->max_b_frames;
    ctx->decode_delay = 1;
    ctx->output_order = - ctx->output_delay - 1;
#ifndef VPG_DRIVER
    // Currently we never generate I frames, only IDR.
    ctx->i_per_idr = 0;
#endif
    ctx->p_per_i = ((avctx->gop_size - 1 + avctx->max_b_frames) /
                    (avctx->max_b_frames + 1));
    ctx->b_per_p = avctx->max_b_frames;

    if (ctx->codec->sequence_params_size > 0) {
        ctx->codec_sequence_params =
            av_mallocz(ctx->codec->sequence_params_size);
        if (!ctx->codec_sequence_params) {
            err = AVERROR(ENOMEM);
            goto fail;
        }
    }
    if (ctx->codec->picture_params_size > 0) {
        ctx->codec_picture_params =
            av_mallocz(ctx->codec->picture_params_size);
        if (!ctx->codec_picture_params) {
            err = AVERROR(ENOMEM);
            goto fail;
        }
    }

    if (ctx->codec->init_sequence_params) {
        err = ctx->codec->init_sequence_params(avctx);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Codec sequence initialisation "
                   "failed: %d.\n", err);
            goto fail;
        }
    }

    // This should be configurable somehow.  (Needs testing on a machine
    // where it actually overlaps properly, though.)
    ctx->issue_mode = ISSUE_MODE_MAXIMISE_THROUGHPUT;

    if (ctx->va_packed_headers & VA_ENC_PACKED_HEADER_SEQUENCE &&
        ctx->codec->write_sequence_header) {
        char data[MAX_PARAM_BUFFER_SIZE];
        size_t bit_len = 8 * sizeof(data);

        err = ctx->codec->write_sequence_header(avctx, data, &bit_len);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to write sequence header "
                   "for extradata: %d.\n", err);
            goto fail;
        } else {
            avctx->extradata_size = (bit_len + 7) / 8;
            avctx->extradata = av_mallocz(avctx->extradata_size +
                                          AV_INPUT_BUFFER_PADDING_SIZE);
            if (!avctx->extradata) {
                err = AVERROR(ENOMEM);
                goto fail;
            }
            memcpy(avctx->extradata, data, avctx->extradata_size);
        }
    }

    return 0;

fail:
    ff_vaapi_encode_close(avctx);
    return err;
}

av_cold int ff_vaapi_encode_close(AVCodecContext *avctx)
{
    VAAPIEncodeContext *ctx = avctx->priv_data;
    VAAPIEncodePicture *pic, *next;
#ifdef VPG_DRIVER
    int i;
    for (i = 0 ; i < ctx->ref_nr; i++) {
        if (!ctx->references[i])
            continue;
        ctx->references[i]->ref_count --;
        ctx->references[i] = NULL;
    }
    ctx->ref_nr = 0;
#endif
    for (pic = ctx->pic_start; pic; pic = next) {
        next = pic->next;
        vaapi_encode_free(avctx, pic);
    }

    if (ctx->va_context != VA_INVALID_ID) {
        vaDestroyContext(ctx->hwctx->display, ctx->va_context);
        ctx->va_context = VA_INVALID_ID;
    }

    if (ctx->va_config != VA_INVALID_ID) {
        vaDestroyConfig(ctx->hwctx->display, ctx->va_config);
        ctx->va_config = VA_INVALID_ID;
    }

    av_buffer_pool_uninit(&ctx->output_buffer_pool);

    av_freep(&ctx->codec_sequence_params);
    av_freep(&ctx->codec_picture_params);

    av_buffer_unref(&ctx->recon_frames_ref);
    av_buffer_unref(&ctx->input_frames_ref);
    av_buffer_unref(&ctx->device_ref);

    av_freep(&ctx->priv_data);

    return 0;
}
