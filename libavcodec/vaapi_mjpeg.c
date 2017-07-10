/*
 * This file is part of FFmpeg.
 *
 * ffmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * ffmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with ffmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "mjpegdec.h"
#include "vaapi_decode.h"

static int vaapi_mjpeg_start_frame(AVCodecContext          *avctx,
                                 av_unused const uint8_t *buffer,
                                 av_unused uint32_t       size)
{
    const MJpegDecodeContext *s = avctx->priv_data;
    VAAPIDecodePicture *pic = s->hwaccel_picture_private;
    VAPictureParameterBufferJPEGBaseline pic_param;
    VAPictureParameterBufferJPEGBaseline *pp;
    VAHuffmanTableBufferJPEGBaseline pic_huffman_table;
    VAHuffmanTableBufferJPEGBaseline *huffman_table;
    VAIQMatrixBufferJPEGBaseline pic_iq_matrix;
    VAIQMatrixBufferJPEGBaseline *iq_matrix;
    int i, j, num_quant_table, num_huffman_table;
    int err;

    pic->output_surface = ff_vaapi_get_surface_id(s->picture_ptr);
    memset(&pic_param, 0, sizeof(VAPictureParameterBufferJPEGBaseline));

    pp = &pic_param;
    pp->picture_width = s->width;
    pp->picture_height = s->height;
    pp->num_components = s->nb_components;
#ifdef VPG_DRIVER
    pp->rotation = 0;
#endif
    for (i = 0; i < pp->num_components; i++) {
        pp->components[i].component_id = s->component_id[i];
        pp->components[i].h_sampling_factor = s->h_count[i];
        pp->components[i].v_sampling_factor = s->v_count[i];
        pp->components[i].quantiser_table_selector = s->quant_index[i];
    }

    err = ff_vaapi_decode_make_param_buffer(avctx, pic,
                                            VAPictureParameterBufferType,
                                            &pic_param, sizeof(pic_param));
	if (err)
		goto fail;
    iq_matrix = &pic_iq_matrix;
    memset(iq_matrix, 0, sizeof(VAIQMatrixBufferJPEGBaseline));
    num_quant_table = sizeof(iq_matrix->load_quantiser_table)/sizeof(iq_matrix->load_quantiser_table[0]);
    for (i = 0; i < num_quant_table; i++) {
        if (!s->q_tables_valid[i])
            continue;
        iq_matrix->load_quantiser_table[i] = 1;
        for (j = 0; j < 64; j++)
            iq_matrix->quantiser_table[i][j] = s->quant_matrixes[i][j];
    }
    
    err = ff_vaapi_decode_make_param_buffer(avctx, pic,
                                            VAIQMatrixBufferType,
                                            iq_matrix, sizeof(pic_iq_matrix));
	if (err < 0)
		goto fail;
    huffman_table = &pic_huffman_table;
    memset(huffman_table, 0, sizeof(VAHuffmanTableBufferJPEGBaseline));
    num_huffman_table = sizeof(huffman_table->load_huffman_table) / sizeof(huffman_table->load_huffman_table[0]);

    for (i = 0; i < num_huffman_table; i++) {
        if (!s->htdc_valid[i] || !s->htac_valid[i])
            continue;
        huffman_table->load_huffman_table[i] = 1;
        memcpy(huffman_table->huffman_table[i].num_dc_codes, s->htdc_bits[i],
                sizeof(huffman_table->huffman_table[i].num_dc_codes));
        memcpy(huffman_table->huffman_table[i].dc_values, s->htdc_value[i],
                sizeof(huffman_table->huffman_table[i].dc_values));
        memcpy(huffman_table->huffman_table[i].num_ac_codes, s->htac_bits[i],
                sizeof(huffman_table->huffman_table[i].num_ac_codes));
        memcpy(huffman_table->huffman_table[i].ac_values, s->htac_value[i],
                sizeof(huffman_table->huffman_table[i].ac_values));
        memset(huffman_table->huffman_table[i].pad, 0,
                sizeof(huffman_table->huffman_table[i].pad));
    }
    err = ff_vaapi_decode_make_param_buffer(avctx, pic,
    		                                VAHuffmanTableBufferType,
                                            huffman_table, sizeof(pic_huffman_table));
	if (err < 0)
		goto fail;

    return 0;
fail:
	ff_vaapi_decode_cancel(avctx, pic);
	return err;
}

static int vaapi_mjpeg_end_frame(AVCodecContext *avctx)
{
	const MJpegDecodeContext *h = avctx->priv_data;
	VAAPIDecodePicture *pic = h->hwaccel_picture_private;
	int ret;

	ret = ff_vaapi_decode_issue(avctx, pic);
	if (ret < 0)
		goto finish;

finish:
	return ret;
}

static int vaapi_mjpeg_decode_slice(AVCodecContext *avctx,
                                  const uint8_t  *buffer,
                                  uint32_t        size)
{
    const MJpegDecodeContext *s = avctx->priv_data;
    VAAPIDecodePicture *pic = s->hwaccel_picture_private;
    VASliceParameterBufferJPEGBaseline pic_sp;
    VASliceParameterBufferJPEGBaseline *sp;
    int i, err;

    sp = &pic_sp;
    sp->slice_data_offset = 0;
    sp->slice_data_flag = VA_SLICE_DATA_FLAG_ALL;
    sp->slice_horizontal_position = 0;
    sp->slice_vertical_position = 0;
    sp->num_components = s->nb_components;
    for (i = 0; i < sp->num_components; i++) {
        sp->components[i].component_selector = s->component_id[i];
        sp->components[i].dc_table_selector = s->dc_index[i];
        sp->components[i].ac_table_selector = s->ac_index[i];
    }
    sp->restart_interval = s->restart_interval;
    sp->num_mcus = s->mb_width * s->mb_height;
    err = ff_vaapi_decode_make_slice_buffer(avctx, pic,
                                            sp, sizeof(pic_sp),
                                            buffer, size);
    if (err) {
        ff_vaapi_decode_cancel(avctx, pic);
        return err;
    }
    return 0;
}

AVHWAccel ff_mjpeg_vaapi_hwaccel = {
    .name                 = "mjpeg_vaapi",
    .type                 = AVMEDIA_TYPE_VIDEO,
    .id                   = AV_CODEC_ID_MJPEG,
    .pix_fmt              = AV_PIX_FMT_VAAPI,
    .start_frame          = &vaapi_mjpeg_start_frame,
    .end_frame            = &vaapi_mjpeg_end_frame,
    .decode_slice         = &vaapi_mjpeg_decode_slice,
    .frame_priv_data_size = sizeof(VAAPIDecodePicture),
    .init                 = &ff_vaapi_decode_init,
    .uninit               = &ff_vaapi_decode_uninit,
    .priv_data_size       = sizeof(VAAPIDecodePicture),
};
