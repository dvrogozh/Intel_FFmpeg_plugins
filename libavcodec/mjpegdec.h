/*
 * MJPEG decoder
 * Copyright (c) 2000, 2001 Fabrice Bellard
 * Copyright (c) 2003 Alex Beregszaszi
 * Copyright (c) 2003-2004 Michael Niedermayer
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
 * MJPEG decoder.
 */

#ifndef AVCODEC_MJPEGDEC_H
#define AVCODEC_MJPEGDEC_H

#include "libavutil/log.h"
#include "libavutil/pixdesc.h"
#include "libavutil/stereo3d.h"

#include "avcodec.h"
#include "blockdsp.h"
#include "get_bits.h"
#include "hpeldsp.h"
#include "idctdsp.h"

#define MAX_COMPONENTS 4

/**
 * Picture.
 */
typedef struct Picture {
    struct AVFrame *f;

    AVBufferRef *qscale_table_buf;
    int8_t *qscale_table;

    AVBufferRef *motion_val_buf[2];
    int16_t (*motion_val[2])[2];

    AVBufferRef *mb_type_buf;
    uint32_t *mb_type;          ///< types and macros are defined in mpegutils.h

    AVBufferRef *mbskip_table_buf;
    uint8_t *mbskip_table;

    AVBufferRef *ref_index_buf[2];
    int8_t *ref_index[2];

    AVBufferRef *mb_var_buf;
    uint16_t *mb_var;           ///< Table for MB variances

    AVBufferRef *mc_mb_var_buf;
    uint16_t *mc_mb_var;        ///< Table for motion compensated MB variances

    int alloc_mb_width;         ///< mb_width used to allocate tables
    int alloc_mb_height;        ///< mb_height used to allocate tables

    AVBufferRef *mb_mean_buf;
    uint8_t *mb_mean;           ///< Table for MB luminance

    AVBufferRef *hwaccel_priv_buf;
    void *hwaccel_picture_private; ///< Hardware accelerator private data

    int field_picture;          ///< whether or not the picture was encoded in separate fields

    int64_t mb_var_sum;         ///< sum of MB variance for current frame
    int64_t mc_mb_var_sum;      ///< motion compensated MB variance for current frame

    int b_frame_score;
    int needs_realloc;          ///< Picture needs to be reallocated (eg due to a frame size change)

    int reference;
    int shared;

    uint64_t encoding_error[AV_NUM_DATA_POINTERS];
} Picture;

typedef struct MJpegDecodeContext {
    AVClass *class;
    AVCodecContext *avctx;
    GetBitContext gb;

    int start_code; /* current start code */
    int buffer_size;
    uint8_t *buffer;
    uint16_t quant_matrixes[4][64];
    int16_t q_tables_valid[4];
    uint8_t htdc_valid[4];
    uint8_t htac_valid[4];
    uint8_t htdc_bits[4][16];
    uint8_t htac_bits[4][16];
    uint8_t htdc_value[4][256];
    uint8_t htac_value[4][256];

    VLC vlcs[3][4];
    int qscale[4];      ///< quantizer scale calculated from quant_matrixes

    int org_height;  /* size given at codec init */
    int first_picture;    /* true if decoding first picture */
    int interlaced;     /* true if interlaced */
    int bottom_field;   /* true if bottom field */
    int lossless;
    int ls;
    int progressive;
    int rgb;
    uint8_t upscale_h[4];
    uint8_t upscale_v[4];
    int rct;            /* standard rct */
    int pegasus_rct;    /* pegasus reversible colorspace transform */
    int bits;           /* bits per component */
    int colr;
    int xfrm;
    int adobe_transform;

    int maxval;
    int near;         ///< near lossless bound (si 0 for lossless)
    int t1,t2,t3;
    int reset;        ///< context halfing interval ?rename

    int width, height;
    int mb_width, mb_height;
    int nb_components;
    int block_stride[MAX_COMPONENTS];
    int component_id[MAX_COMPONENTS];
    int h_count[MAX_COMPONENTS]; /* horizontal and vertical count for each component */
    int v_count[MAX_COMPONENTS];
    int comp_index[MAX_COMPONENTS];
    int dc_index[MAX_COMPONENTS];
    int ac_index[MAX_COMPONENTS];
    int nb_blocks[MAX_COMPONENTS];
    int h_scount[MAX_COMPONENTS];
    int v_scount[MAX_COMPONENTS];
    int quant_sindex[MAX_COMPONENTS];
    int h_max, v_max; /* maximum h and v counts */
    int quant_index[4];   /* quant table index for each component */
    int last_dc[MAX_COMPONENTS]; /* last DEQUANTIZED dc (XXX: am I right to do that ?) */
    AVFrame *picture; /* picture structure */
    AVFrame *picture_ptr; /* pointer to picture structure */
    int got_picture;                                ///< we found a SOF and picture is valid, too.
    int linesize[MAX_COMPONENTS];                   ///< linesize << interlaced
    int8_t *qscale_table;
    DECLARE_ALIGNED(16, int16_t, block)[64];
    int16_t (*blocks[MAX_COMPONENTS])[64]; ///< intermediate sums (progressive mode)
    uint8_t *last_nnz[MAX_COMPONENTS];
    uint64_t coefs_finished[MAX_COMPONENTS]; ///< bitmask of which coefs have been completely decoded (progressive mode)
    int palette_index;
    ScanTable scantable;
    BlockDSPContext bdsp;
    HpelDSPContext hdsp;
    IDCTDSPContext idsp;
    Picture *current_picture_ptr;    ///< buffer to store the decompressed current picture

    int restart_interval;
    int restart_count;

    int buggy_avid;
    int cs_itu601;
    int interlace_polarity;
    int multiscope;

    int mjpb_skiptosod;

    int cur_scan; /* current scan, used by JPEG-LS */
    int flipped; /* true if picture is flipped */

    uint16_t (*ljpeg_buffer)[4];
    unsigned int ljpeg_buffer_size;

    int extern_huff;
    AVDictionary *exif_metadata;

    AVStereo3D *stereo3d; ///!< stereoscopic information (cached, since it is read before frame allocation)

    const AVPixFmtDescriptor *pix_desc;
    enum AVPixelFormat pix_fmt;
} MJpegDecodeContext;

int ff_mjpeg_decode_init(AVCodecContext *avctx);
int ff_mjpeg_decode_end(AVCodecContext *avctx);
int ff_mjpeg_decode_frame(AVCodecContext *avctx,
                          void *data, int *got_frame,
                          AVPacket *avpkt);
int ff_mjpeg_decode_dqt(MJpegDecodeContext *s);
int ff_mjpeg_decode_dht(MJpegDecodeContext *s);
int ff_mjpeg_decode_sof(MJpegDecodeContext *s);
int ff_mjpeg_decode_sos(MJpegDecodeContext *s,
                        const uint8_t *mb_bitmask,int mb_bitmask_size,
                        const AVFrame *reference);
int ff_mjpeg_find_marker(MJpegDecodeContext *s,
                         const uint8_t **buf_ptr, const uint8_t *buf_end,
                         const uint8_t **unescaped_buf_ptr, int *unescaped_buf_size);

#endif /* AVCODEC_MJPEGDEC_H */
