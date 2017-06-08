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

#ifndef AVFILTER_QSVVPP_H
#define AVFILTER_QSVVPP_H

#include "avfilter.h"
#include <mfx/mfxvideo.h>

typedef struct FFQSVVPPContext FFQSVVPPContext;

/* for callback function */
typedef int (*qsvvpp_post_proc) (AVFilterLink *outlink, AVFrame *frame);
typedef struct FFQSVVPPParam {
    qsvvpp_post_proc cb;
    mfxVideoParam    vpp_param;
} FFQSVVPPParam;

/* blew APIs are abstracted to be called by different QSV VPP filters,
 * most codes of different QSV filters (scale/deinterlace/framerate/overlay) are nearly
 * the same so use the common APIs to reduce code redundancy */

/* create and initialize the QSV session*/
int ff_qsvvpp_create(AVFilterContext *avctx, FFQSVVPPContext **vpp, FFQSVVPPParam *param);

/* release the resources eg.surfaces */
int ff_qsvvpp_free(FFQSVVPPContext **vpp);

/* vpp filter frame and call the cb if needed */
int ff_qsvvpp_filter_frame(FFQSVVPPContext *vpp, AVFilterLink *inlink, AVFrame *frame);

/* fill the mfxFrameInfo */
int ff_qsvvpp_frameinfo_fill(mfxFrameInfo *frameinfo, AVFilterLink *link, int out);


#endif /* QSVVPP_H */
