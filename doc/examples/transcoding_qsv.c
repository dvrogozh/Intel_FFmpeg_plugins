/*
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/**
 * @file
 * API example for demuxing, decoding, filtering, encoding and muxing using
 * Intel QSV.
 * @example transcoding_qsv.c
 */

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavfilter/avfiltergraph.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavutil/opt.h>
#include <libavutil/pixdesc.h>
#include <libavutil/hwcontext.h>
#include <libavutil/hwcontext_qsv.h>
#include <libavutil/avstring.h>

#define find_x_by_codec_id(_x_) \
static const AVCodec *find_ ## _x_ ## _by_codec_id(enum AVCodecID codec_id)\
{\
    const char *codec_name = NULL;\
    switch (codec_id) {\
        case AV_CODEC_ID_H264:\
            codec_name = "h264_qsv";\
            break;\
        case AV_CODEC_ID_MPEG2VIDEO:\
            codec_name = "mpeg2_qsv";\
            break;\
        case AV_CODEC_ID_H265:\
            codec_name = "hevc_qsv";\
            break;\
        default:\
            break;\
    }\
\
    if (codec_name)\
        return avcodec_find_ ## _x_ ## _by_name(codec_name);\
\
    return avcodec_find_##_x_(codec_id);\
}

static AVFormatContext *ifmt_ctx;
static AVFormatContext *ofmt_ctx;
typedef struct FilteringContext {
    AVCodecContext  *dec_ctx;
    AVCodecContext  *enc_ctx;
    AVFilterContext *buffersink_ctx;
    AVFilterContext *buffersrc_ctx;
    AVFilterGraph *filter_graph;
    int initiallized;
} FilteringContext;
static FilteringContext *filter_ctx;
static AVBufferRef *g_device;

find_x_by_codec_id(decoder);
find_x_by_codec_id(encoder);

static int init_filters(void)
{
    unsigned int i;

    filter_ctx = av_malloc_array(ifmt_ctx->nb_streams, sizeof(*filter_ctx));
    if (!filter_ctx)
        return AVERROR(ENOMEM);

    for (i = 0; i < ifmt_ctx->nb_streams; i++) {
        filter_ctx[i].dec_ctx = NULL;
        filter_ctx[i].enc_ctx = NULL;
        filter_ctx[i].buffersrc_ctx  = NULL;
        filter_ctx[i].buffersink_ctx = NULL;
        filter_ctx[i].filter_graph   = NULL;
        filter_ctx[i].initiallized   = 0;

        if (ifmt_ctx->streams[i]->codecpar->codec_type == AVMEDIA_TYPE_VIDEO)
            continue;
        filter_ctx[i].initiallized   = 1;
    }
    return 0;
}

static enum AVPixelFormat get_format(AVCodecContext *s,
                                     const enum AVPixelFormat *pix_fmts)
{
    const enum AVPixelFormat *p;
    int ret;
    AVHWFramesContext *frames_ctx;
    AVQSVFramesContext *frames_hwctx;

    for (p = pix_fmts; *p != -1; p++) {
        const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(*p);
        if (!(desc->flags & AV_PIX_FMT_FLAG_HWACCEL))
            break;

        if (*p == AV_PIX_FMT_QSV) {
            av_buffer_unref(&s->hw_frames_ctx);
            s->hw_frames_ctx = av_hwframe_ctx_alloc(g_device);
            if (!s->hw_frames_ctx)
                return AV_PIX_FMT_NONE;

            frames_ctx   = (AVHWFramesContext*)s->hw_frames_ctx->data;
            frames_hwctx = frames_ctx->hwctx;

            frames_ctx->width             = s->coded_width;
            frames_ctx->height            = s->coded_height;
            frames_ctx->format            = AV_PIX_FMT_QSV;
            frames_ctx->sw_format         = s->sw_pix_fmt;
            frames_ctx->initial_pool_size = 0;
            frames_hwctx->frame_type      = MFX_MEMTYPE_VIDEO_MEMORY_DECODER_TARGET;

            ret = av_hwframe_ctx_init(s->hw_frames_ctx);
            if (ret < 0) {
                av_log(NULL, AV_LOG_ERROR, "Error initializing a QSV frame pool\n");
                return AV_PIX_FMT_NONE;
            }
            break;
        }
    }

    return *p;
}

static int open_input_file(const char *filename)
{
    int ret;
    unsigned int i;
    AVStream *stream;
    FilteringContext *filt_ctx;

    ifmt_ctx = NULL;
    if ((ret = avformat_open_input(&ifmt_ctx, filename, NULL, NULL)) < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot open input file\n");
        return ret;
    }

    if ((ret = avformat_find_stream_info(ifmt_ctx, NULL)) < 0) {
        av_log(NULL, AV_LOG_ERROR, "Cannot find stream information\n");
        return ret;
    }

    if ((ret = init_filters()) < 0)
        return ret;

    for (i = 0; i < ifmt_ctx->nb_streams; i++) {
        stream = ifmt_ctx->streams[i];
        filt_ctx = &filter_ctx[i];
        /* Reencode video and remux subtitles etc. */
        if (stream->codecpar->codec_type == AVMEDIA_TYPE_VIDEO) {
            filt_ctx->dec_ctx = avcodec_alloc_context3(NULL);
            if (!filt_ctx)
                return AVERROR(ENOMEM);

            avcodec_parameters_to_context(filt_ctx->dec_ctx, stream->codecpar);
            filt_ctx->dec_ctx->framerate  = stream->avg_frame_rate;
            filt_ctx->dec_ctx->time_base  = av_inv_q(filt_ctx->dec_ctx->framerate);
            filt_ctx->dec_ctx->get_format = get_format;
            filt_ctx->dec_ctx->refcounted_frames = 1;
            /* Open decoder */
            ret = avcodec_open2(filt_ctx->dec_ctx,
                    find_decoder_by_codec_id(filt_ctx->dec_ctx->codec_id), NULL);
            if (ret < 0) {
                av_log(NULL, AV_LOG_ERROR, "Failed to open decoder for stream #%u\n", i);
                return ret;
            }
        }
    }

    av_dump_format(ifmt_ctx, 0, filename, 0);
    return 0;
}

static int open_output_file(const char *filename)
{
    AVStream *out_stream;
    const AVCodec *encoder;
    int ret;
    unsigned int i;
    FilteringContext *filt_ctx;

    ofmt_ctx = NULL;
    avformat_alloc_output_context2(&ofmt_ctx, NULL, NULL, filename);
    if (!ofmt_ctx) {
        av_log(NULL, AV_LOG_ERROR, "Could not create output context\n");
        return AVERROR_UNKNOWN;
    }

    for (i = 0; i < ifmt_ctx->nb_streams; i++) {
        filt_ctx = filter_ctx + i;
        out_stream = avformat_new_stream(ofmt_ctx, NULL);
        if (!out_stream) {
            av_log(NULL, AV_LOG_ERROR, "Failed allocating output stream\n");
            return AVERROR_UNKNOWN;
        }

        if (ifmt_ctx->streams[i]->codecpar->codec_type == AVMEDIA_TYPE_VIDEO) {
            filt_ctx->enc_ctx = avcodec_alloc_context3(NULL);
            if (!filt_ctx->enc_ctx)
                return AVERROR(ENOMEM);
            /* in this example, we choose transcoding to same codec */
            encoder = find_encoder_by_codec_id(filt_ctx->dec_ctx->codec_id);
            if (!encoder) {
                av_log(NULL, AV_LOG_FATAL, "Necessary encoder not found\n");
                return AVERROR_INVALIDDATA;
            }

            filt_ctx->enc_ctx->width  = filt_ctx->buffersink_ctx->inputs[0]->w;
            filt_ctx->enc_ctx->height = filt_ctx->buffersink_ctx->inputs[0]->h;
            filt_ctx->enc_ctx->sample_aspect_ratio = filt_ctx->buffersink_ctx->inputs[0]->sample_aspect_ratio;
            /* take first format from list of supported formats */
            filt_ctx->enc_ctx->pix_fmt = filt_ctx->buffersink_ctx->inputs[0]->format;
            /* video time_base can be set to whatever is handy and supported by encoder */
            filt_ctx->enc_ctx->time_base = filt_ctx->buffersink_ctx->inputs[0]->time_base;
            if (filt_ctx->buffersink_ctx->inputs[0]->hw_frames_ctx)
                filter_ctx->enc_ctx->hw_frames_ctx =
                        av_buffer_ref(filt_ctx->buffersink_ctx->inputs[0]->hw_frames_ctx);

            /* Third parameter can be used to pass settings to encoder */
            ret = avcodec_open2(filt_ctx->enc_ctx, encoder, NULL);
            if (ret < 0) {
                av_log(NULL, AV_LOG_ERROR, "Cannot open video encoder for stream #%u\n", i);
                return ret;
            }
            avcodec_parameters_from_context(out_stream->codecpar, filt_ctx->enc_ctx);
            out_stream->time_base = filt_ctx->enc_ctx->time_base;

            if (ofmt_ctx->oformat->flags & AVFMT_GLOBALHEADER)
                filt_ctx->enc_ctx->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
        } else if (ifmt_ctx->streams[i]->codecpar->codec_type == AVMEDIA_TYPE_UNKNOWN) {
            av_log(NULL, AV_LOG_FATAL, "Elementary stream #%d is of unknown type, cannot proceed\n", i);
            return AVERROR_INVALIDDATA;
        } else {
            /* if this stream must be remuxed */
            ret = avcodec_parameters_copy(ofmt_ctx->streams[i]->codecpar,
                    ifmt_ctx->streams[i]->codecpar);
            if (ret < 0) {
                av_log(NULL, AV_LOG_ERROR, "Copying stream context failed\n");
                return ret;
            }
            ofmt_ctx->streams[i]->codecpar->codec_tag =
                    av_codec_get_tag(ofmt_ctx->oformat->codec_tag,
                        ofmt_ctx->streams[i]->codecpar->codec_id);
        }
    }
    av_dump_format(ofmt_ctx, 0, filename, 1);

    if (!(ofmt_ctx->oformat->flags & AVFMT_NOFILE)) {
        ret = avio_open(&ofmt_ctx->pb, filename, AVIO_FLAG_WRITE);
        if (ret < 0) {
            av_log(NULL, AV_LOG_ERROR, "Could not open output file '%s'", filename);
            return ret;
        }
    }

    /* init muxer, write output file header */
    ret = avformat_write_header(ofmt_ctx, NULL);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Error occurred when opening output file\n");
        return ret;
    }

    return 0;
}

static int init_filter(FilteringContext* fctx, AVCodecContext *dec_ctx, const char *filter_spec)
{
    char args[512];
    int ret = 0;
    AVFilter *buffersrc = NULL;
    AVFilter *buffersink = NULL;
    AVFilterContext *buffersrc_ctx = NULL;
    AVFilterContext *buffersink_ctx = NULL;
    AVFilterInOut *outputs = avfilter_inout_alloc();
    AVFilterInOut *inputs  = avfilter_inout_alloc();
    AVFilterGraph *filter_graph = avfilter_graph_alloc();

    if (!outputs || !inputs || !filter_graph) {
        ret = AVERROR(ENOMEM);
        goto end;
    }

    if (dec_ctx->codec_type == AVMEDIA_TYPE_VIDEO) {
        buffersrc = avfilter_get_by_name("buffer");
        buffersink = avfilter_get_by_name("buffersink");
        if (!buffersrc || !buffersink) {
            av_log(NULL, AV_LOG_ERROR, "filtering source or sink element not found\n");
            ret = AVERROR_UNKNOWN;
            goto end;
        }

        snprintf(args, sizeof(args),
                "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=%d/%d"
                ":frame_rate=%d/%d",
                dec_ctx->width, dec_ctx->height, dec_ctx->pix_fmt,
                dec_ctx->time_base.num, dec_ctx->time_base.den,
                dec_ctx->sample_aspect_ratio.num,
                dec_ctx->sample_aspect_ratio.den,
                dec_ctx->framerate.num, dec_ctx->framerate.den);

        ret = avfilter_graph_create_filter(&buffersrc_ctx, buffersrc, "in",
                args, NULL, filter_graph);
        if (ret < 0) {
            av_log(NULL, AV_LOG_ERROR, "Cannot create buffer source\n");
            goto end;
        }

        if (dec_ctx->hw_frames_ctx) {
            AVBufferSrcParameters *par = av_buffersrc_parameters_alloc();
            par->hw_frames_ctx = dec_ctx->hw_frames_ctx;
            ret = av_buffersrc_parameters_set(buffersrc_ctx, par);
            av_freep(&par);
            if (ret < 0)
                goto end;
        }

        ret = avfilter_graph_create_filter(&buffersink_ctx, buffersink, "out",
                NULL, NULL, filter_graph);
        if (ret < 0) {
            av_log(NULL, AV_LOG_ERROR, "Cannot create buffer sink\n");
            goto end;
        }
    } else {
        ret = AVERROR_UNKNOWN;
        goto end;
    }

    /* Endpoints for the filter graph. */
    outputs->name       = av_strdup("in");
    outputs->filter_ctx = buffersrc_ctx;
    outputs->pad_idx    = 0;
    outputs->next       = NULL;

    inputs->name       = av_strdup("out");
    inputs->filter_ctx = buffersink_ctx;
    inputs->pad_idx    = 0;
    inputs->next       = NULL;

    if (!outputs->name || !inputs->name) {
        ret = AVERROR(ENOMEM);
        goto end;
    }

    if ((ret = avfilter_graph_parse_ptr(filter_graph, filter_spec,
                    &inputs, &outputs, NULL)) < 0)
        goto end;

    if ((ret = avfilter_graph_config(filter_graph, NULL)) < 0)
        goto end;

    /* Fill FilteringContext */
    fctx->buffersrc_ctx = buffersrc_ctx;
    fctx->buffersink_ctx = buffersink_ctx;
    fctx->filter_graph = filter_graph;
    fctx->initiallized = 1;

end:
    avfilter_inout_free(&inputs);
    avfilter_inout_free(&outputs);

    return ret;
}

static int encode_write_frame(AVFrame *filt_frame, unsigned int stream_index, int *got_frame) {
    int ret;
    int got_frame_local;
    AVPacket enc_pkt;

    if (!got_frame)
        got_frame = &got_frame_local;

    av_log(NULL, AV_LOG_INFO, "Encoding frame\n");
    /* encode filtered frame */
    enc_pkt.data = NULL;
    enc_pkt.size = 0;
    av_init_packet(&enc_pkt);
    ret = avcodec_encode_video2(filter_ctx[stream_index].enc_ctx, &enc_pkt,
                   filt_frame, got_frame);
    av_frame_free(&filt_frame);
    if (ret < 0)
        return ret;
    if (!(*got_frame))
        return 0;

    /* prepare packet for muxing */
    enc_pkt.stream_index = stream_index;
    av_packet_rescale_ts(&enc_pkt,
                         filter_ctx[stream_index].enc_ctx->time_base,
                         ofmt_ctx->streams[stream_index]->time_base);

    av_log(NULL, AV_LOG_DEBUG, "Muxing frame\n");
    /* mux encoded frame */
    ret = av_interleaved_write_frame(ofmt_ctx, &enc_pkt);
    return ret;
}

static int filter_encode_write_frame(AVFrame *frame, unsigned int stream_index)
{
    int ret;
    AVFrame *filt_frame;

    av_log(NULL, AV_LOG_INFO, "Pushing decoded frame to filters\n");
    /* push the decoded frame into the filtergraph */
    ret = av_buffersrc_add_frame_flags(filter_ctx[stream_index].buffersrc_ctx,
            frame, 0);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Error while feeding the filtergraph\n");
        return ret;
    }

    if (!ofmt_ctx)
        return 0;

    /* pull filtered frames from the filtergraph */
    while (1) {
        filt_frame = av_frame_alloc();
        if (!filt_frame) {
            ret = AVERROR(ENOMEM);
            break;
        }
        av_log(NULL, AV_LOG_INFO, "Pulling filtered frame from filters\n");
        ret = av_buffersink_get_frame(filter_ctx[stream_index].buffersink_ctx,
                filt_frame);
        if (ret < 0) {
            /* if no more frames for output - returns AVERROR(EAGAIN)
             * if flushed and no more frames for output - returns AVERROR_EOF
             * rewrite retcode to 0 to show it as normal procedure completion
             */
            if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF)
                ret = 0;
            av_frame_free(&filt_frame);
            break;
        }

        filt_frame->pict_type = AV_PICTURE_TYPE_NONE;
        ret = encode_write_frame(filt_frame, stream_index, NULL);
        if (ret < 0)
            break;
    }

    return ret;
}

static int flush_encoder(unsigned int stream_index)
{
    int ret;
    int got_frame;

    if (!(filter_ctx[stream_index].enc_ctx->codec->capabilities &
                AV_CODEC_CAP_DELAY))
        return 0;

    while (1) {
        av_log(NULL, AV_LOG_INFO, "Flushing stream #%u encoder\n", stream_index);
        ret = encode_write_frame(NULL, stream_index, &got_frame);
        if (ret < 0)
            break;
        if (!got_frame)
            return 0;
    }
    return ret;
}

int main(int argc, char **argv)
{
    int ret;
    AVPacket packet = { .data = NULL, .size = 0 };
    AVFrame *frame = NULL;
    enum AVMediaType type;
    unsigned int stream_index;
    unsigned int i;
    int got_frame;
    char *vpp_config = NULL;

    if (argc < 3) {
        av_log(NULL, AV_LOG_ERROR, "Usage: %s <input file> <output file> [vpp options]\n", argv[0]);
        return 1;
    }
    if (argc == 4)
        vpp_config = av_asprintf("vpp_qsv=%s", argv[3]);
    else
        vpp_config = av_strdup("null");

    av_register_all();
    avfilter_register_all();
    avcodec_register_all();

    ret = av_hwdevice_ctx_create(&g_device, AV_HWDEVICE_TYPE_QSV,
                "/dev/dri/render128", NULL, 0);
    if (ret < 0) {
        av_log(NULL, AV_LOG_ERROR, "Failed to create QSV device.\n");
        return ret;
    }

    if ((ret = open_input_file(argv[1])) < 0)
        goto end;

    /* read all packets */
    while (1) {
        if (!ofmt_ctx) {
            for (i = 0; i < ifmt_ctx->nb_streams; i++)
                if (!filter_ctx[i].initiallized)
                    break;
            if (i == ifmt_ctx->nb_streams)
                if ((ret = open_output_file(argv[2])) < 0)
                    goto end;
        }

        if ((ret = av_read_frame(ifmt_ctx, &packet)) < 0)
            break;
        stream_index = packet.stream_index;
        type = ifmt_ctx->streams[stream_index]->codecpar->codec_type;
        av_log(NULL, AV_LOG_DEBUG, "Demuxer gave frame of stream_index %u\n",
                stream_index);

        if (type == AVMEDIA_TYPE_VIDEO) {
            av_log(NULL, AV_LOG_DEBUG, "Going to reencode&filter the frame\n");
            frame = av_frame_alloc();
            if (!frame) {
                ret = AVERROR(ENOMEM);
                break;
            }
            av_packet_rescale_ts(&packet,
                                 ifmt_ctx->streams[stream_index]->time_base,
                                 filter_ctx[stream_index].dec_ctx->time_base);
            ret = avcodec_decode_video2(filter_ctx[stream_index].dec_ctx, frame,
                    &got_frame, &packet);
            if (ret < 0) {
                av_frame_free(&frame);
                av_log(NULL, AV_LOG_ERROR, "Decoding failed\n");
                break;
            }

            if (got_frame) {
                if (!filter_ctx[stream_index].initiallized) {
                    ret = init_filter(&filter_ctx[stream_index],
                            filter_ctx[stream_index].dec_ctx, vpp_config);
                    if (ret < 0)
                        return ret;
                }
                frame->pts = av_frame_get_best_effort_timestamp(frame);
                ret = filter_encode_write_frame(frame, stream_index);
                av_frame_free(&frame);
                if (ret < 0)
                    goto end;
            } else {
                av_frame_free(&frame);
            }
        } else {
            /* remux this frame without reencoding */
            if (ofmt_ctx) {
                av_packet_rescale_ts(&packet,
                                     ifmt_ctx->streams[stream_index]->time_base,
                                     ofmt_ctx->streams[stream_index]->time_base);

                ret = av_interleaved_write_frame(ofmt_ctx, &packet);
                if (ret < 0)
                    goto end;
            }
        }
        av_packet_unref(&packet);
    }

    /* flush filters and encoders */
    for (i = 0; i < ifmt_ctx->nb_streams; i++) {
        /* flush filter */
        if (!filter_ctx[i].filter_graph)
            continue;
        ret = filter_encode_write_frame(NULL, i);
        if (ret < 0) {
            av_log(NULL, AV_LOG_ERROR, "Flushing filter failed\n");
            goto end;
        }

        /* flush encoder */
        ret = flush_encoder(i);
        if (ret < 0) {
            av_log(NULL, AV_LOG_ERROR, "Flushing encoder failed\n");
            goto end;
        }
    }

    av_write_trailer(ofmt_ctx);
end:
    av_freep(&vpp_config);
    av_packet_unref(&packet);
    av_frame_free(&frame);
    for (i = 0; i < ifmt_ctx->nb_streams; i++) {
        avcodec_close(filter_ctx[i].dec_ctx);
        avcodec_free_context(&filter_ctx[i].dec_ctx);
        if (ofmt_ctx && ofmt_ctx->nb_streams > i && ofmt_ctx->streams[i] && filter_ctx[i].enc_ctx) {
            avcodec_close(filter_ctx[i].enc_ctx);
            avcodec_free_context(&filter_ctx[i].enc_ctx);
        }
        if (filter_ctx && filter_ctx[i].filter_graph)
            avfilter_graph_free(&filter_ctx[i].filter_graph);
    }
    av_free(filter_ctx);
    avformat_close_input(&ifmt_ctx);
    if (ofmt_ctx && !(ofmt_ctx->oformat->flags & AVFMT_NOFILE))
        avio_closep(&ofmt_ctx->pb);
    avformat_free_context(ofmt_ctx);
    av_buffer_unref(&g_device);

    if (ret < 0)
        av_log(NULL, AV_LOG_ERROR, "Error occurred: %s\n", av_err2str(ret));

    return ret ? 1 : 0;
}
