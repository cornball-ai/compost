# render.R
# Lower a rotio (OpenTimelineIO) Timeline to a rendered video via ffmpeg.
# This is the OTIO -> ffmpeg renderer: it reads the timeline structure and the
# cornball metadata conventions, then emits a single ffmpeg invocation.
#
# Scope (v1): the lc40 / LilCasey shape. One video track of sequential clips
# (concatenated), an optional separate audio track, an optional caption track
# that references a subtitle file, and a framing transform (scale + pad) carried
# in `metadata$cornball$framing`. Stacks, transitions, per-clip effects, and
# overlapping layers are not lowered yet.

#' Is this track a caption track?
#'
#' A track is treated as captions if its cornball metadata sets
#' \code{role = "caption"} or its name contains "caption".
#'
#' @param track A rotio Track.
#' @return TRUE if the track holds captions.
#' @keywords internal
.is_caption_track <- function(track) {
    role <- tryCatch(rotio::metadata(track)$cornball$role,
                     error = function(e) NULL)
    if (!is.null(role) && identical(role, "caption")) {
        return(TRUE)
    }
    grepl("caption", rotio::name(track), ignore.case = TRUE)
}

#' Resolve a media reference url against an optional base directory
#'
#' @param url Target url from an OTIO media reference.
#' @param media_dir Base directory for relative urls, or NULL.
#' @return The resolved path.
#' @keywords internal
.resolve_media <- function(url, media_dir) {
    if (is.null(media_dir) || startsWith(url, "/")) {
        return(url)
    }
    file.path(media_dir, url)
}

#' Ordered media urls of the Clips on a track
#'
#' Gaps and non-clip children are skipped.
#'
#' @param track A rotio Track.
#' @param media_dir Base directory for relative urls, or NULL.
#' @return Character vector of resolved media paths, in track order.
#' @keywords internal
.clip_urls <- function(track, media_dir = NULL) {
    ch <- rotio::children(track)
    urls <- character(0)
    for (item in ch) {
        if (!inherits(item, "Clip")) {
            next
        }
        mr <- tryCatch(rotio::media_reference(item), error = function(e) NULL)
        if (is.null(mr)) {
            next
        }
        u <- tryCatch(rotio::target_url(mr), error = function(e) NA_character_)
        if (!is.na(u) && nzchar(u)) {
            urls <- c(urls, .resolve_media(u, media_dir))
        }
    }
    urls
}

#' Walk a video track into renderable clip feeds and join fades
#'
#' Returns the clips' resolved files, per-clip source windows (the media to
#' feed in: the source range, widened at the front by a preceding Transition's
#' in_offset so the dissolve has its handle), and per-join fade durations
#' (0 = butt join). A clip without a source_range feeds the whole file.
#'
#' @param track A rotio video Track.
#' @param media_dir Base directory for relative urls, or NULL.
#' @return list(files, windows, fades).
#' @keywords internal
.video_sequence <- function(track, media_dir = NULL) {
    files <- character(0)
    windows <- list()
    fades <- numeric(0)
    pending <- 0
    for (kd in rotio::children(track)) {
        if (inherits(kd, "Transition")) {
            if (rotio::to_seconds(kd$out_offset) > 0) {
                stop("render_timeline(): Transition with out_offset > 0 is ",
                     "not lowered yet (the outgoing clip would need a tail ",
                     "handle)", call. = FALSE)
            }
            pending <- rotio::to_seconds(kd$in_offset)
            next
        }
        if (!inherits(kd, "Clip")) {
            next
        }
        mr <- tryCatch(rotio::media_reference(kd), error = function(e) NULL)
        if (is.null(mr)) {
            next
        }
        u <- tryCatch(rotio::target_url(mr), error = function(e) NA_character_)
        if (is.na(u) || !nzchar(u)) {
            next
        }
        sr <- tryCatch(rotio::source_range(kd), error = function(e) NULL)
        if (length(files) > 0) {
            fade <- pending
        } else {
            fade <- 0
        }
        pending <- 0
        win <- NULL
        if (!is.null(sr)) {
            s <- rotio::to_seconds(sr$start_time)
            e <- s + rotio::to_seconds(sr$duration)
            # Widen the front by the fade so the dissolve consumes the head
            # handle (the conditioning replay) instead of played content.
            ws <- max(0, s - fade)
            # Legacy timelines wrote nominal [0, dur] ranges over whole-file
            # media; only a range that starts past 0 (a real head trim) or a
            # fade needing its handle constitutes an actual window.
            if (ws > 0 || fade > 0) {
                win <- c(ws, e)
            }
        }
        files <- c(files, .resolve_media(u, media_dir))
        if (length(files) > 1) {
            fades <- c(fades, fade)
        }
        # c(list(win)) rather than [[<-: assigning NULL would drop the slot.
        windows <- c(windows, list(win))
    }
    list(files = files, windows = windows, fades = fades)
}

#' Framing transform for the timeline
#'
#' Looks for \code{metadata$cornball$framing} on the first video clip's media
#' reference, then the clip, then the timeline. A framing is a list with optional
#' \code{scale} (a box edge, or c(w, h)), \code{pad} (c(w, h)), and \code{pos}
#' (c(x, y), defaulting to c(0, 0)). Any element may be a string for ffmpeg
#' expressions (e.g. pos = c("(ow-iw)/2", "(oh-ih)/2")).
#'
#' @param vtrack The primary video Track.
#' @param timeline The Timeline.
#' @return A framing list, or NULL.
#' @keywords internal
.timeline_framing <- function(vtrack, timeline) {
    clips <- Filter(function(x) inherits(x, "Clip"), rotio::children(vtrack))
    if (length(clips) > 0) {
        f <- tryCatch(rotio::metadata(rotio::media_reference(clips[[1]]))$cornball$framing,
                      error = function(e) NULL)
        if (!is.null(f)) {
            return(f)
        }
        f <- tryCatch(rotio::metadata(clips[[1]])$cornball$framing,
                      error = function(e) NULL)
        if (!is.null(f)) {
            return(f)
        }
    }
    tryCatch(rotio::metadata(timeline)$cornball$framing, error = function(e) NULL)
}

#' Build the scale/pad ffmpeg filter chain from a framing list
#'
#' @param framing A framing list, or NULL.
#' @return Character vector of filter expressions (possibly empty).
#' @keywords internal
.framing_vf <- function(framing) {
    vf <- character(0)
    if (is.null(framing)) {
        return(vf)
    }
    if (!is.null(framing$scale)) {
        s <- framing$scale
        if (length(s) == 1) {
            s <- c(s, s)
        }
        vf <- c(vf, sprintf("scale=%s:%s:force_original_aspect_ratio=decrease",
                            s[1], s[2]))
    }
    if (!is.null(framing$pad)) {
        pad <- framing$pad
        if (is.null(framing$pos)) {
            pos <- c(0, 0)
        } else {
            pos <- framing$pos
        }
        vf <- c(vf, sprintf("pad=%s:%s:%s:%s:black",
                            pad[1], pad[2], pos[1], pos[2]))
    }
    vf
}

#' Build the subtitles burn filter for a subtitle file
#'
#' Escapes the path the way ffmpeg's subtitles filter requires.
#'
#' @param sub_file Path to an .ass/.srt/.vtt file.
#' @return A single filter expression.
#' @keywords internal
.subtitles_filter <- function(sub_file) {
    p <- gsub("\\\\", "/", sub_file)
    p <- gsub(":", "\\\\:", p)
    sprintf("subtitles='%s'", p)
}

#' First subtitle file referenced by any caption track
#'
#' @param ctracks List of caption Tracks.
#' @param media_dir Base directory for relative urls, or NULL.
#' @return A subtitle path, or NULL.
#' @keywords internal
.caption_file <- function(ctracks, media_dir = NULL) {
    for (t in ctracks) {
        # A caption track may point at its burn artifact directly via
        # metadata$cornball$ass (its clips hold text, not an .ass reference).
        ass <- tryCatch(rotio::metadata(t)$cornball$ass,
                        error = function(e) NULL)
        if (!is.null(ass) && nzchar(ass)) {
            return(.resolve_media(ass, media_dir))
        }
        urls <- .clip_urls(t, media_dir)
        subs <- urls[grepl("\\.(ass|srt|vtt)$", urls, ignore.case = TRUE)]
        if (length(subs) > 0) {
            return(subs[[1]])
        }
    }
    NULL
}

#' Render an OTIO Timeline to a Video File
#'
#' Lowers a rotio (OpenTimelineIO) Timeline to a single ffmpeg invocation and
#' renders it. The video track's clips are concatenated in order; an optional
#' separate audio track is mapped over them; a framing transform (scale + pad,
#' from \code{metadata$cornball$framing}) and an optional caption burn (from a
#' caption track referencing an .ass/.srt file) are applied.
#'
#' The video track is lowered honoring per-clip \code{source_range} trims and
#' \code{Transition}s: a transition between two clips becomes a dissolve over
#' exactly its duration, fed by the incoming clip's head handle (the media
#' before its source range -- for chained generation, the conditioning-head
#' replay). Only transitions with \code{out_offset == 0} are lowered (the
#' cornductor bundle shape: the outgoing clip has no tail handle). Gaps and
#' overlapping layers are not lowered yet.
#'
#' Caption tracks are identified by \code{metadata$cornball$role == "caption"} or
#' a name containing "caption". When the primary video track has a single clip
#' that already carries its own audio (the talking-head case), no separate audio
#' track is needed.
#'
#' @param timeline A rotio Timeline, or a path to a \code{.otio} file.
#' @param output Path for the output video file.
#' @param media_dir Base directory for resolving relative media urls. Defaults to
#'   the timeline file's directory when \code{timeline} is a path, otherwise NULL
#'   (urls used as-is).
#' @param overwrite If TRUE (default), overwrite the output file.
#' @param dry_run If TRUE, return the ffmpeg command string without executing.
#'   Note: concatenation of multiple video clips still runs, since it produces an
#'   intermediate the final command depends on.
#'
#' @return Invisibly returns the output path. If dry_run, returns the command
#'   string for the final render pass.
#'
#' @examples
#' \dontrun{
#' render_timeline("AAA/20260131/t41_n22_intro/timeline.otio",
#'                 "AAA/20260131/t41_n22_intro/video.mp4")
#' }
#'
#' @export
render_timeline <- function(timeline, output, media_dir = NULL,
                            overwrite = TRUE, dry_run = FALSE) {
    if (is.character(timeline)) {
        tl_path <- normalizePath(timeline, mustWork = TRUE)
        if (is.null(media_dir)) {
            media_dir <- dirname(tl_path)
        }
        timeline <- rotio::from_json_file(tl_path)
    }

    output <- normalizePath(output, mustWork = FALSE)

    # Partition the Video-kind tracks into captions and real video.
    vk_tracks <- rotio::video_tracks(timeline)
    is_cap <- vapply(vk_tracks, .is_caption_track, logical(1))
    vtracks <- vk_tracks[!is_cap]
    ctracks <- vk_tracks[is_cap]
    atracks <- rotio::audio_tracks(timeline)

    if (length(vtracks) == 0) {
        stop("render_timeline(): no video track found", call. = FALSE)
    }

    # Video items from the first video track: clips with their source windows
    # and the Transitions joining them.
    seq_v <- .video_sequence(vtracks[[1]], media_dir)
    if (length(seq_v$files) == 0) {
        stop("render_timeline(): the video track has no clips", call. = FALSE)
    }
    vclips <- normalizePath(seq_v$files, mustWork = TRUE)

    trivial <- all(seq_v$fades == 0) &&
    all(vapply(seq_v$windows, is.null, logical(1)))
    if (length(vclips) == 1 && trivial) {
        base_video <- vclips[[1]]
    } else if (trivial) {
        # Whole files, butt joins: stream-copy concat is cheaper than filtering.
        base_video <- tempfile(fileext = ".mp4")
        on.exit(unlink(base_video), add = TRUE)
        concat(vclips, base_video, overwrite = TRUE)
    } else {
        base_video <- tempfile(fileext = ".mp4")
        on.exit(unlink(base_video), add = TRUE)
        crossfade_concat(vclips, base_video,
                         fade = if (length(seq_v$fades)) seq_v$fades else 0,
                         windows = seq_v$windows, overwrite = TRUE)
    }

    # Framing (scale + pad) and an optional caption burn share one filter chain.
    framing <- .timeline_framing(vtracks[[1]], timeline)
    vf <- .framing_vf(framing)

    sub_file <- .caption_file(ctracks, media_dir)
    if (!is.null(sub_file)) {
        sub_file <- normalizePath(sub_file, mustWork = TRUE)
        vf <- c(vf, .subtitles_filter(sub_file))
    }

    # Audio: a separate audio track if present, otherwise from the video itself.
    audio_file <- NULL
    if (length(atracks) > 0) {
        aclips <- .clip_urls(atracks[[1]], media_dir)
        if (length(aclips) > 0) {
            audio_file <- normalizePath(aclips[[1]], mustWork = TRUE)
        }
    }

    # The narration is the ground truth: when an audio bed is mapped, the
    # video is padded (last frame held) or cut to EXACTLY the audio's
    # duration, so the two streams always match. Spoken words are never
    # clipped to a too-short video; surplus silent video never trails the
    # voice.
    n_frames <- NULL
    if (!is.null(audio_file)) {
        adur <- as.numeric(probe(audio_file, "duration"))
        vfps <- .video_fps(base_video)
        n_frames <- as.integer(round(adur * vfps))
        vf <- c(vf, "tpad=stop_mode=clone:stop=-1")
    }

    args <- c(
        if (overwrite) "-y",
              "-i", base_video,
        if (!is.null(audio_file)) c("-i", audio_file),
        if (length(vf) > 0) c("-vf", paste(vf, collapse = ",")),
              "-map", "0:v:0",
        if (!is.null(audio_file)) c("-map", "1:a:0") else c("-map", "0:a:0?"),
        if (!is.null(n_frames)) c("-frames:v", n_frames),
        if (length(vf) > 0) {
            c("-c:v", "libx264", "-preset", "fast")
        } else {
            c("-c:v", "copy")
        },
              "-c:a", "aac",
              "-movflags", "+faststart",
              output
    )

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}

