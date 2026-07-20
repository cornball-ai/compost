# render.R
# Lower a rotio (OpenTimelineIO) Timeline to a rendered video via ffmpeg.
# This is the OTIO -> ffmpeg renderer: it reads the timeline structure and the
# cornball metadata conventions, then emits a single ffmpeg invocation.
#
# Scope: one video track of sequential clips (concatenated, dissolving over
# Transitions), an optional separate audio track, an optional caption track
# that references a subtitle file, and a framing transform (scale + pad)
# carried in `metadata$cornball$framing`. Still-image and ImageSequenceReference
# clips are pre-rendered into video via still_clip()/frames_clip(), honoring
# the `cornball.*` motion-effect namespace (inst/schema/cornball-effects.md).
# Roled tracks (narrator, ...) compose onto a canvas per the cornball layout
# contract (inst/schema/cornball-layout.md) via compose_layout(). Arbitrary
# stacks and overlapping layers beyond that are not lowered.

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

#' A track's cornball role ("" when untagged)
#' @keywords internal
.track_role <- function(track) {
    role <- tryCatch(rotio::metadata(track)$cornball$role,
                     error = function(e) NULL)
    if (is.null(role)) {
        return("")
    }
    as.character(role)
}

#' The timeline's cornball layout metadata, or NULL
#' @keywords internal
.timeline_layout <- function(timeline) {
    tryCatch(rotio::metadata(timeline)$cornball$layout,
             error = function(e) NULL)
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
#' @return list(files, windows, fades, clips). A Clip over an
#'   ImageSequenceReference contributes an NA file, filled in by
#'   \code{.prerender_sources()}.
#' @keywords internal
.video_sequence <- function(track, media_dir = NULL) {
    files <- character(0)
    windows <- list()
    fades <- numeric(0)
    clips <- list()
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
        if (inherits(mr, "ImageSequenceReference")) {
            # No single url to resolve; .prerender_sources() encodes the
            # sequence from the reference fields.
            u <- NA_character_
        } else {
            u <- tryCatch(rotio::target_url(mr),
                          error = function(e) NA_character_)
            if (is.na(u) || !nzchar(u)) {
                next
            }
            u <- .resolve_media(u, media_dir)
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
        files <- c(files, u)
        if (length(files) > 1) {
            fades <- c(fades, fade)
        }
        # c(list(win)) rather than [[<-: assigning NULL would drop the slot.
        windows <- c(windows, list(win))
        clips <- c(clips, list(kd))
    }
    list(files = files, windows = windows, fades = fades, clips = clips)
}

#' Motion spec from a clip's OTIO effects
#'
#' Reads the open \code{cornball.*} effect namespace (see
#' \code{inst/schema/cornball-effects.md}). Effects outside the namespace
#' belong to other ecosystems and are ignored silently. Inside it, anything
#' this renderer cannot honor -- an unrecognized effect_name, enabled = FALSE,
#' or a metadata schema newer than supported -- degrades to a static clip with
#' one warning. The first usable motion effect wins; extras warn.
#'
#' @param effects List of rotio Effect objects (a clip's effects).
#' @return A Ken Burns metadata list for \code{\link{still_clip}}, or NULL.
#' @keywords internal
.motion_from_effects <- function(effects) {
    motion <- NULL
    for (e in effects) {
        en <- tryCatch(rotio::effect_name(e), error = function(err) "")
        if (!startsWith(en, "cornball.")) {
            next
        }
        if (!identical(en, "cornball.kenburns")) {
            warning("render_timeline(): skipping unrecognized effect '", en,
                    "' (clip renders static)", call. = FALSE)
            next
        }
        if (!isTRUE(tryCatch(rotio::enabled(e), error = function(err) TRUE))) {
            warning("render_timeline(): skipping disabled effect '", en, "'",
                    call. = FALSE)
            next
        }
        md <- tryCatch(rotio::metadata(e), error = function(err) NULL)
        sch <- if (is.null(md$schema)) {
            1L
        } else {
            suppressWarnings(as.integer(unlist(md$schema)[1]))
        }
        if (is.na(sch) || sch > 1L) {
            warning("render_timeline(): effect '", en, "' has schema ",
                    md$schema, "; this renderer supports 1 (clip renders ",
                    "static)", call. = FALSE)
            next
        }
        if (!is.null(motion)) {
            warning("render_timeline(): multiple motion effects on one clip; ",
                    "using the first", call. = FALSE)
            next
        }
        motion <- md
    }
    motion
}

#' Pre-render still-image and image-sequence clips into video
#'
#' A still image has no intrinsic duration or motion, and an
#' ImageSequenceReference has no single file: both are encoded into temporary
#' video clips here, so everything downstream (concat, crossfades, framing,
#' captions, audio) handles only video. Stills go through
#' \code{\link{still_clip}} at the source_range's rate, covering the media
#' window up to the range's end (so a preceding dissolve keeps its head
#' handle), rastered to the source aspect fitted inside the framing box, and
#' animated by the clip's \code{cornball.*} motion effects. Sequences go
#' through \code{\link{frames_clip}} at the reference's own rate; motion
#' effects are not applied to them (a drawn sequence animates itself). Video
#' files pass through untouched.
#'
#' @param seq_v A \code{.video_sequence()} result.
#' @param framing The timeline framing list, for still rastering.
#' @param media_dir Base directory for relative urls, or NULL.
#' @return list(files, temps): files with pre-rendered replacements, and the
#'   temp paths for the caller to clean up.
#' @keywords internal
.prerender_sources <- function(seq_v, framing, media_dir = NULL) {
    files <- seq_v$files
    temps <- character(0)
    for (i in seq_along(files)) {
        clip <- seq_v$clips[[i]]
        mr <- rotio::media_reference(clip)
        sr <- tryCatch(rotio::source_range(clip), error = function(e) NULL)

        if (inherits(mr, "ImageSequenceReference")) {
            zp <- as.integer(mr$frame_zero_padding)
            pattern <- paste0(mr$name_prefix,
                if (!is.na(zp) && zp > 0) {
                    sprintf("%%0%dd", zp)
                } else {
                    "%d"
                },
                              mr$name_suffix)
            fps <- as.numeric(mr$rate)
            if (!isTRUE(fps > 0)) {
                if (is.null(sr)) {
                    fps <- 30
                } else {
                    fps <- rotio::rate(sr$duration)
                }
            }
            tmp <- tempfile(fileext = ".mp4")
            frames_clip(.resolve_media(mr$target_url_base, media_dir), tmp,
                        fps = fps, pattern = pattern,
                        start = as.integer(mr$start_frame))
            files[i] <- tmp
            temps <- c(temps, tmp)
            next
        }

        if (!.is_image(files[i])) {
            next
        }
        if (is.null(sr)) {
            stop("render_timeline(): still-image clip '", rotio::name(clip),
                 "' needs a source_range (a still has no intrinsic duration)",
                 call. = FALSE)
        }
        # Media length = the range's end, so the head handle a preceding
        # Transition widened into exists in the pre-rendered file.
        dur <- rotio::to_seconds(sr$start_time) +
        rotio::to_seconds(sr$duration)
        fps <- rotio::rate(sr$duration)
        if (!isTRUE(fps > 0)) {
            fps <- 30
        }
        img <- normalizePath(files[i], mustWork = TRUE)
        size <- .still_size(as.integer(probe(img, "width")),
                            as.integer(probe(img, "height")), framing)
        motion <- .motion_from_effects(
                                       tryCatch(rotio::effects(clip), error = function(e) list()))
        tmp <- tempfile(fileext = ".mp4")
        still_clip(img, tmp, duration = dur, fps = fps, size = size,
                   motion = motion)
        files[i] <- tmp
        temps <- c(temps, tmp)
    }
    list(files = files, temps = temps)
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

#' Assemble one video track into a single renderable file
#'
#' The track walk + still/sequence pre-render + passthrough/concat/crossfade
#' pipeline, shared by the content track and layout slot tracks.
#'
#' @param track A rotio video Track.
#' @param media_dir Base directory for relative urls, or NULL.
#' @param framing Framing for still rastering (NULL for slot tracks).
#' @return list(file, temps), or NULL when the track has no clips.
#' @keywords internal
.assemble_track <- function(track, media_dir, framing) {
    seq_v <- .video_sequence(track, media_dir)
    if (length(seq_v$files) == 0) {
        return(NULL)
    }
    pre <- .prerender_sources(seq_v, framing, media_dir)
    temps <- pre$temps
    vclips <- normalizePath(pre$files, mustWork = TRUE)
    trivial <- all(seq_v$fades == 0) &&
    all(vapply(seq_v$windows, is.null, logical(1)))
    if (length(vclips) == 1 && trivial) {
        file <- vclips[[1]]
    } else if (trivial) {
        # Whole files, butt joins: stream-copy concat beats filtering.
        file <- tempfile(fileext = ".mp4")
        temps <- c(temps, file)
        concat(vclips, file, overwrite = TRUE)
    } else {
        file <- tempfile(fileext = ".mp4")
        temps <- c(temps, file)
        crossfade_concat(vclips, file,
                         fade = if (length(seq_v$fades)) seq_v$fades else 0,
                         windows = seq_v$windows, overwrite = TRUE)
    }
    list(file = file, temps = temps)
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
#' Still-image clips (a media reference targeting a \code{.png}/\code{.jpg}/
#' ...) are pre-rendered into video via \code{\link{still_clip}} at the
#' source_range's rate, honoring \code{cornball.*} motion effects on the clip
#' (Ken Burns pan/zoom; the schema lives in
#' \code{system.file("schema", "cornball-effects.md", package = "compost")}).
#' A still-image clip must carry a source_range. \code{ImageSequenceReference}
#' clips are pre-rendered via \code{\link{frames_clip}} at the reference's
#' rate; motion effects are not applied to sequences. Effects the renderer
#' cannot honor degrade to a static clip with a warning; effects outside the
#' \code{cornball.} namespace are ignored silently.
#'
#' When the timeline carries \code{metadata$cornball$layout} (schema in
#' \code{system.file("schema", "cornball-layout.md", package = "compost")}),
#' tracks whose \code{cornball$role} matches a layout slot are each assembled
#' like the content track and composed onto the canvas via
#' \code{\link{compose_layout}} -- muted, painted in slot order, with the
#' content track bound to the \code{visual} slot as the timing reference.
#' The framing transform is suppressed when composing (the layout defines
#' the canvas). A layout the renderer cannot honor -- schema too new, a
#' slot's track missing, or a roled track without a slot -- degrades to the
#' plain single-stream render with one warning.
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
#'   Note: concatenation of multiple video clips, still/sequence
#'   pre-renders, and layout composition still run, since they produce
#'   intermediates the final command depends on.
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

    # Partition the Video-kind tracks: captions, layout slot tracks (role
    # matching a layout slot other than "visual"), and content.
    vk_tracks <- rotio::video_tracks(timeline)
    is_cap <- vapply(vk_tracks, .is_caption_track, logical(1))
    ctracks <- vk_tracks[is_cap]
    vtracks <- vk_tracks[!is_cap]
    atracks <- rotio::audio_tracks(timeline)

    layout <- .timeline_layout(timeline)
    if (!is.null(layout) && is.null(.layout_slots(layout))) {
        layout <- NULL # schema too new: warned inside, degrade to slides-only
    }
    slot_names <- if (is.null(layout)) {
        character(0)
    } else {
        setdiff(names(layout$slots), "visual")
    }
    roles <- vapply(vtracks, .track_role, character(1))
    in_slot <- roles %in% slot_names
    stray <- !(roles %in% c("", "visual")) & !in_slot
    if (any(stray)) {
        warning("render_timeline(): track role(s) ",
                paste(unique(roles[stray]), collapse = ", "),
                " have no layout slot; ignoring those tracks", call. = FALSE)
    }
    slot_tracks <- vtracks[in_slot]
    names(slot_tracks) <- roles[in_slot]
    vtracks <- vtracks[!in_slot & !stray]

    if (length(vtracks) == 0) {
        stop("render_timeline(): no video track found", call. = FALSE)
    }

    # Framing is resolved before pre-rendering: still clips are rastered to
    # their source aspect fitted inside the framing box.
    framing <- .timeline_framing(vtracks[[1]], timeline)

    base <- .assemble_track(vtracks[[1]], media_dir, framing)
    if (is.null(base)) {
        stop("render_timeline(): the video track has no clips", call. = FALSE)
    }
    if (length(base$temps) > 0) {
        on.exit(unlink(base$temps), add = TRUE)
    }
    base_video <- base$file

    # Layout composition: paint the content and each slot track onto the
    # canvas. The composed base is video-only, so the narration-bed mapping
    # below stays authoritative. Framing is suppressed afterwards (the
    # layout defines the canvas); it was already consumed above for still
    # rastering, so slides keep their full-quality raster and the compose
    # downscales into the slot.
    if (!is.null(layout) && length(slot_names) > 0) {
        lost <- setdiff(slot_names, names(slot_tracks))
        ok <- length(lost) == 0
        if (!ok) {
            warning("render_timeline(): layout slot(s) ",
                    paste(lost, collapse = ", "),
                    " have no matching track; rendering without composition",
                    call. = FALSE)
        }
        srcs <- list(visual = base_video)
        if (ok) {
            for (nm in slot_names) {
                st <- .assemble_track(slot_tracks[[nm]], media_dir, NULL)
                if (is.null(st)) {
                    warning("render_timeline(): slot track '", nm,
                            "' has no clips; rendering without composition",
                            call. = FALSE)
                    ok <- FALSE
                    break
                }
                if (length(st$temps) > 0) {
                    on.exit(unlink(st$temps), add = TRUE)
                }
                srcs[[nm]] <- st$file
            }
        }
        if (ok) {
            composed <- tempfile(fileext = ".mp4")
            on.exit(unlink(composed), add = TRUE)
            compose_layout(srcs, composed, layout, reference = "visual")
            base_video <- composed
            framing <- NULL
        }
    }

    # Framing (scale + pad) and an optional caption burn share one filter chain.
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
