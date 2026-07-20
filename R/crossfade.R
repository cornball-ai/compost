# crossfade.R
# Stitch a sequence of clips with video crossfades over their overlaps, and
# (for chained generation) overlay a single continuous audio track in place of
# the per-clip audio. Built for cornductor's chained chunks: each chunk's
# conditioning head replays the previous chunk's tail, so a dissolve over that
# overlap hides the join, while the head's silent audio is sidestepped by
# overlaying the original narration.

#' Crossfade-concatenate clips, optionally overlaying one audio track
#'
#' Joins \code{videos} in order, dissolving each transition over \code{fade}
#' seconds (the duplicated conditioning overlap, so the blend is between
#' near-identical frames and is invisible). When \code{audio} is given it
#' becomes the sole audio track -- the right move for chained chunks, whose
#' per-chunk audio has a silent conditioning head; the continuous narration has
#' no such gaps. Without \code{audio}, the first clip's audio is mapped through.
#'
#' Each join consumes its \code{fade} seconds of overlap, so the result runs
#' \code{sum(durations) - sum(fades)} long -- slightly shorter than a hard
#' concat. One ffmpeg pass via xfade.
#'
#' @param videos Character vector of video paths, in order. Must share
#'   resolution / fps / pixel format (xfade requires it).
#' @param output Output video path.
#' @param fade Crossfade duration in seconds: a scalar applied to every join,
#'   or a vector with one duration per join (length \code{length(videos) - 1}).
#'   A join with \code{fade} 0 is a plain butt join -- no overlap consumed, no
#'   dissolve -- for boundaries where the next clip has no duplicated
#'   conditioning head (see \code{\link{align_overlaps}}). Default 0.375
#'   (= 9 frames @ 24fps, the default conditioning overlap).
#' @param audio Optional path to a continuous audio track to overlay as the sole
#'   audio (e.g. the track's full narration mp3).
#' @param transition xfade transition name (default \code{"dissolve"}).
#' @param cuts Optional logical vector, one per join (length \code{length(videos)
#'   - 1}). \code{TRUE} makes that join a hard cut (trim the next clip's head by
#'   that join's \code{fade} and butt-join) instead of a crossfade. \code{NULL}
#'   (default) = all crossfades. Output length is the same either way; a cut at
#'   a zero-\code{fade} join is just a butt join.
#' @param windows Optional list, one element per video: \code{c(start, end)} in
#'   seconds selecting the part of the file to feed in (\code{NA} end = to the
#'   end of file), or \code{NULL} for the whole file. For a crossfaded join the
#'   incoming window should \emph{include} the head handle the fade consumes
#'   (the fade eats \code{fade} seconds off the window's front against the
#'   previous clip's tail). Used by \code{render_timeline} to honor OTIO
#'   source ranges.
#' @param overwrite Overwrite \code{output} (default TRUE).
#' @param dry_run If TRUE, return the ffmpeg command string without running.
#' @return Invisibly, \code{output} (or the command string when dry_run).
#' @examples
#' \dontrun{
#' crossfade_concat(c("chunk01.mp4", "chunk02_chained.mp4", "chunk03_chained.mp4"),
#'                  "video.mp4", fade = 0.375, audio = "audio.mp3")
#' }
#' @export
crossfade_concat <- function(videos, output, fade = 0.375, audio = NULL,
                             transition = "dissolve", cuts = NULL,
                             windows = NULL, overwrite = TRUE,
                             dry_run = FALSE) {
    videos <- normalizePath(videos, mustWork = TRUE)
    n <- length(videos)
    if (n < 1) {
        stop("crossfade_concat(): need at least one video", call. = FALSE)
    }
    output <- normalizePath(output, mustWork = FALSE)
    if (is.null(cuts)) {
        cuts <- rep(FALSE, max(0L, n - 1L))
    }
    cuts <- as.logical(cuts)
    n_joins <- max(0L, n - 1L)
    if (!length(fade) %in% c(1L, n_joins) && n_joins > 0L) {
        stop("crossfade_concat(): fade must be a scalar or one value per join (",
             n_joins, ")", call. = FALSE)
    }
    fades <- rep_len(as.numeric(fade), n_joins)
    if (!is.null(windows) && length(windows) != n) {
        stop("crossfade_concat(): windows must have one element per video",
             call. = FALSE)
    }

    # xfade truncates SILENTLY when offset + duration lands past the first
    # input's end by even float dust, and seconds-based trims lose a frame to
    # rounding; across several joins the drift compounds and the chain
    # collapses (a 30s track once rendered 8s of motion, then froze on the
    # tail pad). So the whole chain runs in frame arithmetic: frame-exact
    # trims, frame counts in the accumulator, and a quarter-frame safety bias
    # on every xfade offset.
    # Probed lazily: the single-clip passthrough (no windows, no audio) needs
    # no fps, which keeps dry_run usable on files that don't exist yet.
    vfps <- if (n > 1L || !is.null(windows) || !is.null(audio)) {
        .video_fps(videos[1])
    } else {
        NA_real_
    }
    fadeF <- as.integer(round(fades * vfps))

    # Per-input feed: the selected window of the file (or the whole file),
    # normalised (format/timebase) so xfade and concat mix cleanly.
    win_frames <- function(i) {
        if (is.null(windows) || is.null(windows[[i]])) {
            return(NULL)
        }
        w <- windows[[i]]
        c(as.integer(round(w[1] * vfps)),
            if (is.na(w[2])) NA_integer_ else as.integer(round(w[2] * vfps)))
    }
    feed_dur <- function(i) {
        w <- win_frames(i)
        full <- suppressWarnings(as.integer(probe(videos[i], "nb_frames")))
        if (is.na(full)) {
            full <- as.integer(round(as.numeric(probe(videos[i],
                            "duration")) * vfps))
        }
        if (is.null(w)) {
            return(full)
        }
        if (is.na(w[2])) {
            full - w[1]
        } else {
            w[2] - w[1]
        }
    }
    feed_filter <- function(i) {
        wf <- win_frames(i)
        trim <- if (is.null(wf) || (wf[1] <= 0L && is.na(wf[2]))) {
            ""
        } else if (is.na(wf[2])) {
            sprintf("trim=start_frame=%d,setpts=PTS-STARTPTS,", wf[1])
        } else {
            sprintf("trim=start_frame=%d:end_frame=%d,setpts=PTS-STARTPTS,",
                    wf[1], wf[2])
        }
        sprintf("[%d:v]%sformat=yuv420p,settb=AVTB[n%d]", i - 1L, trim, i - 1L)
    }

    # Build the join chain. Each join j consumes fades[j]: a crossfade overlaps
    # the clips by fades[j]; a hard cut (cuts[j] TRUE) trims fades[j] off the
    # next clip's head -- dropping the duplicated conditioning frames -- and
    # butt-joins. fades[j] == 0 is a plain butt join either way.
    if (n == 1) {
        if (is.null(windows)) {
            w1 <- NULL
        } else {
            w1 <- windows[[1]]
        }
        if (is.null(w1) || (w1[1] <= 0 && is.na(w1[2]))) {
            vfilter <- "[0:v]null[vout]"
        } else {
            vfilter <- sub("\\[n0\\]$", "[vout]", feed_filter(1))
        }
    } else {
        durs <- vapply(seq_len(n), feed_dur, integer(1))
        parts <- vapply(seq_len(n), feed_filter, character(1))
        prev <- "[n0]"
        cum <- durs[1]
        for (k in 2:n) {
            j <- k - 1L
            f <- fadeF[j]
            if (k == n) {
                out_lbl <- "[vout]"
            } else {
                out_lbl <- sprintf("[vx%d]", k)
            }
            if (f <= 0L) {
                parts <- c(parts,
                           sprintf("%s[n%d]concat=n=2:v=1:a=0%s",
                                   prev, k - 1L, out_lbl))
            } else if (isTRUE(cuts[j])) {
                parts <- c(parts,
                           sprintf("[n%d]trim=start_frame=%d,setpts=PTS-STARTPTS[ct%d]",
                                   k - 1L, f, k - 1L),
                           sprintf("%s[ct%d]concat=n=2:v=1:a=0%s",
                                   prev, k - 1L, out_lbl))
            } else {
                # The dissolve must END on the first input's last frame:
                # offset + duration past it (by even float dust) makes xfade
                # truncate the chain silently. The quarter-frame early bias
                # keeps the boundary strict without moving the blend a
                # visible amount.
                parts <- c(parts, sprintf(
                        "%s[n%d]xfade=transition=%s:duration=%.6f:offset=%.6f%s",
                        prev, k - 1L, transition, f / vfps,
                        (cum - f - 0.25) / vfps, out_lbl))
            }
            prev <- out_lbl
            cum <- cum + durs[k] - f
        }
        vfilter <- paste(parts, collapse = ";")
    }

    inputs <- as.vector(rbind("-i", videos))
    a_input <- character(0)
    if (!is.null(audio)) {
        audio <- normalizePath(audio, mustWork = TRUE)
        a_input <- c("-i", audio)
        # The overlaid audio is the ground truth: pad the assembled video
        # (last frame held) or cut it to EXACTLY the audio's duration, so the
        # two streams always match.
        adur <- as.numeric(probe(audio, "duration"))
        n_frames <- as.integer(round(adur * vfps))
        vfilter <- sprintf("%s;[vout]tpad=stop_mode=clone:stop=-1[vfin]",
                           vfilter)
    }
    if (!is.null(audio)) {
        a_map <- c("-map", sprintf("%d:a", n))
    } else {
        a_map <- c("-map", "0:a?")
    }

    args <- c(
        if (overwrite) "-y",
              inputs, a_input,
              "-filter_complex", vfilter,
              "-map", if (is.null(audio)) "[vout]" else "[vfin]",
              a_map,
        if (!is.null(audio)) c("-frames:v", n_frames),
              "-c:v", "libx264", "-preset", "fast", "-crf", "18",
              "-c:a", "aac", "-b:a", "192k",
              "-movflags", "+faststart",
              output
    )

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}
