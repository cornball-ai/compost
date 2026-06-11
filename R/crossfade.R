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
#'   conditioning head (see \code{\link{detect_overlap}}). Default 0.375
#'   (= 9 frames @ 24fps, the default conditioning overlap).
#' @param audio Optional path to a continuous audio track to overlay as the sole
#'   audio (e.g. the track's full narration mp3).
#' @param transition xfade transition name (default \code{"dissolve"}).
#' @param cuts Optional logical vector, one per join (length \code{length(videos)
#'   - 1}). \code{TRUE} makes that join a hard cut (trim the next clip's head by
#'   that join's \code{fade} and butt-join) instead of a crossfade. \code{NULL}
#'   (default) = all crossfades. Output length is the same either way; a cut at
#'   a zero-\code{fade} join is just a butt join.
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
                             overwrite = TRUE, dry_run = FALSE) {
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

    # Build the join chain. Each join j consumes fades[j]: a crossfade overlaps
    # the clips by fades[j]; a hard cut (cuts[j] TRUE) trims fades[j] off the
    # next clip's head -- dropping the duplicated conditioning frames -- and
    # butt-joins. fades[j] == 0 is a plain butt join either way. Each input is
    # normalised (format/timebase) so xfade and concat mix cleanly.
    if (n == 1) {
        vfilter <- "[0:v]null[vout]"
    } else {
        durs <- vapply(videos, function(v) as.numeric(probe(v, "duration")),
                       numeric(1))
        idx <- 0:(n - 1L)
        parts <- sprintf("[%d:v]format=yuv420p,settb=AVTB[n%d]", idx, idx)
        prev <- "[n0]"
        cum <- durs[1]
        for (k in 2:n) {
            j <- k - 1L
            f <- fades[j]
            if (k == n) {
                out_lbl <- "[vout]"
            } else {
                out_lbl <- sprintf("[vx%d]", k)
            }
            if (f <= 0) {
                parts <- c(parts,
                           sprintf("%s[n%d]concat=n=2:v=1:a=0%s",
                                   prev, k - 1L, out_lbl))
            } else if (isTRUE(cuts[j])) {
                parts <- c(parts,
                           sprintf("[n%d]trim=start=%g,setpts=PTS-STARTPTS[ct%d]",
                                   k - 1L, f, k - 1L),
                           sprintf("%s[ct%d]concat=n=2:v=1:a=0%s",
                                   prev, k - 1L, out_lbl))
            } else {
                parts <- c(parts, sprintf(
                        "%s[n%d]xfade=transition=%s:duration=%g:offset=%g%s",
                        prev, k - 1L, transition, f, cum - f, out_lbl))
            }
            prev <- out_lbl
            cum <- cum + durs[k] - f
        }
        vfilter <- paste(parts, collapse = ";")
    }

    inputs <- as.vector(rbind("-i", videos))
    a_input <- character(0)
    if (!is.null(audio)) {
        a_input <- c("-i", normalizePath(audio, mustWork = TRUE))
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
              "-map", "[vout]",
              a_map,
              "-c:v", "libx264", "-preset", "fast", "-crf", "18",
              "-c:a", "aac", "-b:a", "192k",
        if (!is.null(audio)) "-shortest",
              "-movflags", "+faststart",
              output
    )

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}

