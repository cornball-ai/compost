# audio_concat.R
# Join audio files end to end with exact silences between and after them.
# Distinct from concat(): that stream-copies compatible videos; this
# normalizes heterogeneous audio (rate/layout) through the concat FILTER and
# re-encodes once, inserting anullsrc-generated gaps -- built for assembling a
# narration bed from per-segment TTS takes.

#' Concatenate Audio Files with Exact Silence Gaps
#'
#' Joins \code{inputs} in order into one audio file, inserting \code{gap}
#' seconds of silence between consecutive inputs and \code{tail} seconds after
#' the last. Every input is resampled and remixed to a common format first, so
#' inputs need not match. One encode, chosen by the output extension.
#'
#' @param inputs Character vector of audio file paths, in order.
#' @param output Path for the output audio file; extension selects the format.
#' @param gap Silence in seconds between consecutive inputs (default 0).
#' @param tail Silence in seconds appended after the last input (default 0).
#' @param sample_rate Output sample rate in Hz, or NULL (default) to use the
#'   first input's rate.
#' @param channels Output channel count (1 = mono, 2 = stereo), or NULL
#'   (default) to use the first input's count.
#' @param bitrate Output audio bitrate (e.g. "192k"), or NULL for the encoder
#'   default. Ignored by lossless formats such as WAV.
#' @param overwrite If TRUE (default), overwrite the output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns the command
#'   string.
#'
#' @examples
#' \dontrun{
#' audio_concat(c("slide01.wav", "slide02.wav"), "audio.mp3",
#'              gap = 0.35, tail = 0.5)
#' }
#'
#' @export
audio_concat <- function(inputs, output, gap = 0, tail = 0, sample_rate = NULL,
                         channels = NULL, bitrate = NULL, overwrite = TRUE,
                         dry_run = FALSE) {
    inputs <- normalizePath(inputs, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)
    n <- length(inputs)
    if (n < 1L) {
        stop("audio_concat(): need at least one input", call. = FALSE)
    }

    if (is.null(sample_rate)) {
        sample_rate <- as.integer(.probe_field(inputs[1], "sample_rate", "a:0"))
    }
    if (is.null(channels)) {
        channels <- as.integer(.probe_field(inputs[1], "channels", "a:0"))
    }

    filter <- .audio_concat_filter(n, gap, tail, sample_rate, channels)

    args <- c(if (overwrite) "-y",
              as.vector(rbind("-i", inputs)),
              "-filter_complex", filter,
              "-map", "[aout]",
        if (!is.null(bitrate)) c("-b:a", bitrate),
              output)

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}

#' Build the filter graph for audio_concat
#'
#' Pure string builder so the graph can be tested without ffmpeg. Each input
#' is normalized (aresample + aformat) to the common rate and layout; gaps and
#' the tail are anullsrc sources trimmed to length; everything runs through
#' one concat filter to \code{[aout]}.
#'
#' @param n Number of inputs.
#' @param gap Silence between consecutive inputs, seconds.
#' @param tail Silence after the last input, seconds.
#' @param sample_rate Common sample rate in Hz.
#' @param channels Common channel count (1 or 2).
#' @return The filter_complex string.
#' @keywords internal
.audio_concat_filter <- function(n, gap, tail, sample_rate, channels) {
    layout <- switch(as.character(channels),
                     "1" = "mono",
                     "2" = "stereo",
                     stop("audio_concat(): channels must be 1 or 2",
                          call. = FALSE))

    silence <- function(len, lbl) {
        sprintf("anullsrc=r=%d:cl=%s,atrim=end=%.6f%s",
                sample_rate, layout, len, lbl)
    }

    parts <- character(0)
    labels <- character(0)
    for (i in seq_len(n)) {
        lbl <- sprintf("[c%d]", i - 1L)
        parts <- c(parts, sprintf(
                "[%d:a]aresample=%d,aformat=sample_fmts=fltp:channel_layouts=%s%s",
                i - 1L, sample_rate, layout, lbl))
        labels <- c(labels, lbl)
        if (gap > 0 && i < n) {
            glbl <- sprintf("[g%d]", i)
            parts <- c(parts, silence(gap, glbl))
            labels <- c(labels, glbl)
        }
    }
    if (tail > 0) {
        parts <- c(parts, silence(tail, "[gt]"))
        labels <- c(labels, "[gt]")
    }

    if (length(labels) == 1L) {
        # Single input, no silences: rename the normalized stream directly.
        return(sub("\\[c0\\]$", "[aout]", parts))
    }

    paste(c(parts, sprintf("%sconcat=n=%d:v=0:a=1[aout]",
                           paste(labels, collapse = ""), length(labels))),
          collapse = ";")
}
