# convert.R
# Plain format/parameter conversion. Distinct from the broadcast mastering chain
# in audio.R: this just re-encodes, resamples, or remixes without altering the
# signal beyond what those parameters imply.

#' Convert an Audio File
#'
#' Re-encode an audio (or A/V) file's audio to a new container, sample rate,
#' channel count, or codec via ffmpeg. The output format is inferred from the
#' output extension. With no optional arguments it is a straight transcode.
#'
#' Resampling to 16 kHz mono is the usual preparation for speech-to-text:
#' \code{audio_convert("audio.mp3", "audio.wav", sample_rate = 16000)}.
#'
#' @param input Path to input audio (or A/V) file.
#' @param output Path for output file; extension selects the format.
#' @param sample_rate Output sample rate in Hz, or NULL to keep the source rate.
#' @param channels Output channel count (1 = mono, 2 = stereo), or NULL to keep.
#' @param bitrate Output audio bitrate (e.g. "192k"), or NULL for the encoder
#'   default. Ignored by lossless formats such as WAV.
#' @param codec Explicit audio codec (e.g. "libmp3lame", "pcm_s16le"), or NULL to
#'   let ffmpeg pick from the output extension.
#' @param overwrite If TRUE (default), overwrite the output file.
#' @param dry_run If TRUE, return the ffmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns the command
#'   string.
#'
#' @examples
#' \dontrun{
#' audio_convert("audio.mp3", "speech.wav", sample_rate = 16000)
#' audio_convert("take.wav", "take.mp3", bitrate = "192k")
#' }
#'
#' @export
audio_convert <- function(input, output, sample_rate = NULL, channels = NULL,
                          bitrate = NULL, codec = NULL, overwrite = TRUE,
                          dry_run = FALSE) {
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    args <- c(if (overwrite) "-y", "-i", input, "-vn",
        if (!is.null(sample_rate)) c("-ar", as.character(sample_rate)),
        if (!is.null(channels)) c("-ac", as.character(channels)),
        if (!is.null(codec)) c("-c:a", codec),
        if (!is.null(bitrate)) c("-b:a", bitrate), output)

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}
