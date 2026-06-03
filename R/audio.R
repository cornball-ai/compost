#' Broadcast-Clean an Audio File
#'
#' The standard broadcast chain: a high-pass to kill rumble, optional 60/120/180
#' Hz hum notches plus FFT denoise, a 4:1 compressor, loudness normalisation to
#' \code{target_lufs}, and short in/out fades.
#'
#' @param input Path to input audio (or A/V) file.
#' @param output Path for output file.
#' @param dehum Apply the 60/120/180 Hz hum notches + FFT denoise (default TRUE).
#' @param target_lufs Integrated loudness target in LUFS (default -14).
#' @param fade In/out fade length in seconds (default 0.06).
#' @param highpass High-pass corner frequency in Hz (default 80).
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' broadcast_audio("raw.wav", "clean.wav")
#' broadcast_audio("take.wav", "clean.wav", dehum = FALSE, target_lufs = -16)
#' }
#'
#' @export
broadcast_audio <- function(
  input,
  output,
  dehum = TRUE,
  target_lufs = -14,
  fade = 0.06,
  highpass = 80,
  overwrite = TRUE,
  dry_run = FALSE
) {

  input <- normalizePath(input, mustWork = TRUE)
  output <- normalizePath(output, mustWork = FALSE)

  # The out-fade needs the clip length; probe the audio stream, tolerate failure.
  dur <- tryCatch(
    as.numeric(.probe_field(input, "duration", stream = "a:0")),
    error = function(e) NA_real_
  )

  af <- .broadcast_audio_filter(dehum = dehum, target_lufs = target_lufs,
                                fade = fade, duration = dur, highpass = highpass)

  args <- c(
    if (overwrite) "-y",
    "-i", input,
    "-af", af,
    output
  )

  if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
  .run_ffmpeg(args)
  invisible(output)
}

#' Build the -af filter graph for the broadcast-clean chain
#'
#' Pure string builder so the chain can be tested without ffmpeg.
#'
#' @param dehum Include the hum notches + FFT denoise.
#' @param target_lufs Loudness target in LUFS.
#' @param fade In/out fade length in seconds; 0 disables fades.
#' @param duration Clip length in seconds, for the out-fade start. NA/NULL skips
#'   the out-fade.
#' @param highpass High-pass corner frequency in Hz.
#' @return The comma-joined filter graph string.
#' @keywords internal
.broadcast_audio_filter <- function(dehum = TRUE, target_lufs = -14, fade = 0.06,
                                    duration = NULL, highpass = 80) {
  f <- sprintf("highpass=f=%g", highpass)
  if (isTRUE(dehum)) {
    notches <- vapply(c(60L, 120L, 180L), function(hz) {
      sprintf("equalizer=f=%d:width_type=q:w=12:g=-30", hz)
    }, character(1))
    f <- c(f, notches, "afftdn=nr=12:nf=-25")
  }
  f <- c(f,
         "acompressor=threshold=-22dB:ratio=4:attack=20:release=200:makeup=4",
         sprintf("loudnorm=I=%g:TP=-1.5:LRA=11", target_lufs))
  if (fade > 0) {
    f <- c(f, sprintf("afade=t=in:st=0:d=%g", fade))
    if (!is.null(duration) && !is.na(duration) && duration > fade) {
      f <- c(f, sprintf("afade=t=out:st=%g:d=%g", duration - fade, fade))
    }
  }
  paste(f, collapse = ",")
}
