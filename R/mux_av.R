#' Combine rendered picture and sound without re-encoding
#'
#' Copies the first video stream from `video` and the first audio stream from
#' `audio`. Timestamps are retained; no synchronization offset is applied.
#'
#' @param video File containing the video stream.
#' @param audio File containing the audio stream.
#' @param output Explicit destination MP4 filename.
#' @param overwrite Whether an existing output may be replaced.
#' @param dry_run Return the FFmpeg command without executing it.
#' @return The output path, invisibly, or a command for dry runs.
#' @examples
#' \dontrun{
#' mux_av("picture.mp4", "sound.m4a", file.path(tempdir(), "movie.mp4"))
#' }
#' @export
mux_av <- function(video, audio, output, overwrite = FALSE, dry_run = FALSE) {
    video <- normalizePath(video, mustWork = TRUE)
    audio <- normalizePath(audio, mustWork = TRUE)
    if (!dry_run && file.exists(output) && !overwrite) {
        stop("mux_av(): output already exists: ", output, call. = FALSE)
    }
    args <- c("-nostdin", if (overwrite) "-y" else "-n",
        "-i", video, "-i", audio, "-map", "0:v:0", "-map", "1:a:0",
        "-c", "copy", "-movflags", "+faststart", output)
    if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
    .run_ffmpeg(args)
    invisible(output)
}
