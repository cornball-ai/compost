#' Export a Single Frame at a Given Time
#'
#' Pull one frame from a video at \code{time} seconds and write it as an image.
#' A call-by-call primitive: no timeline, no asset lookup. The caller (an editor
#' such as kerNLE) decides which file and which time; this just extracts the
#' frame. Seeking is placed before \code{-i} for a fast keyframe seek.
#'
#' @param input Path to input video.
#' @param time Time in seconds to grab the frame at.
#' @param output Path for output image (e.g. .png, .jpg).
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' frame_export("scene.mp4", 4.0, "frame.png")
#' }
#'
#' @export
frame_export <- function(input, time, output, overwrite = TRUE, dry_run = FALSE) {

  input <- normalizePath(input, mustWork = TRUE)
  output <- normalizePath(output, mustWork = FALSE)

  args <- c(
    if (overwrite) "-y",
    "-ss", format(time, scientific = FALSE),
    "-i", input,
    "-frames:v", "1",
    output
  )

  if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
  .run_ffmpeg(args)
  invisible(output)
}
