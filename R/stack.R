#' Vertically Stack Two Videos
#'
#' Stack two videos vertically (top over bottom) using FFmpeg's vstack filter.
#' Useful for YouTube Shorts layout (e.g. SadTalker top + slideshow bottom).
#'
#' Both inputs are scaled to the target width. Heights can be specified
#' individually; by default each gets half the frame.
#'
#' @param top Path to top video.
#' @param bottom Path to bottom video.
#' @param output Path for output video file.
#' @param height_top Height in pixels for top video (default: auto from width).
#' @param height_bottom Height in pixels for bottom video (default: auto from width).
#' @param width Output width in pixels (default 1080).
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' vstack("talking_head.mp4", "slideshow.mp4", "shorts.mp4")
#' vstack("top.mp4", "bottom.mp4", "out.mp4", height_top = 960, height_bottom = 960)
#' }
#'
#' @export
vstack <- function(
  top,
  bottom,
  output,
  height_top = NULL,
  height_bottom = NULL,
  width = 1080,
  overwrite = TRUE,
  dry_run = FALSE
) {

  top <- normalizePath(top, mustWork = TRUE)
  bottom <- normalizePath(bottom, mustWork = TRUE)
  output <- normalizePath(output, mustWork = FALSE)

  # Build scale filters
  scale_top <- if (!is.null(height_top)) {
    sprintf("[0:v]scale=%d:%d:force_original_aspect_ratio=disable[top]", width, height_top)
  } else {
    sprintf("[0:v]scale=%d:-2[top]", width)
  }

  scale_bottom <- if (!is.null(height_bottom)) {
    sprintf("[1:v]scale=%d:%d:force_original_aspect_ratio=disable[bottom]", width, height_bottom)
  } else {
    sprintf("[1:v]scale=%d:-2[bottom]", width)
  }

  filter <- paste0(scale_top, ";", scale_bottom, ";[top][bottom]vstack=inputs=2")

  args <- c(
    if (overwrite) "-y",
    "-i", top,
    "-i", bottom,
    "-filter_complex", filter,
    "-c:a", "copy",
    output
  )

  if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
  .run_ffmpeg(args)
  invisible(output)
}


#' Horizontally Stack Two Videos
#'
#' Stack two videos side by side using FFmpeg's hstack filter.
#'
#' @param left Path to left video.
#' @param right Path to right video.
#' @param output Path for output video file.
#' @param width_left Width in pixels for left video (default: auto from height).
#' @param width_right Width in pixels for right video (default: auto from height).
#' @param height Output height in pixels (default 1080).
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' hstack("left.mp4", "right.mp4", "wide.mp4")
#' }
#'
#' @export
hstack <- function(
  left,
  right,
  output,
  width_left = NULL,
  width_right = NULL,
  height = 1080,
  overwrite = TRUE,
  dry_run = FALSE
) {

  left <- normalizePath(left, mustWork = TRUE)
  right <- normalizePath(right, mustWork = TRUE)
  output <- normalizePath(output, mustWork = FALSE)

  scale_left <- if (!is.null(width_left)) {
    sprintf("[0:v]scale=%d:%d:force_original_aspect_ratio=disable[left]", width_left, height)
  } else {
    sprintf("[0:v]scale=-2:%d[left]", height)
  }

  scale_right <- if (!is.null(width_right)) {
    sprintf("[1:v]scale=%d:%d:force_original_aspect_ratio=disable[right]", width_right, height)
  } else {
    sprintf("[1:v]scale=-2:%d[right]", height)
  }

  filter <- paste0(scale_left, ";", scale_right, ";[left][right]hstack=inputs=2")

  args <- c(
    if (overwrite) "-y",
    "-i", left,
    "-i", right,
    "-filter_complex", filter,
    "-c:a", "copy",
    output
  )

  if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
  .run_ffmpeg(args)
  invisible(output)
}
