#' Overlay Foreground onto Background
#'
#' Composite a PNG (with alpha) or video over a background video or image
#' using FFmpeg's overlay filter.
#'
#' @param background Path to background video or image.
#' @param foreground Path to foreground video or PNG (alpha supported).
#' @param output Path for output video file.
#' @param x Horizontal offset for overlay placement (default 0).
#' @param y Vertical offset for overlay placement (default 0).
#' @param scale Optional scale string for the foreground (e.g. "320:240" or "iw/2:ih/2").
#' @param shortest If TRUE (default), end output when the shortest input ends.
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' overlay("bg.mp4", "character.png", "out.mp4", x = 312, y = 580)
#' overlay("bg.mp4", "logo.png", "out.mp4", x = 10, y = 10, scale = "100:100")
#' }
#'
#' @export
overlay <- function(
  background,
  foreground,
  output,
  x = 0,
  y = 0,
  scale = NULL,
  shortest = TRUE,
  overwrite = TRUE,
  dry_run = FALSE
) {

  background <- normalizePath(background, mustWork = TRUE)
  foreground <- normalizePath(foreground, mustWork = TRUE)
  output <- normalizePath(output, mustWork = FALSE)

  # Build filter_complex
  if (!is.null(scale)) {
    filter <- sprintf(
      "[1:v]scale=%s[fg];[0:v][fg]overlay=%s:%s%s",
      scale, x, y,
      if (shortest) ":shortest=1" else ""
    )
  } else {
    filter <- sprintf(
      "[0:v][1:v]overlay=%s:%s%s",
      x, y,
      if (shortest) ":shortest=1" else ""
    )
  }

  args <- c(
    if (overwrite) "-y",
    "-i", background,
    "-i", foreground,
    "-filter_complex", filter,
    "-c:a", "copy",
    output
  )

  if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
  .run_ffmpeg(args)
  invisible(output)
}


#' Chromakey (Green/Blue Screen) Compositing
#'
#' Remove a colored background from foreground footage and composite
#' onto a background video or image using FFmpeg's chromakey filter.
#'
#' @param background Path to background video or image.
#' @param foreground Path to foreground video with green/blue screen.
#' @param output Path for output video file.
#' @param color Hex color to key out (default "0x00ff00" for green).
#' @param similarity Chromakey similarity threshold (default 0.1).
#'   Higher values key out more of the color.
#' @param blend Chromakey blend amount (default 0.075).
#'   Higher values create softer edges.
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' chromakey("bg.mp4", "greenscreen.mp4", "out.mp4")
#' chromakey("bg.mp4", "bluescreen.mp4", "out.mp4", color = "0x0000ff")
#' }
#'
#' @export
chromakey <- function(
  background,
  foreground,
  output,
  color = "0x00ff00",
  similarity = 0.1,
  blend = 0.075,
  overwrite = TRUE,
  dry_run = FALSE
) {

  background <- normalizePath(background, mustWork = TRUE)
  foreground <- normalizePath(foreground, mustWork = TRUE)
  output <- normalizePath(output, mustWork = FALSE)

  filter <- sprintf(
    "[1:v]chromakey=%s:%s:%s[fg];[0:v][fg]overlay",
    color, similarity, blend
  )

  args <- c(
    if (overwrite) "-y",
    "-i", background,
    "-i", foreground,
    "-filter_complex", filter,
    "-c:a", "copy",
    output
  )

  if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
  .run_ffmpeg(args)
  invisible(output)
}
