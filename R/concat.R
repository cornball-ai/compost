#' Concatenate Video Segments
#'
#' Join multiple video files sequentially using FFmpeg's concat demuxer.
#' All inputs should have compatible codecs and dimensions.
#'
#' @param inputs Character vector of paths to input video files.
#' @param output Path for output video file.
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' concat(c("part1.mp4", "part2.mp4", "part3.mp4"), "full.mp4")
#' }
#'
#' @export
concat <- function(
  inputs,
  output,
  overwrite = TRUE,
  dry_run = FALSE
) {

  if (length(inputs) < 2) stop("concat() requires at least 2 inputs", call. = FALSE)

  inputs <- normalizePath(inputs, mustWork = TRUE)
  output <- normalizePath(output, mustWork = FALSE)

  # Write concat list to temp file
  concat_list <- tempfile(fileext = ".txt")
  on.exit(unlink(concat_list), add = TRUE)
  writeLines(paste0("file '", inputs, "'"), concat_list)

  args <- c(
    if (overwrite) "-y",
    "-f", "concat",
    "-safe", "0",
    "-i", concat_list,
    "-c", "copy",
    output
  )

  if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
  .run_ffmpeg(args)
  invisible(output)
}
