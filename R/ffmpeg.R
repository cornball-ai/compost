#' Run FFmpeg Command
#'
#' Internal helper that executes ffmpeg with the given arguments.
#' Surfaces stderr on failure.
#'
#' @param args Character vector of ffmpeg arguments.
#' @param dry_run If TRUE, return the command string instead of running it.
#' @return On success, the processx result (invisibly). On dry_run, the command string.
#' @keywords internal
#'
#' @importFrom processx run
.run_ffmpeg <- function(args, dry_run = FALSE) {
  if (dry_run) {
    return(paste("ffmpeg", paste(args, collapse = " ")))
  }

  result <- processx::run("ffmpeg", args, error_on_status = FALSE)

  if (result$status != 0) {
    stop("FFmpeg failed with status ", result$status, ":\n", result$stderr,
         call. = FALSE)
  }

  invisible(result)
}

#' Query a Single Field via ffprobe
#'
#' @param file Path to media file.
#' @param field The ffprobe field to query (e.g. "duration", "width", "height").
#' @param stream Stream specifier: "v:0" for video, "a:0" for audio.
#' @return The field value as a character string.
#' @keywords internal
.probe_field <- function(file, field, stream = "v:0") {
  file <- normalizePath(file, mustWork = TRUE)

  args <- c(
    "-v", "error",
    "-select_streams", stream,
    "-show_entries", paste0("stream=", field),
    "-of", "csv=p=0",
    file
  )

  result <- processx::run("ffprobe", args, error_on_status = FALSE)

  if (result$status != 0) {
    stop("ffprobe failed with status ", result$status, ":\n", result$stderr,
         call. = FALSE)
  }

  trimws(result$stdout)
}
