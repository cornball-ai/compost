#' Run FFmpeg Command
#'
#' Internal helper that executes ffmpeg with the given arguments.
#' Surfaces stderr on failure.
#'
#' @param args Character vector of ffmpeg arguments.
#' @param dry_run If TRUE, return the command string instead of running it.
#' @return On success, the exit status (invisibly). On dry_run, the command string.
#' @keywords internal
.run_ffmpeg <- function(args, dry_run = FALSE) {
  if (dry_run) {
    return(paste("ffmpeg", paste(args, collapse = " ")))
  }

  err <- tempfile()
  on.exit(unlink(err))
  status <- system2("ffmpeg", args, stdout = FALSE, stderr = err)

  if (!identical(as.integer(status), 0L)) {
    stop("FFmpeg failed with status ", status, ":\n",
         paste(readLines(err, warn = FALSE), collapse = "\n"), call. = FALSE)
  }

  invisible(status)
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

  err <- tempfile()
  on.exit(unlink(err))
  out <- suppressWarnings(system2("ffprobe", args, stdout = TRUE, stderr = err))
  status <- attr(out, "status")

  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop("ffprobe failed with status ", status, ":\n",
         paste(readLines(err, warn = FALSE), collapse = "\n"), call. = FALSE)
  }

  trimws(paste(out, collapse = "\n"))
}

#' Is this output path a still image?
#'
#' @param path Output path.
#' @return TRUE for common still-image extensions.
#' @keywords internal
.is_image <- function(path) {
  grepl("\\.(png|jpe?g|webp|bmp|tiff?)$", path, ignore.case = TRUE)
}
