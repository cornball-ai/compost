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

  # system2() captures output via system() -> sh -c, so shQuote each arg: filter
  # graphs carry ; [ ] ' and other shell metacharacters that would otherwise be
  # split or globbed before ffmpeg ever sees them.
  err <- suppressWarnings(system2("ffmpeg", shQuote(args), stdout = FALSE,
                                  stderr = TRUE))
  status <- attr(err, "status")

  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop("FFmpeg failed with status ", status, ":\n",
         paste(err, collapse = "\n"), call. = FALSE)
  }

  invisible(0L)
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

  # system2() captures via system() -> sh -c, so shQuote each arg (file paths may
  # contain spaces). stderr discarded; ffprobe runs with -v error, so a non-zero
  # status is the signal we act on.
  out <- suppressWarnings(system2("ffprobe", shQuote(args), stdout = TRUE,
                                  stderr = FALSE))
  status <- attr(out, "status")

  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop("ffprobe failed with status ", status, call. = FALSE)
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
