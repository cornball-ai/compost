#' Run FFmpeg Command
#'
#' Internal helper that executes ffmpeg with the given arguments.
#' Surfaces stderr on failure.
#'
#' @param args Character vector of ffmpeg arguments.
#' @param dry_run If TRUE, return the command string instead of running it.
#' @return On success, the captured stderr lines (invisibly), so callers that
#'   parse ffmpeg's log (e.g. \code{ametadata=print}, which writes to stderr)
#'   can read them. On dry_run, the command string.
#' @keywords internal
.run_ffmpeg <- function(args, dry_run = FALSE) {
    if (dry_run) {
        return(paste("ffmpeg", paste(args, collapse = " ")))
    }

    # shQuote() each arg: filter graphs carry ; [ ] ' and other metacharacters
    # the shell would otherwise split or glob before ffmpeg sees them. system2()
    # runs via sh -c on Unix and cmd.exe on Windows; shQuote() adapts its quoting
    # style to the platform, so this holds on both (verified against Windows
    # ffmpeg: real overlay/chromakey/concat filtergraphs survive intact).
    err <- suppressWarnings(system2("ffmpeg", shQuote(args), stdout = FALSE,
                                    stderr = TRUE))
    status <- attr(err, "status")

    if (!is.null(status) && !identical(as.integer(status), 0L)) {
        stop("FFmpeg failed with status ", status, ":\n",
             paste(err, collapse = "\n"), call. = FALSE)
    }

    invisible(err)
}

#' Run ffprobe Command
#'
#' Internal helper that executes ffprobe with the given arguments and returns
#' its stdout. The single exec site for every ffprobe query in the package.
#'
#' @param args Character vector of ffprobe arguments.
#' @return Character vector of ffprobe's stdout lines.
#' @keywords internal
.run_ffprobe <- function(args) {
    # system2() captures via system() -> sh -c, so shQuote each arg (file paths may
    # contain spaces). stderr discarded; callers pass -v error, so a non-zero
    # status is the signal we act on.
    out <- suppressWarnings(system2("ffprobe", shQuote(args), stdout = TRUE,
                                    stderr = FALSE))
    status <- attr(out, "status")

    if (!is.null(status) && !identical(as.integer(status), 0L)) {
        stop("ffprobe failed with status ", status, call. = FALSE)
    }

    out
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

    out <- .run_ffprobe(c("-v", "error", "-select_streams", stream,
                          "-show_entries", paste0("stream=", field), "-of",
                          "csv=p=0", file))

    trimws(paste(out, collapse = "\n"))
}

#' Query a Container-Level Field via ffprobe
#'
#' Reads a format (container) field rather than a stream field, so it works for
#' audio-only and video files alike. Used for duration, which is not always
#' present on a given stream.
#'
#' @param file Path to media file.
#' @param field The ffprobe format field (e.g. "duration").
#' @return The field value as a character string.
#' @keywords internal
.probe_format_field <- function(file, field) {
    file <- normalizePath(file, mustWork = TRUE)

    out <- .run_ffprobe(c("-v", "error", "-show_entries",
                          paste0("format=", field), "-of", "csv=p=0", file))

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
