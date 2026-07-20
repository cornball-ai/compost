# subclip.R
# Temporal cut: extract a time range from an audio or video file. Named
# "subclip" rather than "trim" so it does not collide with rotio::trim, the OTIO
# edit verb that operates on timeline objects (compost works on media files).

#' Extract a Time Range from a Media File
#'
#' Cut \code{[start, start + duration]} (or \code{[start, end]}) out of an audio
#' or video file via ffmpeg. Re-encodes by default for frame-accurate cuts; set
#' \code{reencode = FALSE} for a fast stream copy (cuts land on keyframes).
#'
#' @param input Path to input media.
#' @param output Path for the extracted output; extension selects the format.
#' @param start Start time in seconds (default 0).
#' @param duration Length in seconds, or NULL to use \code{end} or run to the
#'   end of the file.
#' @param end End time in seconds (ignored when \code{duration} is set).
#' @param reencode If TRUE (default), re-encode for accurate cuts; if FALSE,
#'   stream-copy (fast, keyframe-aligned).
#' @param overwrite If TRUE (default), overwrite the output file.
#' @param dry_run If TRUE, return the ffmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns the command
#'   string.
#'
#' @examples
#' \dontrun{
#' subclip("audio.mp3", "chunk.mp3", start = 2.14, duration = 5.0)
#' subclip("clip.mp4", "head.mp4", start = 0, end = 3, reencode = FALSE)
#' }
#'
#' @export
subclip <- function(input, output, start = 0, duration = NULL, end = NULL,
                    reencode = TRUE, overwrite = TRUE, dry_run = FALSE) {
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    args <- c(
        if (overwrite) "-y",
              "-ss", as.character(start),
              "-i", input,
        if (!is.null(duration)) c("-t", as.character(duration)),
        if (is.null(duration) &&
              !is.null(end)) c("-to", as.character(end - start)),
        if (!reencode) c("-c", "copy"),
              output
    )

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}
