# frames.R
# Encode a directory of numbered frame images into a video clip. The landing
# pad for programmatically drawn animations (an OTIO ImageSequenceReference):
# something else draws frame_0001.png ... frame_NNNN.png; this turns them into
# a clip whose encode matches still_clip() and crossfade_concat() outputs.

#' Encode a Numbered Frame Sequence as a Video Clip
#'
#' Encodes \code{dir/pattern} (a C-style numbered pattern such as
#' \code{"frame_\%04d.png"}) into a video at \code{fps}. The sequence's frame
#' count sets the clip length. Encode settings match \code{\link{still_clip}},
#' so stills and frame sequences concatenate cleanly.
#'
#' @param dir Directory holding the frames.
#' @param output Path for the output video file.
#' @param fps Frame rate the sequence was drawn at (default 30).
#' @param pattern C-style filename pattern of the frames (default
#'   \code{"frame_\%04d.png"}).
#' @param start Number of the first frame (default 1).
#' @param size Output dimensions as \code{c(width, height)}, or NULL (default)
#'   to keep the source dimensions. Scaling ignores aspect; pass a size that
#'   preserves it (see \code{\link{pad}} for letterboxing instead).
#' @param overwrite If TRUE (default), overwrite the output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns the command
#'   string.
#'
#' @examples
#' \dontrun{
#' frames_clip("scene01/", "scene01.mp4", fps = 30)
#' }
#'
#' @export
frames_clip <- function(dir, output, fps = 30, pattern = "frame_%04d.png",
                        start = 1L, size = NULL, overwrite = TRUE,
                        dry_run = FALSE) {
    dir <- normalizePath(dir, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    first <- file.path(dir, sprintf(pattern, as.integer(start)))
    if (!file.exists(first)) {
        stop("frames_clip(): first frame not found: ", first, call. = FALSE)
    }

    vf <- paste0(
        if (!is.null(size)) {
            sprintf("scale=%d:%d,", as.integer(size[1]), as.integer(size[2]))
        },
                 "format=yuv420p,setsar=1")

    args <- c(if (overwrite) "-y", "-framerate", format(fps),
              "-start_number", as.integer(start), "-i",
              file.path(dir, pattern), "-vf", vf, "-c:v", "libx264",
              "-preset", "fast", "-crf", "18", "-movflags", "+faststart",
              output)

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}
