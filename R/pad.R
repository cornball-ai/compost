#' Scale to Fit and Pad onto a Canvas
#'
#' Scales \code{input} to fit within \code{fit_width} x \code{fit_height}
#' (preserving aspect ratio, never upscaling past the box), then pads the result
#' onto a \code{width} x \code{height} canvas at \code{x},\code{y} filled with
#' \code{color}. This is the ffmpeg
#' \code{scale=...:force_original_aspect_ratio=decrease,pad=...} idiom for
#' letterboxing media into a fixed frame (e.g. a square clip into 1080x1920
#' vertical shorts with the video at the top and a black caption strip below).
#'
#' @param input,output Input and output media paths.
#' @param width,height Final canvas dimensions (pixels).
#' @param fit_width,fit_height Box to scale the input into before padding
#'   (default the canvas dimensions).
#' @param x,y Position of the scaled input on the canvas as ffmpeg \code{pad}
#'   expressions. Default centers it (\code{"(ow-iw)/2"}, \code{"(oh-ih)/2"});
#'   pass \code{0}, \code{0} to top-left align (e.g. video at the top of a taller
#'   canvas).
#' @param color Pad color (default \code{"black"}).
#' @param overwrite Overwrite the output if it exists (default TRUE).
#' @param dry_run If TRUE, return the ffmpeg command string instead of running.
#' @return The output path, invisibly (or the command string on \code{dry_run}).
#' @examples
#' \dontrun{
#' # Letterbox a square clip into vertical shorts, video at the top:
#' pad("clip.mp4", "shorts.mp4", 1080, 1920,
#'     fit_width = 1080, fit_height = 1080, x = 0, y = 0)
#' }
#' @export
pad <- function(input, output, width, height, fit_width = width,
                fit_height = height, x = "(ow-iw)/2", y = "(oh-ih)/2",
                color = "black", overwrite = TRUE, dry_run = FALSE) {
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    vf <- sprintf(
                  "scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:%s:%s:%s",
                  as.integer(fit_width), as.integer(fit_height),
                  as.integer(width), as.integer(height), x, y, color)

    args <- c(if (overwrite) "-y", "-i", input, "-vf", vf, output)

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}

