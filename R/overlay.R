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
overlay <- function(background, foreground, output, x = 0, y = 0,
                    scale = NULL, shortest = TRUE, overwrite = TRUE,
                    dry_run = FALSE) {
    background <- normalizePath(background, mustWork = TRUE)
    foreground <- normalizePath(foreground, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    # Build filter_complex
    if (!is.null(scale)) {
        filter <- sprintf("[1:v]scale=%s[fg];[0:v][fg]overlay=%s:%s%s", scale,
                          x, y, if (shortest) ":shortest=1" else "")
    } else {
        filter <- sprintf(
                          "[0:v][1:v]overlay=%s:%s%s",
                          x, y,
            if (shortest) ":shortest=1" else ""
        )
    }

    img <- .is_image(output)
    args <- c(
        if (overwrite) "-y",
              "-i", background,
              "-i", foreground,
              "-filter_complex", filter,
        if (img) c("-frames:v", "1") else c("-c:a", "copy"),
              output
    )

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
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
chromakey <- function(background, foreground, output, color = "0x00ff00",
                      similarity = 0.1, blend = 0.075, overwrite = TRUE,
                      dry_run = FALSE) {
    background <- normalizePath(background, mustWork = TRUE)
    foreground <- normalizePath(foreground, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    filter <- sprintf("[1:v]chromakey=%s:%s:%s[fg];[0:v][fg]overlay", color,
                      similarity, blend)

    args <- c(
        if (overwrite) "-y",
              "-i", background,
              "-i", foreground,
              "-filter_complex", filter,
              "-c:a", "copy",
              output
    )

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}

#' Picture-in-Picture Corner Overlay
#'
#' Composite a foreground clip (a character, a webcam, a logo) into a corner of
#' the background, scaled and inset by a pixel margin. A convenience wrapper over
#' the overlay filter: it computes corner placement with \code{main_w}/
#' \code{overlay_w} expressions, which the fixed numeric offsets of
#' \code{\link{overlay}} cannot express. Coordinates are top-left, +Y down.
#'
#' @param background Path to background video or image.
#' @param foreground Path to foreground video or image (the PiP).
#' @param output Path for output file.
#' @param scale Foreground scale factor (default 0.604).
#' @param margin Pixel margin from the corner (default 230).
#' @param corner One of "upper-right", "upper-left", "lower-right", "lower-left".
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' pip("scene.mp4", "casey.mov", "out.mp4", corner = "upper-right")
#' }
#'
#' @export
pip <- function(background, foreground, output, scale = 0.604, margin = 230,
                corner = c("upper-right", "upper-left", "lower-right", "lower-left"),
                overwrite = TRUE, dry_run = FALSE) {
    corner <- match.arg(corner)
    background <- normalizePath(background, mustWork = TRUE)
    foreground <- normalizePath(foreground, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    filter <- .pip_filter(scale, margin, corner)

    args <- c(if (overwrite) "-y", "-i", background, "-i", foreground,
              "-filter_complex", filter,
        if (.is_image(output)) c("-frames:v", "1"), output)

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}

#' Build the PiP corner overlay filter_complex
#'
#' @param scale Foreground scale factor.
#' @param margin Pixel margin from the corner.
#' @param corner Corner name (matched on "right"/"lower").
#' @return The filter_complex string.
#' @keywords internal
.pip_filter <- function(scale = 0.604, margin = 230, corner = "upper-right") {
    x <- if (grepl("right", corner)) {
        sprintf("main_w-overlay_w-%g", margin)
    } else {
        sprintf("%g", margin)
    }
    y <- if (grepl("lower", corner)) {
        sprintf("main_h-overlay_h-%g", margin)
    } else {
        sprintf("%g", margin)
    }
    sprintf("[1:v]scale=iw*%g:ih*%g[pip];[0:v][pip]overlay=%s:%s", scale,
            scale, x, y)
}

#' Colour-Key to a ProRes 4444 File with Alpha
#'
#' Key out a flat background colour and write ProRes 4444 (\code{yuva444p10le}) so
#' the alpha channel survives. Where \code{\link{chromakey}} composites the keyed
#' foreground straight onto a background, this writes a standalone alpha clip you
#' overlay later. By default the key colour is auto-sampled from a corner pixel
#' (generated backgrounds are sometimes black, sometimes grey); pass
#' \code{color} to override.
#'
#' @param input Path to input video.
#' @param output Path for output .mov file.
#' @param color Hex colour (e.g. "0x000000"), a name ("green"), or "auto" to
#'   sample it from a corner pixel (default).
#' @param similarity Key similarity threshold (default 0.15).
#' @param blend Key edge blend (default 0.05).
#' @param sample_xy Pixel c(x, y) to sample when color = "auto" (default c(0, 0)).
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' colorkey("ltx.mp4", "ltx_alpha.mov")
#' colorkey("scene.mp4", "scene_alpha.mov", color = "0x000000")
#' }
#'
#' @export
colorkey <- function(input, output, color = "auto", similarity = 0.15,
                     blend = 0.05, sample_xy = c(0, 0), overwrite = TRUE,
                     dry_run = FALSE) {
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    if (identical(color, "auto")) {
        color <- .sample_corner_color(input, sample_xy[1], sample_xy[2])
    }

    filter <- sprintf("colorkey=%s:%g:%g", color, similarity, blend)

    args <- c(if (overwrite) "-y", "-i", input, "-vf", filter, "-c:v",
              "prores_ks", "-profile:v", "4444", "-pix_fmt",
              "yuva444p10le", output)

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}

#' Sample one pixel's colour from the first frame as 0xRRGGBB
#'
#' @param file Path to media file (already validated by the caller).
#' @param x Pixel x coordinate to sample.
#' @param y Pixel y coordinate to sample.
#' @return Colour string "0xRRGGBB".
#' @keywords internal
.sample_corner_color <- function(file, x = 0, y = 0) {
    tmp <- tempfile(fileext = ".raw")
    on.exit(unlink(tmp))
    # format=rgb24 before the 1x1 crop: a 4:2:0 source can't be cropped to 1px
    # (the chroma plane would be sub-pixel), so convert to full-res RGB first.
    .run_ffmpeg(c("-v", "error", "-y", "-i", file, "-vf",
                  sprintf("format=rgb24,crop=1:1:%d:%d", x, y), "-frames:v",
                  "1", "-f", "rawvideo", "-pix_fmt", "rgb24", tmp))
    rgb <- readBin(tmp, "integer", n = 3L, size = 1L, signed = FALSE)
    if (length(rgb) < 3L) {
        stop("could not sample a pixel colour from ", file, call. = FALSE)
    }
    sprintf("0x%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}
