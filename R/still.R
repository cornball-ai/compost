# still.R
# Turn a single still image into a video clip, optionally animated by a
# deterministic Ken Burns pan/zoom. Static and animated clips run through the
# same zoompan path and the same encode settings as crossfade_concat(), so the
# outputs stay concat-compatible with each other and with rendered chains.

#' Render a Still Image as a Video Clip
#'
#' Turns one image into a video of \code{duration} seconds, optionally
#' animated by a deterministic Ken Burns pan/zoom described by \code{motion}.
#' Both the static and the animated case go through ffmpeg's \code{zoompan},
#' so the outputs are byte-compatible for \code{\link{concat}} and
#' \code{\link{crossfade_concat}} regardless of motion.
#'
#' \code{zoompan} stretches its crop to \code{size} without preserving the
#' source aspect ratio, so \code{size} should match the source's aspect (or a
#' deliberate crop of it). Renderers letterbox afterwards; see
#' \code{\link{pad}}.
#'
#' @param image Path to the input image (png, jpg, ...).
#' @param output Path for the output video file.
#' @param duration Clip length in seconds.
#' @param fps Output frame rate (default 30).
#' @param size Output dimensions as \code{c(width, height)}, or NULL (default)
#'   to use the source dimensions rounded to even.
#' @param motion NULL (default) for a static clip, or a Ken Burns spec: a list
#'   with any of \code{zoom_from}, \code{zoom_to} (numeric >= 1; values below 1
#'   are clamped), \code{from}, \code{to} (crop-window anchor as \code{c(x, y)}
#'   fractions of the source, top-left origin; the window is centered on the
#'   anchor and clamped to the frame), and \code{ease} (\code{"linear"} or
#'   \code{"smooth"}, default smooth).
#' @param overwrite If TRUE (default), overwrite the output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns the command
#'   string.
#'
#' @examples
#' \dontrun{
#' still_clip("slide01.png", "slide01.mp4", duration = 4.2)
#' still_clip("slide01.png", "slide01.mp4", duration = 4.2,
#'            motion = list(zoom_from = 1, zoom_to = 1.15,
#'                          from = c(0.5, 0.5), to = c(0.5, 0.4)))
#' }
#'
#' @export
still_clip <- function(image, output, duration, fps = 30, size = NULL,
                       motion = NULL, overwrite = TRUE, dry_run = FALSE) {
    image <- normalizePath(image, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)

    n_frames <- as.integer(round(duration * fps))
    if (is.na(n_frames) || n_frames < 1L) {
        stop("still_clip(): duration must cover at least one frame",
             call. = FALSE)
    }

    if (is.null(size)) {
        w <- as.integer(probe(image, "width"))
        h <- as.integer(probe(image, "height"))
        size <- c(2L * as.integer(round(w / 2)), 2L * as.integer(round(h / 2)))
    }

    zp <- .kenburns_filter(motion, n_frames, size[1], size[2], fps)
    vf <- paste0(zp, ",format=yuv420p,setsar=1")

    args <- c(if (overwrite) "-y", "-i", image, "-vf", vf,
              "-frames:v", n_frames,
              "-c:v", "libx264", "-preset", "fast", "-crf", "18",
              "-movflags", "+faststart",
              output)

    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}

#' Build the zoompan filter for a still clip
#'
#' Pure string builder so the motion math can be tested without ffmpeg. The
#' zoom and anchor interpolate from their \code{*_from}/\code{from} values at
#' the first frame to \code{*_to}/\code{to} at the last, along the easing
#' curve. The crop window is centered on the anchor and clamped to the frame,
#' all in ffmpeg expressions of \code{iw}/\code{ih}, so no source dimensions
#' are needed here.
#'
#' @param motion Ken Burns spec list (see \code{\link{still_clip}}), or NULL
#'   for a static clip.
#' @param n_frames Total output frames.
#' @param out_w,out_h Output dimensions in pixels.
#' @param fps Output frame rate.
#' @return The zoompan filter string.
#' @keywords internal
.kenburns_filter <- function(motion, n_frames, out_w, out_h, fps) {
    if (is.null(motion)) {
        z <- "1"
        x <- "0"
        y <- "0"
    } else {
        zf <- max(1, .motion_num(motion$zoom_from, 1))
        zt <- max(1, .motion_num(motion$zoom_to, 1))
        from <- .motion_xy(motion$from, c(0.5, 0.5))
        to <- .motion_xy(motion$to, from)
        ease <- if (is.null(motion$ease)) "smooth" else motion$ease

        e <- .ease_expr(ease, max(1L, n_frames - 1L))
        z <- .lerp_expr(zf, zt, e)
        ax <- .lerp_expr(from[1], to[1], e)
        ay <- .lerp_expr(from[2], to[2], e)
        x <- sprintf("clip((%s)*iw-iw/zoom/2,0,iw-iw/zoom)", ax)
        y <- sprintf("clip((%s)*ih-ih/zoom/2,0,ih-ih/zoom)", ay)
    }

    sprintf("zoompan=z='%s':x='%s':y='%s':d=%d:s=%dx%d:fps=%s",
            z, x, y, as.integer(n_frames),
            as.integer(out_w), as.integer(out_h), format(fps))
}

#' Easing expression over the frame counter
#'
#' Progress p runs 0 to 1 over frames 0..n1, clamped at 1 so a trailing frame
#' from rounding can never overshoot the end state.
#'
#' @param ease "linear" or "smooth" (smoothstep).
#' @param n1 Index of the last frame (frames - 1, at least 1).
#' @return The easing expression string in terms of \code{on}.
#' @keywords internal
.ease_expr <- function(ease, n1) {
    p <- sprintf("min(on/%d,1)", as.integer(n1))
    switch(ease,
           linear = p,
           smooth = sprintf("pow(%s,2)*(3-2*%s)", p, p),
           stop("still_clip(): unknown ease '", ease, "'", call. = FALSE))
}

#' Interpolation expression between two values along an easing curve
#'
#' @param a,b Start and end values.
#' @param e Easing expression string.
#' @return A constant when a == b, else \code{a+(b-a)*e}.
#' @keywords internal
.lerp_expr <- function(a, b, e) {
    if (isTRUE(all.equal(a, b))) {
        return(sprintf("%.6f", a))
    }
    d <- b - a
    sprintf("%.6f%s%.6f*%s", a, if (d < 0) "-" else "+", abs(d), e)
}

#' Coerce one motion field to numeric with a default
#'
#' JSON-deserialized specs may carry integers or single-element lists where
#' doubles were written; unify here.
#'
#' @param v Field value.
#' @param default Value when v is NULL.
#' @return A numeric scalar.
#' @keywords internal
.motion_num <- function(v, default) {
    if (is.null(v)) {
        return(default)
    }
    as.numeric(unlist(v))[1]
}

#' Coerce a motion anchor to a length-2 numeric
#'
#' @param v Field value (vector or list of two numbers).
#' @param default Value when v is NULL.
#' @return A numeric vector of length 2.
#' @keywords internal
.motion_xy <- function(v, default) {
    if (is.null(v)) {
        return(default)
    }
    v <- as.numeric(unlist(v))
    if (length(v) != 2L || anyNA(v)) {
        stop("still_clip(): motion anchors must be two numbers", call. = FALSE)
    }
    v
}

#' Fit source dimensions into a framing box
#'
#' Aspect-fits \code{c(img_w, img_h)} into the framing's \code{pad} box (either
#' direction -- upscale or downscale), rounded to the nearest even pixel as
#' libx264 requires. With no framing, just even-rounds the source dimensions.
#' This is what keeps zoompan from stretching a slide: the clip is rendered at
#' the source's own aspect and letterboxed later.
#'
#' @param img_w,img_h Source dimensions in pixels.
#' @param framing A framing list with a \code{pad = c(w, h)} element, or NULL.
#' @return \code{c(width, height)}, both even.
#' @keywords internal
.still_size <- function(img_w, img_h, framing = NULL) {
    if (is.null(framing) || is.null(framing$pad)) {
        w <- img_w
        h <- img_h
    } else {
        box <- as.numeric(unlist(framing$pad))
        s <- min(box[1] / img_w, box[2] / img_h)
        w <- img_w * s
        h <- img_h * s
    }
    c(2L * as.integer(round(w / 2)), 2L * as.integer(round(h / 2)))
}
