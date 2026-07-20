# layout.R
# Compose N video streams onto a canvas per a cornball layout: named slots
# with pixel rects, painted in order over a black canvas (schema doc:
# inst/schema/cornball-layout.md). One ffmpeg pass, video-only output -- the
# caller owns audio (in render_timeline the narration bed is mapped in the
# final pass, so a layout can never hijack the voice). The existing
# pip()/overlay()/vstack() primitives are deliberately NOT chained here:
# their implicit audio pick and shortest semantics are wrong for this job.

#' Compose Video Streams onto a Canvas per a Layout
#'
#' Paints each named source into its slot's pixel rect on a black canvas, in
#' slot order (first = bottom). \code{fit = "fill"} covers the rect
#' (scale-up + center-crop, for heads); \code{fit = "fit"} contains
#' (scale-down + centered pad, for content). Every chain is normalized
#' (\code{fps}, square pixels, yuv420p), non-reference chains freeze their
#' last frame if shorter, and the output is cut frame-exactly to the
#' reference source's length. The output carries no audio.
#'
#' @param sources Named character vector or list of video paths, keyed by
#'   slot name; must cover every slot in \code{layout$slots}.
#' @param output Path for the output video file.
#' @param layout A cornball layout: \code{list(schema, name, canvas,
#'   slots)}, with \code{slots} a named list of
#'   \code{list(rect = c(x, y, w, h), fit = "fill"|"fit")} in integer pixels
#'   (see \code{system.file("schema", "cornball-layout.md",
#'   package = "compost")}).
#' @param reference Slot name whose source defines the output frame rate and
#'   length (default the first source).
#' @param overwrite If TRUE (default), overwrite the output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns the
#'   command string.
#'
#' @examples
#' \dontrun{
#' layout <- list(schema = 1L, name = "vertical", canvas = c(1080L, 1920L),
#'                slots = list(
#'                    narrator = list(rect = c(0L, 0L, 1080L, 960L),
#'                                    fit = "fill"),
#'                    visual = list(rect = c(0L, 960L, 1080L, 960L),
#'                                  fit = "fit")))
#' compose_layout(c(narrator = "head.mp4", visual = "slides.mp4"),
#'                "composed.mp4", layout, reference = "visual")
#' }
#'
#' @export
compose_layout <- function(sources, output, layout,
                           reference = names(sources)[1], overwrite = TRUE,
                           dry_run = FALSE) {
    slots <- .layout_slots(layout)
    if (is.null(slots)) {
        stop("compose_layout(): unusable layout", call. = FALSE)
    }
    sources <- vapply(sources, function(p) {
        normalizePath(p, mustWork = TRUE)
    }, character(1))
    missing <- setdiff(names(slots$slots), names(sources))
    if (length(missing)) {
        stop("compose_layout(): no source for slot(s) ",
             paste(missing, collapse = ", "), call. = FALSE)
    }
    if (!reference %in% names(sources)) {
        stop("compose_layout(): reference '", reference,
             "' is not a source", call. = FALSE)
    }
    output <- normalizePath(output, mustWork = FALSE)

    ref_file <- sources[[reference]]
    fps <- .video_fps(ref_file)
    n_frames <- suppressWarnings(as.integer(probe(ref_file, "nb_frames")))
    if (is.na(n_frames)) {
        n_frames <- as.integer(round(as.numeric(
                probe(ref_file, "duration")) * fps))
    }

    # Inputs in slot (paint) order; the filter references them by index.
    ordered <- sources[names(slots$slots)]
    filter <- .layout_filter(slots, fps, reference)

    args <- c(if (overwrite) "-y",
              as.vector(rbind("-i", unname(ordered))),
              "-filter_complex", filter,
              "-map", "[vout]", "-an",
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

#' Validate and coerce a cornball layout
#'
#' JSON-deserialized layouts carry lists where integer vectors were written;
#' unify, then validate: even canvas, even rect sizes, rects in-bounds,
#' known fits. Returns NULL (with one warning) when the schema is newer than
#' supported -- render degradation; hard errors on malformed geometry --
#' writer bugs should be loud.
#'
#' @param layout The layout list.
#' @return list(canvas, slots) with integer geometry, or NULL.
#' @keywords internal
.layout_slots <- function(layout) {
    if (is.null(layout)) {
        return(NULL)
    }
    sch <- if (is.null(layout$schema)) {
        1L
    } else {
        suppressWarnings(as.integer(unlist(layout$schema)[1]))
    }
    if (is.na(sch) || sch > 1L) {
        warning("layout schema ", layout$schema,
                " is newer than this compost supports (1); ignoring layout",
                call. = FALSE)
        return(NULL)
    }
    canvas <- as.integer(unlist(layout$canvas))
    if (length(canvas) != 2L || anyNA(canvas) || any(canvas <= 0L) ||
        any(canvas %% 2L != 0L)) {
        stop("layout canvas must be two positive even integers",
             call. = FALSE)
    }
    slots <- layout$slots
    if (is.null(names(slots)) || any(names(slots) == "")) {
        stop("layout slots must be named", call. = FALSE)
    }
    slots <- lapply(slots, function(s) {
        rect <- as.integer(unlist(s$rect))
        if (length(rect) != 4L || anyNA(rect)) {
            stop("layout slot rect must be c(x, y, w, h) integers",
                 call. = FALSE)
        }
        if (any(rect[3:4] <= 0L) || any(rect[3:4] %% 2L != 0L)) {
            stop("layout slot sizes must be positive and even", call. = FALSE)
        }
        if (rect[1] < 0L || rect[2] < 0L ||
            rect[1] + rect[3] > canvas[1] || rect[2] + rect[4] > canvas[2]) {
            stop("layout slot rect out of canvas bounds", call. = FALSE)
        }
        fit <- if (is.null(s$fit)) "fit" else as.character(s$fit)
        if (!fit %in% c("fill", "fit")) {
            stop("layout slot fit must be \"fill\" or \"fit\"", call. = FALSE)
        }
        list(rect = rect, fit = fit)
    })
    list(canvas = canvas, slots = slots)
}

#' One slot's normalization chain
#'
#' fill covers the rect (scale up + center-crop); fit contains it (scale
#' down + centered black pad). Both normalize fps, SAR, pixel format, and
#' timebase so heterogeneous sources (phone footage, 10-bit, 24/25 fps)
#' overlay cleanly; non-reference chains freeze their last frame so a short
#' take never goes black.
#'
#' @param i Zero-based input index.
#' @param rect Integer c(x, y, w, h).
#' @param fit "fill" or "fit".
#' @param fps Reference frame rate.
#' @param freeze Append the freeze (tpad clone) -- FALSE for the reference.
#' @param lbl Output pad label.
#' @return One filter chain string.
#' @keywords internal
.slot_chain <- function(i, rect, fit, fps, freeze, lbl) {
    w <- rect[3]
    h <- rect[4]
    scale <- if (fit == "fill") {
        sprintf("scale=%d:%d:force_original_aspect_ratio=increase,crop=%d:%d",
                w, h, w, h)
    } else {
        sprintf(
                "scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:(ow-iw)/2:(oh-ih)/2:black",
                w, h, w, h)
    }
    sprintf("[%d:v]fps=%s,%s,setsar=1,format=yuv420p,settb=AVTB%s%s",
            i, format(fps), scale,
            if (freeze) ",tpad=stop_mode=clone:stop=-1" else "",
            lbl)
}

#' Build the full compose filter graph
#'
#' A black canvas source at the reference rate, one normalization chain per
#' slot (inputs in slot order), then overlays in paint order. Everything is
#' infinite-or-held; the caller cuts frame-exactly with -frames:v (the
#' crossfade_concat discipline -- no shortest= anywhere).
#'
#' @param slots A validated \code{.layout_slots()} result.
#' @param fps Reference frame rate.
#' @param reference Reference slot name (no freeze on its chain).
#' @return The filter_complex string.
#' @keywords internal
.layout_filter <- function(slots, fps, reference) {
    canvas <- slots$canvas
    nms <- names(slots$slots)
    parts <- sprintf("color=c=black:s=%dx%d:r=%s,settb=AVTB[cv]",
                     canvas[1], canvas[2], format(fps))
    for (i in seq_along(nms)) {
        s <- slots$slots[[i]]
        parts <- c(parts, .slot_chain(i - 1L, s$rect, s$fit, fps,
                                      freeze = !identical(nms[i], reference),
                                      lbl = sprintf("[s%d]", i)))
    }
    prev <- "[cv]"
    for (i in seq_along(nms)) {
        out <- if (i == length(nms)) "[vout]" else sprintf("[t%d]", i)
        parts <- c(parts, sprintf("%s[s%d]overlay=%d:%d%s", prev, i,
                                  slots$slots[[i]]$rect[1],
                                  slots$slots[[i]]$rect[2], out))
        prev <- out
    }
    paste(parts, collapse = ";")
}
