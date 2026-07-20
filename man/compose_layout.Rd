% tinyrox says don't edit this manually, but it can't stop you!
\name{compose_layout}
\alias{compose_layout}
\title{Compose Video Streams onto a Canvas per a Layout}
\usage{
compose_layout(sources, output, layout, reference = names(sources)[1],
               overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{sources}{Named character vector or list of video paths, keyed by
slot name; must cover every slot in \code{layout$slots}.}

\item{output}{Path for the output video file.}

\item{layout}{A cornball layout: \code{list(schema, name, canvas,
slots)}, with \code{slots} a named list of
\code{list(rect = c(x, y, w, h), fit = "fill"|"fit")} in integer pixels
(see \code{system.file("schema", "cornball-layout.md",
package = "compost")}).}

\item{reference}{Slot name whose source defines the output frame rate and
length (default the first source).}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns the
  command string.
}
\description{
Paints each named source into its slot's pixel rect on a black canvas, in
slot order (first = bottom). \code{fit = "fill"} covers the rect
(scale-up + center-crop, for heads); \code{fit = "fit"} contains
(scale-down + centered pad, for content). Every chain is normalized
(\code{fps}, square pixels, yuv420p), non-reference chains freeze their
last frame if shorter, and the output is cut frame-exactly to the
reference source's length. The output carries no audio.
}
\examples{
\dontrun{
layout <- list(schema = 1L, name = "vertical", canvas = c(1080L, 1920L),
               slots = list(
                   narrator = list(rect = c(0L, 0L, 1080L, 960L),
                                   fit = "fill"),
                   visual = list(rect = c(0L, 960L, 1080L, 960L),
                                 fit = "fit")))
compose_layout(c(narrator = "head.mp4", visual = "slides.mp4"),
               "composed.mp4", layout, reference = "visual")
}

}
