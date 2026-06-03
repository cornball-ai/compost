% tinyrox says don't edit this manually, but it can't stop you!
\name{pip}
\alias{pip}
\title{Picture-in-Picture Corner Overlay}
\usage{
pip(background, foreground, output, scale = 0.604, margin = 230,
    corner = c("upper-right", "upper-left", "lower-right", "lower-left"),
    overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{background}{Path to background video or image.}

\item{foreground}{Path to foreground video or image (the PiP).}

\item{output}{Path for output file.}

\item{scale}{Foreground scale factor (default 0.604).}

\item{margin}{Pixel margin from the corner (default 230).}

\item{corner}{One of "upper-right", "upper-left", "lower-right", "lower-left".}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Composite a foreground clip (a character, a webcam, a logo) into a corner of
the background, scaled and inset by a pixel margin. A convenience wrapper over
the overlay filter: it computes corner placement with \code{main_w}/
\code{overlay_w} expressions, which the fixed numeric offsets of
\code{\link{overlay}} cannot express. Coordinates are top-left, +Y down.
}
\examples{
\dontrun{
pip("scene.mp4", "casey.mov", "out.mp4", corner = "upper-right")
}

}
