% tinyrox says don't edit this manually, but it can't stop you!
\name{overlay}
\alias{overlay}
\title{Overlay Foreground onto Background}
\usage{
overlay(
  background,
  foreground,
  output,
  x = 0,
  y = 0,
  scale = NULL,
  shortest = TRUE,
  overwrite = TRUE,
  dry_run = FALSE
)
}
\arguments{
\item{background}{Path to background video or image.}

\item{foreground}{Path to foreground video or PNG (alpha supported).}

\item{output}{Path for output video file.}

\item{x}{Horizontal offset for overlay placement (default 0).}

\item{y}{Vertical offset for overlay placement (default 0).}

\item{scale}{Optional scale string for the foreground (e.g. "320:240" or "iw/2:ih/2").}

\item{shortest}{If TRUE (default), end output when the shortest input ends.}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Composite a PNG (with alpha) or video over a background video or image
using FFmpeg's overlay filter.
}
\examples{
\dontrun{
overlay("bg.mp4", "character.png", "out.mp4", x = 312, y = 580)
overlay("bg.mp4", "logo.png", "out.mp4", x = 10, y = 10, scale = "100:100")
}

}
