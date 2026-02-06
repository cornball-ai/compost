% tinyrox says don't edit this manually, but it can't stop you!
\name{vstack}
\alias{vstack}
\title{Vertically Stack Two Videos}
\usage{
vstack(
  top,
  bottom,
  output,
  height_top = NULL,
  height_bottom = NULL,
  width = 1080,
  overwrite = TRUE,
  dry_run = FALSE
)
}
\arguments{
\item{top}{Path to top video.}

\item{bottom}{Path to bottom video.}

\item{output}{Path for output video file.}

\item{height_top}{Height in pixels for top video (default: auto from width).}

\item{height_bottom}{Height in pixels for bottom video (default: auto from width).}

\item{width}{Output width in pixels (default 1080).}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Stack two videos vertically (top over bottom) using FFmpeg's vstack filter.
Useful for YouTube Shorts layout (e.g. SadTalker top + slideshow bottom).
Both inputs are scaled to the target width. Heights can be specified
individually; by default each gets half the frame.
}
\examples{
\dontrun{
vstack("talking_head.mp4", "slideshow.mp4", "shorts.mp4")
vstack("top.mp4", "bottom.mp4", "out.mp4", height_top = 960, height_bottom = 960)
}

}
