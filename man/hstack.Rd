% tinyrox says don't edit this manually, but it can't stop you!
\name{hstack}
\alias{hstack}
\title{Horizontally Stack Two Videos}
\usage{
hstack(
  left,
  right,
  output,
  width_left = NULL,
  width_right = NULL,
  height = 1080,
  overwrite = TRUE,
  dry_run = FALSE
)
}
\arguments{
\item{left}{Path to left video.}

\item{right}{Path to right video.}

\item{output}{Path for output video file.}

\item{width_left}{Width in pixels for left video (default: auto from height).}

\item{width_right}{Width in pixels for right video (default: auto from height).}

\item{height}{Output height in pixels (default 1080).}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Stack two videos side by side using FFmpeg's hstack filter.
}
\examples{
\dontrun{
hstack("left.mp4", "right.mp4", "wide.mp4")
}

}
