% tinyrox says don't edit this manually, but it can't stop you!
\name{.kenburns_filter}
\alias{.kenburns_filter}
\title{Build the zoompan filter for a still clip}
\usage{
.kenburns_filter(motion, n_frames, out_w, out_h, fps)
}
\arguments{
\item{motion}{Ken Burns spec list (see \code{\link{still_clip}}), or NULL
for a static clip.}

\item{n_frames}{Total output frames.}

\item{out_w}{Output width in pixels.}

\item{out_h}{Output height in pixels.}

\item{fps}{Output frame rate.}
}
\value{
The zoompan filter string.
}
\description{
Pure string builder so the motion math can be tested without ffmpeg. The
zoom and anchor interpolate from their \code{*_from}/\code{from} values at
the first frame to \code{*_to}/\code{to} at the last, along the easing
curve. The crop window is centered on the anchor and clamped to the frame,
all in ffmpeg expressions of \code{iw}/\code{ih}, so no source dimensions
are needed here.
}
\keyword{internal}
