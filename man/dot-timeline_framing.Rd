% tinyrox says don't edit this manually, but it can't stop you!
\name{.timeline_framing}
\alias{.timeline_framing}
\title{Framing transform for the timeline}
\usage{
.timeline_framing(vtrack, timeline)
}
\arguments{
\item{vtrack}{The primary video Track.}

\item{timeline}{The Timeline.}
}
\value{
A framing list, or NULL.
}
\description{
Looks for \code{metadata$cornball$framing} on the first video clip's media
reference, then the clip, then the timeline. A framing is a list with optional
\code{scale} (a box edge, or c(w, h)), \code{pad} (c(w, h)), and \code{pos}
(c(x, y), defaulting to c(0, 0)). Any element may be a string for ffmpeg
expressions (e.g. pos = c("(ow-iw)/2", "(oh-ih)/2")).
}
\keyword{internal}
