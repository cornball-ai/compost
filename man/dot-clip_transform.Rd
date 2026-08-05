% tinyrox says don't edit this manually, but it can't stop you!
\name{.clip_transform}
\alias{.clip_transform}
\title{A clip's compositing transform, or NULL when it is the identity}
\usage{
.clip_transform(clip)
}
\arguments{
\item{clip}{A rotio Clip.}
}
\value{
A named list of the six fields, or NULL.
}
\description{
Read from \code{metadata$cornball$transform}, falling back to
\code{metadata$cornball$nle$transform} where an editor keeps it with the
rest of its clip state. Fields: \code{pos_x}, \code{pos_y} (pixels from
centred), \code{scale_x}, \code{scale_y} (multipliers), \code{rotation}
(degrees clockwise), \code{opacity} (0-1).
}
\details{
NULL for the identity so callers can take the cheap path: the common
clip carries a transform that does nothing, and it should not cost a
filter chain to say so.

}
\keyword{internal}
