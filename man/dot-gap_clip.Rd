% tinyrox says don't edit this manually, but it can't stop you!
\name{.gap_clip}
\alias{.gap_clip}
\title{Render blank video to stand in for a Gap}
\usage{
.gap_clip(output, duration, size, fps)
}
\arguments{
\item{output}{Output path.}

\item{duration}{Seconds of blank.}

\item{size}{Integer width/height.}

\item{fps}{Frame rate.}
}
\value{
\code{output}, invisibly.
}
\description{
Matched to the neighbouring clips' geometry and rate so the result
concatenates with them, and encoded exactly as \code{\link{still_clip}}
encodes a slide, for the same reason.
}
\details{
Black, not transparent: assembling one track flat has nothing to show
through. When track compositing lands, a gap on an upper track has to
become transparent instead, and this is the function that changes.

}
\keyword{internal}
