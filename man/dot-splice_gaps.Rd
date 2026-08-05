% tinyrox says don't edit this manually, but it can't stop you!
\name{.splice_gaps}
\alias{.splice_gaps}
\title{Splice blank video in where the track carries Gaps}
\usage{
.splice_gaps(vclips, seq_v)
}
\arguments{
\item{vclips}{Character vector of assembled clip files, in order.}

\item{seq_v}{The \code{.video_sequence()} result.}
}
\value{
list(files, windows, fades, temps) with filler interleaved, or
  the inputs unchanged when the track has no gaps.
}
\description{
Splice blank video in where the track carries Gaps
}
\keyword{internal}
