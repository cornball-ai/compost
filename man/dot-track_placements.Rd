% tinyrox says don't edit this manually, but it can't stop you!
\name{.track_placements}
\alias{.track_placements}
\title{Where each of a track's clips sits on the timeline}
\usage{
.track_placements(seq_v, files)
}
\arguments{
\item{seq_v}{A \code{.video_sequence()} result.}

\item{files}{The pre-rendered files, parallel to \code{seq_v$files}.}
}
\value{
data.frame of \code{file}, \code{start}, \code{dur}, and the
  source in-point \code{ss} (NA for no trim), one row per clip.
}
\description{
A Track is a sequence: a child's start is the sum of what precedes it,
Gaps included. This turns the walk plus the pre-rendered files into
absolute placements, resolving any duration the walk could not know.
}
\keyword{internal}
