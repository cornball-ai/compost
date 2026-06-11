% tinyrox says don't edit this manually, but it can't stop you!
\name{align_overlaps}
\alias{align_overlaps}
\title{Align a chunk sequence to its narration and derive per-join overlaps}
\usage{
align_overlaps(files, narration, fps = 24)
}
\arguments{
\item{files}{Character vector of chunk media paths, in order.}

\item{narration}{The continuous narration audio.}

\item{fps}{Frame rate for the \code{overlap} frame counts (default 24).}
}
\value{
A data frame with one row per chunk: \code{chunk}, \code{file},
  \code{frames}, \code{fps}, \code{offset} (narration seconds),
  \code{correlation}, and \code{overlap} (frames; 0 for the first chunk).
}
\description{
Places each chunk on the narration clock via [align_audio], then derives
each join's overlap from the positions: the seconds by which chunk
\code{j}'s video runs past where chunk \code{j+1}'s content begins
(clamped at 0 = butt join). This is layout truth by construction -- a
chained chunk's duplicated head and an undersized chunk's shortfall both
fall out of where the words actually are.
}
