% tinyrox says don't edit this manually, but it can't stop you!
\name{.layout_filter}
\alias{.layout_filter}
\title{Build the full compose filter graph}
\usage{
.layout_filter(slots, fps, reference)
}
\arguments{
\item{slots}{A validated \code{.layout_slots()} result.}

\item{fps}{Reference frame rate.}

\item{reference}{Reference slot name (no freeze on its chain).}
}
\value{
The filter_complex string.
}
\description{
A black canvas source at the reference rate, one normalization chain per
slot (inputs in slot order), then overlays in paint order. Everything is
infinite-or-held; the caller cuts frame-exactly with -frames:v (the
crossfade_concat discipline -- no shortest= anywhere).
}
\keyword{internal}
