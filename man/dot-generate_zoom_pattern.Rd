% tinyrox says don't edit this manually, but it can't stop you!
\name{.generate_zoom_pattern}
\alias{.generate_zoom_pattern}
\title{Generate Stepped Zoom Pattern}
\usage{
.generate_zoom_pattern(n, levels = 3, seed = NULL)
}
\arguments{
\item{n}{Number of segments (STT lines).}

\item{levels}{Number of zoom levels (default 3).}

\item{seed}{Optional random seed.}
}
\value{
Integer vector of zoom levels (0 to levels-1).
}
\description{
Creates a zoom level sequence that alternates between rest (level 0),
zoom-in ramps, holds at target, and zoom-out ramps.
}
\keyword{internal}
