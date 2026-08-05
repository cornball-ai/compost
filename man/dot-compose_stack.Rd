% tinyrox says don't edit this manually, but it can't stop you!
\name{.compose_stack}
\alias{.compose_stack}
\title{Paint upper tracks onto a base, each clip over its own time range}
\usage{
.compose_stack(base, layers, output, duration = NULL, dry_run = FALSE)
}
\arguments{
\item{base}{Assembled bottom track (the canvas).}

\item{layers}{data.frame of placements, bottom-to-top paint order.}

\item{output}{Output path.}

\item{dry_run}{If TRUE, return the command instead of running it.}
}
\value{
\code{output}, invisibly (or the command string).
}
\description{
The Stack's composition rule: later tracks paint over earlier ones, and
a clip is visible exactly during its range. Placement is temporal, so
no transparency is needed anywhere -- an upper track's Gap is simply a
span with no overlay enabled over it, which is also why gap filler is
only ever generated for the base.
}
\keyword{internal}
