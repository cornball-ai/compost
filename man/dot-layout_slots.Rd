% tinyrox says don't edit this manually, but it can't stop you!
\name{.layout_slots}
\alias{.layout_slots}
\title{Validate and coerce a cornball layout}
\usage{
.layout_slots(layout)
}
\arguments{
\item{layout}{The layout list.}
}
\value{
list(canvas, slots) with integer geometry, or NULL.
}
\description{
JSON-deserialized layouts carry lists where integer vectors were written;
unify, then validate: even canvas, even rect sizes, rects in-bounds,
known fits. Returns NULL (with one warning) when the schema is newer than
supported -- render degradation; hard errors on malformed geometry --
writer bugs should be loud.
}
\keyword{internal}
