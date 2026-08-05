% tinyrox says don't edit this manually, but it can't stop you!
\name{.transform_chain}
\alias{.transform_chain}
\title{Filter chain placing one layer on the canvas under its transform}
\usage{
.transform_chain(tf, bw, bh)
}
\arguments{
\item{tf}{A \code{.clip_transform()} result, or NULL.}

\item{bw,bh}{Canvas size.}
}
\value{
Character vector of filters, in order.
}
\description{
Scale first, then rotate, then opacity, because each consumes the
previous one's frame. Rotation and opacity force RGBA: a rotated layer
has empty corners and a translucent one has to blend, and without an
alpha channel both would come out as black painted over the base.
}
\keyword{internal}
