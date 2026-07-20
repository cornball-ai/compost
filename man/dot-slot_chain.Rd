% tinyrox says don't edit this manually, but it can't stop you!
\name{.slot_chain}
\alias{.slot_chain}
\title{One slot's normalization chain}
\usage{
.slot_chain(i, rect, fit, fps, freeze, lbl)
}
\arguments{
\item{i}{Zero-based input index.}

\item{rect}{Integer c(x, y, w, h).}

\item{fit}{"fill" or "fit".}

\item{fps}{Reference frame rate.}

\item{freeze}{Append the freeze (tpad clone) -- FALSE for the reference.}

\item{lbl}{Output pad label.}
}
\value{
One filter chain string.
}
\description{
fill covers the rect (scale up + center-crop); fit contains it (scale
down + centered black pad). Both normalize fps, SAR, pixel format, and
timebase so heterogeneous sources (phone footage, 10-bit, 24/25 fps)
overlay cleanly; non-reference chains freeze their last frame so a short
take never goes black.
}
\keyword{internal}
