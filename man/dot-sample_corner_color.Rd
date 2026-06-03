% tinyrox says don't edit this manually, but it can't stop you!
\name{.sample_corner_color}
\alias{.sample_corner_color}
\title{Sample one pixel's colour from the first frame as 0xRRGGBB}
\usage{
.sample_corner_color(file, x = 0, y = 0)
}
\arguments{
\item{file}{Path to media file (already validated by the caller).}

\item{x}{Pixel x coordinate to sample.}

\item{y}{Pixel y coordinate to sample.}
}
\value{
Colour string "0xRRGGBB".
}
\description{
Sample one pixel's colour from the first frame as 0xRRGGBB
}
\keyword{internal}
