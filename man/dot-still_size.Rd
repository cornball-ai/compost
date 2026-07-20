% tinyrox says don't edit this manually, but it can't stop you!
\name{.still_size}
\alias{.still_size}
\title{Fit source dimensions into a framing box}
\usage{
.still_size(img_w, img_h, framing = NULL)
}
\arguments{
\item{img_w}{Source width in pixels.}

\item{img_h}{Source height in pixels.}

\item{framing}{A framing list with a \code{pad = c(w, h)} element, or NULL.}
}
\value{
\code{c(width, height)}, both even.
}
\description{
Aspect-fits \code{c(img_w, img_h)} into the framing's \code{pad} box (either
direction -- upscale or downscale), rounded to the nearest even pixel as
libx264 requires. With no framing, just even-rounds the source dimensions.
This is what keeps zoompan from stretching a slide: the clip is rendered at
the source's own aspect and letterboxed later.
}
\keyword{internal}
