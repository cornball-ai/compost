% tinyrox says don't edit this manually, but it can't stop you!
\name{.ease_expr}
\alias{.ease_expr}
\title{Easing expression over the frame counter}
\usage{
.ease_expr(ease, n1)
}
\arguments{
\item{ease}{"linear" or "smooth" (smoothstep).}

\item{n1}{Index of the last frame (frames - 1, at least 1).}
}
\value{
The easing expression string in terms of \code{on}.
}
\description{
Progress p runs 0 to 1 over frames 0..n1, clamped at 1 so a trailing frame
from rounding can never overshoot the end state.
}
\keyword{internal}
