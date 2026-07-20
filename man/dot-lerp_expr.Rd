% tinyrox says don't edit this manually, but it can't stop you!
\name{.lerp_expr}
\alias{.lerp_expr}
\title{Interpolation expression between two values along an easing curve}
\usage{
.lerp_expr(a, b, e)
}
\arguments{
\item{a}{Start value.}

\item{b}{End value.}

\item{e}{Easing expression string.}
}
\value{
A constant when a == b, else \code{a+(b-a)*e}.
}
\description{
Interpolation expression between two values along an easing curve
}
\keyword{internal}
