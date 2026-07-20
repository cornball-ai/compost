% tinyrox says don't edit this manually, but it can't stop you!
\name{.motion_num}
\alias{.motion_num}
\title{Coerce one motion field to numeric with a default}
\usage{
.motion_num(v, default)
}
\arguments{
\item{v}{Field value.}

\item{default}{Value when v is NULL.}
}
\value{
A numeric scalar.
}
\description{
JSON-deserialized specs may carry integers or single-element lists where
doubles were written; unify here.
}
\keyword{internal}
