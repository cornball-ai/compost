% tinyrox says don't edit this manually, but it can't stop you!
\name{.pip_filter}
\alias{.pip_filter}
\title{Build the PiP corner overlay filter_complex}
\usage{
.pip_filter(scale = 0.604, margin = 230, corner = "upper-right")
}
\arguments{
\item{scale}{Foreground scale factor.}

\item{margin}{Pixel margin from the corner.}

\item{corner}{Corner name (matched on "right"/"lower").}
}
\value{
The filter_complex string.
}
\description{
Build the PiP corner overlay filter_complex
}
\keyword{internal}
