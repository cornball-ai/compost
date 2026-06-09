% tinyrox says don't edit this manually, but it can't stop you!
\name{.parse_rms}
\alias{.parse_rms}
\title{Parse ffmpeg ametadata output into time/rms vectors}
\usage{
.parse_rms(lines)
}
\description{
The print stream alternates a \code{pts_time:<t>} line and an
\code{RMS_level=<db>} line per window. Digital silence prints \code{-inf},
mapped to -120.
}
\keyword{internal}
