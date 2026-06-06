% tinyrox says don't edit this manually, but it can't stop you!
\name{.probe_format_field}
\alias{.probe_format_field}
\title{Query a Container-Level Field via ffprobe}
\usage{
.probe_format_field(file, field)
}
\arguments{
\item{file}{Path to media file.}

\item{field}{The ffprobe format field (e.g. "duration").}
}
\value{
The field value as a character string.
}
\description{
Reads a format (container) field rather than a stream field, so it works for
audio-only and video files alike. Used for duration, which is not always
present on a given stream.
}
\keyword{internal}
