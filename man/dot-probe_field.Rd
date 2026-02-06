% tinyrox says don't edit this manually, but it can't stop you!
\name{.probe_field}
\alias{.probe_field}
\title{Query a Single Field via ffprobe}
\usage{
.probe_field(file, field, stream = "v:0")
}
\arguments{
\item{file}{Path to media file.}

\item{field}{The ffprobe field to query (e.g. "duration", "width", "height").}

\item{stream}{Stream specifier: "v:0" for video, "a:0" for audio.}
}
\value{
The field value as a character string.
}
\description{
Query a Single Field via ffprobe
}
\keyword{internal}
