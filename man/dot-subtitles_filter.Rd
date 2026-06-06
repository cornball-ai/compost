% tinyrox says don't edit this manually, but it can't stop you!
\name{.subtitles_filter}
\alias{.subtitles_filter}
\title{Build the subtitles burn filter for a subtitle file}
\usage{
.subtitles_filter(sub_file)
}
\arguments{
\item{sub_file}{Path to an .ass/.srt/.vtt file.}
}
\value{
A single filter expression.
}
\description{
Escapes the path the way ffmpeg's subtitles filter requires.
}
\keyword{internal}
