% tinyrox says don't edit this manually, but it can't stop you!
\name{.silence_clip}
\alias{.silence_clip}
\title{Render silence to stand in for a Gap on an audio track}
\usage{
.silence_clip(output, duration, sample_rate, channels)
}
\arguments{
\item{output}{Output path.}

\item{duration}{Seconds of silence.}

\item{sample_rate}{Hz, matched to the track's clips.}

\item{channels}{1 or 2, matched to the track's clips.}
}
\value{
\code{output}, invisibly.
}
\description{
The audio counterpart of \code{.gap_clip()}: a Gap is silent, and
silence has to exist as a file because \code{audio_concat()} inserts one
uniform gap between all inputs rather than a different one per position.
}
\keyword{internal}
