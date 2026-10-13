% tinyrox says don't edit this manually, but it can't stop you!
\name{.audio_demuxer}
\alias{.audio_demuxer}
\title{Force the right demuxer for an audio input}
\usage{
.audio_demuxer(path)
}
\arguments{
\item{path}{Path to an audio input file.}
}
\value{
A character vector of ffmpeg args (possibly empty) to precede
  the \code{-i} for this input.
}
\description{
ffmpeg's container auto-detection can misclassify a short re-encoded mp3.
Its raw bitstream sometimes scores higher as an H.266/VVC elementary stream
than as mp3, so the decode fails with "Invalid NAL unit" and status 234
instead of reading the audio. Naming the demuxer with \code{-f} before
\code{-i} skips the guess. Return the args to place immediately before the
\code{-i} for a given input; empty for formats that are not misdetected.
}
\keyword{internal}
