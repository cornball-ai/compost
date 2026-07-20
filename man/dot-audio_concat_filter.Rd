% tinyrox says don't edit this manually, but it can't stop you!
\name{.audio_concat_filter}
\alias{.audio_concat_filter}
\title{Build the filter graph for audio_concat}
\usage{
.audio_concat_filter(n, gap, tail, sample_rate, channels)
}
\arguments{
\item{n}{Number of inputs.}

\item{gap}{Silence between consecutive inputs, seconds.}

\item{tail}{Silence after the last input, seconds.}

\item{sample_rate}{Common sample rate in Hz.}

\item{channels}{Common channel count (1 or 2).}
}
\value{
The filter_complex string.
}
\description{
Pure string builder so the graph can be tested without ffmpeg. Each input
is normalized (aresample + aformat) to the common rate and layout; gaps and
the tail are anullsrc sources trimmed to length; everything runs through
one concat filter to \code{[aout]}.
}
\keyword{internal}
