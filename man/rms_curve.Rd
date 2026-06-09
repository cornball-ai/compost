% tinyrox says don't edit this manually, but it can't stop you!
\name{rms_curve}
\alias{rms_curve}
\title{Per-window RMS curve of an audio file}
\usage{
rms_curve(file, window = 1024L)
}
\arguments{
\item{file}{Path to an audio (or audio-bearing video) file.}

\item{window}{Window size in samples (default 1024; ~64ms at 16kHz). Smaller
windows give finer time resolution at more rows.}
}
\value{
A list with numeric \code{time} (window centre, seconds) and
  \code{rms} (RMS level in dB; digital silence is -120). Empty vectors if the
  pass produced nothing.
}
\description{
Runs one ffmpeg \code{astats} pass over \code{file} and returns the RMS level
of each fixed-size window. Quieter windows (more-negative dB) mark pauses, so
the curve is a cheap way to find good places to cut. About 140x realtime.
}
\examples{
\dontrun{
cur <- rms_curve("speech.mp3")
plot(cur$time, cur$rms, type = "l")   # dips are pauses
}
}
