% tinyrox says don't edit this manually, but it can't stop you!
\name{.rms_filter}
\alias{.rms_filter}
\title{ffmpeg filter chain for a per-window RMS readout}
\usage{
.rms_filter(window, mfile)
}
\description{
Slices the audio into \code{window}-sample blocks and prints each block's
overall RMS level (dB) as ametadata to \code{mfile}.
}
\keyword{internal}
