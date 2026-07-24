% tinyrox says don't edit this manually, but it can't stop you!
\name{.rms_filter}
\alias{.rms_filter}
\title{ffmpeg filter chain for a per-window RMS readout}
\usage{
.rms_filter(window)
}
\arguments{
\item{window}{Window size in samples.}
}
\value{
A single ffmpeg filter-chain string.
}
\description{
Slices the audio into \code{window}-sample blocks and prints each block's
overall RMS level (dB) as ametadata. No \code{file=} option: the print
stream goes to ffmpeg's stderr, which the caller captures and parses. Writing
to a \code{file=} path breaks on Windows, where a drive-letter path
(\code{C:\\...}) is truncated at the colon by ffmpeg's filter-option parser
and the metadata lands in a stray file named after the drive; reading stderr
sidesteps path escaping entirely and behaves identically on every platform.
}
\keyword{internal}
