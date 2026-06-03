% tinyrox says don't edit this manually, but it can't stop you!
\name{.broadcast_audio_filter}
\alias{.broadcast_audio_filter}
\title{Build the -af filter graph for the broadcast-clean chain}
\usage{
.broadcast_audio_filter(dehum = TRUE, target_lufs = -14, fade = 0.06,
                        duration = NULL, highpass = 80)
}
\arguments{
\item{dehum}{Include the hum notches + FFT denoise.}

\item{target_lufs}{Loudness target in LUFS.}

\item{fade}{In/out fade length in seconds; 0 disables fades.}

\item{duration}{Clip length in seconds, for the out-fade start. NA/NULL skips
the out-fade.}

\item{highpass}{High-pass corner frequency in Hz.}
}
\value{
The comma-joined filter graph string.
}
\description{
Pure string builder so the chain can be tested without ffmpeg.
}
\keyword{internal}
