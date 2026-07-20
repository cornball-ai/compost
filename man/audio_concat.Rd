% tinyrox says don't edit this manually, but it can't stop you!
\name{audio_concat}
\alias{audio_concat}
\title{Concatenate Audio Files with Exact Silence Gaps}
\usage{
audio_concat(inputs, output, gap = 0, tail = 0, sample_rate = NULL,
             channels = NULL, bitrate = NULL, overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{inputs}{Character vector of audio file paths, in order.}

\item{output}{Path for the output audio file; extension selects the format.}

\item{gap}{Silence in seconds between consecutive inputs (default 0).}

\item{tail}{Silence in seconds appended after the last input (default 0).}

\item{sample_rate}{Output sample rate in Hz, or NULL (default) to use the
first input's rate.}

\item{channels}{Output channel count (1 = mono, 2 = stereo), or NULL
(default) to use the first input's count.}

\item{bitrate}{Output audio bitrate (e.g. "192k"), or NULL for the encoder
default. Ignored by lossless formats such as WAV.}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns the command
  string.
}
\description{
Joins \code{inputs} in order into one audio file, inserting \code{gap}
seconds of silence between consecutive inputs and \code{tail} seconds after
the last. Every input is resampled and remixed to a common format first, so
inputs need not match. One encode, chosen by the output extension.
}
\examples{
\dontrun{
audio_concat(c("slide01.wav", "slide02.wav"), "audio.mp3",
             gap = 0.35, tail = 0.5)
}

}
