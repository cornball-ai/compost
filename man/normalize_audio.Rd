% tinyrox says don't edit this manually, but it can't stop you!
\name{normalize_audio}
\alias{normalize_audio}
\title{Normalize Audio Loudness}
\usage{
normalize_audio(input, output = input, integrated = -16, lra = 11, tp = -1.5,
                bitrate = "192k", overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{input}{Path to input audio (or A/V) file.}

\item{output}{Path for output file (default: overwrite input in place).}

\item{integrated}{Target integrated loudness in LUFS (default -16).}

\item{lra}{Loudness range target in LU (default 11).}

\item{tp}{True-peak ceiling in dBTP (default -1.5).}

\item{bitrate}{Output audio bitrate for lossy formats (default "192k"),
or NULL for the encoder default.}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns the
  command string.
}
\description{
A single loudnorm pass to an EBU R128 / ITU-R BS.1770 target -- the
light-touch cousin of \code{\link{broadcast_audio}}, which wraps loudnorm
in a full mastering chain (high-pass, de-hum notches, compression, fades).
Use this to level takes that are already clean, e.g. TTS output across a
batch of tracks.
}
\examples{
\dontrun{
normalize_audio("audio.mp3")
normalize_audio("take.wav", "leveled.wav", integrated = -14)
}

}
