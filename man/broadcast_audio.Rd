% tinyrox says don't edit this manually, but it can't stop you!
\name{broadcast_audio}
\alias{broadcast_audio}
\title{Broadcast-Clean an Audio File}
\usage{
broadcast_audio(input, output, dehum = TRUE, target_lufs = -14, fade = 0.06,
                highpass = 80, overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{input}{Path to input audio (or A/V) file.}

\item{output}{Path for output file.}

\item{dehum}{Apply the 60/120/180 Hz hum notches + FFT denoise (default TRUE).}

\item{target_lufs}{Integrated loudness target in LUFS (default -14).}

\item{fade}{In/out fade length in seconds (default 0.06).}

\item{highpass}{High-pass corner frequency in Hz (default 80).}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
The standard broadcast chain: a high-pass to kill rumble, optional 60/120/180
Hz hum notches plus FFT denoise, a 4:1 compressor, loudness normalisation to
\code{target_lufs}, and short in/out fades.
}
\examples{
\dontrun{
broadcast_audio("raw.wav", "clean.wav")
broadcast_audio("take.wav", "clean.wav", dehum = FALSE, target_lufs = -16)
}

}
