% tinyrox says don't edit this manually, but it can't stop you!
\name{audio_convert}
\alias{audio_convert}
\title{Convert an Audio File}
\usage{
audio_convert(input, output, sample_rate = NULL, channels = NULL,
              bitrate = NULL, codec = NULL, overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{input}{Path to input audio (or A/V) file.}

\item{output}{Path for output file; extension selects the format.}

\item{sample_rate}{Output sample rate in Hz, or NULL to keep the source rate.}

\item{channels}{Output channel count (1 = mono, 2 = stereo), or NULL to keep.}

\item{bitrate}{Output audio bitrate (e.g. "192k"), or NULL for the encoder
default. Ignored by lossless formats such as WAV.}

\item{codec}{Explicit audio codec (e.g. "libmp3lame", "pcm_s16le"), or NULL to
let ffmpeg pick from the output extension.}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the ffmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns the command
  string.
}
\description{
Re-encode an audio (or A/V) file's audio to a new container, sample rate,
channel count, or codec via ffmpeg. The output format is inferred from the
output extension. With no optional arguments it is a straight transcode.
}
\details{
Resampling to 16 kHz mono is the usual preparation for speech-to-text:
\code{audio_convert("audio.mp3", "audio.wav", sample_rate = 16000)}.

}
\examples{
\dontrun{
audio_convert("audio.mp3", "speech.wav", sample_rate = 16000)
audio_convert("take.wav", "take.mp3", bitrate = "192k")
}

}
