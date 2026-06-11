% tinyrox says don't edit this manually, but it can't stop you!
\name{align_audio}
\alias{align_audio}
\title{Locate a clip's audio within a longer narration}
\usage{
align_audio(clip, narration, sr = 8000L, hop = 80L)
}
\arguments{
\item{clip}{Media file whose audio to place.}

\item{narration}{The continuous reference audio (e.g. the track's
\code{audio.mp3}).}

\item{sr}{Decode sample rate (default 8000).}

\item{hop}{Envelope hop in samples (default 80 = 10ms at 8kHz).}
}
\value{
Offset in seconds (numeric), with attributes \code{correlation}
  (peak normalized correlation, 0-1ish) and \code{curve} (correlation per
  candidate lag).
}
\description{
Normalized cross-correlation of amplitude envelopes (10ms resolution by
default): returns the offset in seconds at which \code{clip}'s audio best
matches \code{narration}. Works on video files (their audio stream) and
plain audio files alike.
}
\examples{
\dontrun{
align_audio("media/chunk02.mp4", "audio.mp3")
}
}
