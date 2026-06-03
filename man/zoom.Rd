% tinyrox says don't edit this manually, but it can't stop you!
\name{zoom}
\alias{zoom}
\title{Zoom Video with Stepped Levels}
\usage{
zoom(input, output, stt, amount = 0.05, levels = 3, transition = 0.3,
     seed = NULL, width = NULL, height = NULL, overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{input}{Path to input video.}

\item{output}{Path for output video file.}

\item{stt}{A whisper_transcription object or data frame with \code{from} and
\code{to} timestamp columns (HH:MM:SS.mmm format).}

\item{amount}{Zoom per level (default 0.05). Level 1 = 1+amount, level 2 = 1+2*amount.}

\item{levels}{Number of zoom levels (default 3: none, single, double).}

\item{transition}{Transition duration in seconds between zoom levels (default 0.3).}

\item{seed}{Random seed for reproducible zoom patterns.}

\item{width}{Output width in pixels. If NULL, detected from input.}

\item{height}{Output height in pixels. If NULL, detected from input.}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Apply a stepped zoom effect to a video, changing zoom level at STT line
boundaries. Uses 3 levels (no zoom, single, double) with smooth transitions
between them. The pattern randomly alternates between zoom-in, hold, and
zoom-out phases.
}
\examples{
\dontrun{
stt <- readRDS("srt_text.RDS")
zoom("talking_head.mp4", "zoomed.mp4", stt = stt)
zoom("head.mp4", "zoomed.mp4", stt = stt, amount = 0.05, seed = 42)
}

}
