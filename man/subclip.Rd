% tinyrox says don't edit this manually, but it can't stop you!
\name{subclip}
\alias{subclip}
\title{Extract a Time Range from a Media File}
\usage{
subclip(input, output, start = 0, duration = NULL, end = NULL, reencode = TRUE,
        overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{input}{Path to input media.}

\item{output}{Path for the extracted output; extension selects the format.}

\item{start}{Start time in seconds (default 0).}

\item{duration}{Length in seconds, or NULL to use \code{end} or run to the
end of the file.}

\item{end}{End time in seconds (ignored when \code{duration} is set).}

\item{reencode}{If TRUE (default), re-encode for accurate cuts; if FALSE,
stream-copy (fast, keyframe-aligned).}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the ffmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns the command
  string.
}
\description{
Cut \code{[start, start + duration]} (or \code{[start, end]}) out of an audio
or video file via ffmpeg. Re-encodes by default for frame-accurate cuts; set
\code{reencode = FALSE} for a fast stream copy (cuts land on keyframes).
}
\examples{
\dontrun{
subclip("audio.mp3", "chunk.mp3", start = 2.14, duration = 5.0)
subclip("clip.mp4", "head.mp4", start = 0, end = 3, reencode = FALSE)
}

}
