% tinyrox says don't edit this manually, but it can't stop you!
\name{mux_av}
\alias{mux_av}
\title{Combine rendered picture and sound without re-encoding}
\usage{
mux_av(video, audio, output, overwrite = FALSE, dry_run = FALSE)
}
\arguments{
\item{video}{File containing the video stream.}

\item{audio}{File containing the audio stream.}

\item{output}{Explicit destination MP4 filename.}

\item{overwrite}{Whether an existing output may be replaced.}

\item{dry_run}{Return the FFmpeg command without executing it.}
}
\value{
The output path, invisibly, or a command for dry runs.
}
\description{
Copies the first video stream from `video` and the first audio stream from
`audio`. Timestamps are retained; no synchronization offset is applied.
}
\examples{
\dontrun{
mux_av("picture.mp4", "sound.m4a", file.path(tempdir(), "movie.mp4"))
}
}
