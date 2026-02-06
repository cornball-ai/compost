% tinyrox says don't edit this manually, but it can't stop you!
\name{concat}
\alias{concat}
\title{Concatenate Video Segments}
\usage{
concat(inputs, output, overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{inputs}{Character vector of paths to input video files.}

\item{output}{Path for output video file.}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Join multiple video files sequentially using FFmpeg's concat demuxer.
All inputs should have compatible codecs and dimensions.
}
\examples{
\dontrun{
concat(c("part1.mp4", "part2.mp4", "part3.mp4"), "full.mp4")
}

}
