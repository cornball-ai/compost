% tinyrox says don't edit this manually, but it can't stop you!
\name{frame_export}
\alias{frame_export}
\title{Export a Single Frame at a Given Time}
\usage{
frame_export(input, time, output, overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{input}{Path to input video.}

\item{time}{Time in seconds to grab the frame at.}

\item{output}{Path for output image (e.g. .png, .jpg).}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Pull one frame from a video at \code{time} seconds and write it as an image.
A call-by-call primitive: no timeline, no asset lookup. The caller (an editor
such as kerNLE) decides which file and which time; this just extracts the
frame. Seeking is placed before \code{-i} for a fast keyframe seek.
}
\examples{
\dontrun{
frame_export("scene.mp4", 4.0, "frame.png")
}

}
