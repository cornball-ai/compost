% tinyrox says don't edit this manually, but it can't stop you!
\name{frames_clip}
\alias{frames_clip}
\title{Encode a Numbered Frame Sequence as a Video Clip}
\usage{
frames_clip(dir, output, fps = 30, pattern = "frame_\%04d.png", start = 1L,
            size = NULL, overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{dir}{Directory holding the frames.}

\item{output}{Path for the output video file.}

\item{fps}{Frame rate the sequence was drawn at (default 30).}

\item{pattern}{C-style filename pattern of the frames (default
\code{"frame_\\%04d.png"}).}

\item{start}{Number of the first frame (default 1).}

\item{size}{Output dimensions as \code{c(width, height)}, or NULL (default)
to keep the source dimensions. Scaling ignores aspect; pass a size that
preserves it (see \code{\link{pad}} for letterboxing instead).}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns the command
  string.
}
\description{
Encodes \code{dir/pattern} (a C-style numbered pattern such as
\code{"frame_\\%04d.png"}) into a video at \code{fps}. The sequence's frame
count sets the clip length. Encode settings match \code{\link{still_clip}},
so stills and frame sequences concatenate cleanly.
}
\examples{
\dontrun{
frames_clip("scene01/", "scene01.mp4", fps = 30)
}

}
