% tinyrox says don't edit this manually, but it can't stop you!
\name{render_timeline}
\alias{render_timeline}
\title{Render an OTIO Timeline to a Video File}
\usage{
render_timeline(timeline, output, media_dir = NULL, overwrite = TRUE,
                dry_run = FALSE)
}
\arguments{
\item{timeline}{A rotio Timeline, or a path to a \code{.otio} file.}

\item{output}{Path for the output video file.}

\item{media_dir}{Base directory for resolving relative media urls. Defaults to
the timeline file's directory when \code{timeline} is a path, otherwise NULL
(urls used as-is).}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the ffmpeg command string without executing.
Note: concatenation of multiple video clips still runs, since it produces an
intermediate the final command depends on.}
}
\value{
Invisibly returns the output path. If dry_run, returns the command
  string for the final render pass.
}
\description{
Lowers a rotio (OpenTimelineIO) Timeline to a single ffmpeg invocation and
renders it. The video track's clips are concatenated in order; an optional
separate audio track is mapped over them; a framing transform (scale + pad,
from \code{metadata$cornball$framing}) and an optional caption burn (from a
caption track referencing an .ass/.srt file) are applied.
}
\details{
Caption tracks are identified by \code{metadata$cornball$role == "caption"} or
a name containing "caption". When the primary video track has a single clip
that already carries its own audio (the talking-head case), no separate audio
track is needed.

}
\examples{
\dontrun{
render_timeline("AAA/20260131/t41_n22_intro/timeline.otio",
                "AAA/20260131/t41_n22_intro/video.mp4")
}

}
