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
Note: concatenation of multiple video clips, still/sequence
pre-renders, and layout composition still run, since they produce
intermediates the final command depends on.}
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
The video track is lowered honoring per-clip \code{source_range} trims and
\code{Transition}s: a transition between two clips becomes a dissolve over
exactly its duration, fed by the incoming clip's head handle (the media
before its source range -- for chained generation, the conditioning-head
replay). Only transitions with \code{out_offset == 0} are lowered (the
cornductor bundle shape: the outgoing clip has no tail handle). Gaps and
overlapping layers are not lowered yet.

Still-image clips (a media reference targeting a \code{.png}/\code{.jpg}/
...) are pre-rendered into video via \code{\link{still_clip}} at the
source_range's rate, honoring \code{cornball.*} motion effects on the clip
(Ken Burns pan/zoom; the schema lives in
\code{system.file("schema", "cornball-effects.md", package = "compost")}).
A still-image clip must carry a source_range. \code{ImageSequenceReference}
clips are pre-rendered via \code{\link{frames_clip}} at the reference's
rate; motion effects are not applied to sequences. Effects the renderer
cannot honor degrade to a static clip with a warning; effects outside the
\code{cornball.} namespace are ignored silently.

When the timeline carries \code{metadata$cornball$layout} (schema in
\code{system.file("schema", "cornball-layout.md", package = "compost")}),
tracks whose \code{cornball$role} matches a layout slot are each assembled
like the content track and composed onto the canvas via
\code{\link{compose_layout}} -- muted, painted in slot order, with the
content track bound to the \code{visual} slot as the timing reference.
The framing transform is suppressed when composing (the layout defines
the canvas). A layout the renderer cannot honor -- schema too new, a
slot's track missing, or a roled track without a slot -- degrades to the
plain single-stream render with one warning.

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
