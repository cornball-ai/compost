% tinyrox says don't edit this manually, but it can't stop you!
\name{.prerender_sources}
\alias{.prerender_sources}
\title{Pre-render still-image and image-sequence clips into video}
\usage{
.prerender_sources(seq_v, framing, media_dir = NULL)
}
\arguments{
\item{seq_v}{A \code{.video_sequence()} result.}

\item{framing}{The timeline framing list, for still rastering.}

\item{media_dir}{Base directory for relative urls, or NULL.}
}
\value{
list(files, temps): files with pre-rendered replacements, and the
  temp paths for the caller to clean up.
}
\description{
A still image has no intrinsic duration or motion, and an
ImageSequenceReference has no single file: both are encoded into temporary
video clips here, so everything downstream (concat, crossfades, framing,
captions, audio) handles only video. Stills go through
\code{\link{still_clip}} at the source_range's rate, covering the media
window up to the range's end (so a preceding dissolve keeps its head
handle), rastered to the source aspect fitted inside the framing box, and
animated by the clip's \code{cornball.*} motion effects. Sequences go
through \code{\link{frames_clip}} at the reference's own rate; motion
effects are not applied to them (a drawn sequence animates itself). Video
files pass through untouched.
}
\keyword{internal}
