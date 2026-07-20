% tinyrox says don't edit this manually, but it can't stop you!
\name{.video_sequence}
\alias{.video_sequence}
\title{Walk a video track into renderable clip feeds and join fades}
\usage{
.video_sequence(track, media_dir = NULL)
}
\arguments{
\item{track}{A rotio video Track.}

\item{media_dir}{Base directory for relative urls, or NULL.}
}
\value{
list(files, windows, fades, clips). A Clip over an
  ImageSequenceReference contributes an NA file, filled in by
  \code{.prerender_sources()}.
}
\description{
Returns the clips' resolved files, per-clip source windows (the media to
feed in: the source range, widened at the front by a preceding Transition's
in_offset so the dissolve has its handle), and per-join fade durations
(0 = butt join). A clip without a source_range feeds the whole file.
}
\keyword{internal}
