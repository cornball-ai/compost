% tinyrox says don't edit this manually, but it can't stop you!
\name{.track_layers}
\alias{.track_layers}
\title{Walk, pre-render and place one track's clips for compositing}
\usage{
.track_layers(track, media_dir)
}
\arguments{
\item{track}{A rotio video Track.}

\item{media_dir}{Base directory for relative urls, or NULL.}
}
\value{
list(layers, temps, span); \code{layers} is NULL when the track
  has no clips.
}
\description{
The upper-track counterpart to \code{.assemble_track()}: the clips stay
separate rather than being concatenated, because each is painted over
its own span.
}
\keyword{internal}
