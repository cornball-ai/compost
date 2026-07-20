% tinyrox says don't edit this manually, but it can't stop you!
\name{.assemble_track}
\alias{.assemble_track}
\title{Assemble one video track into a single renderable file}
\usage{
.assemble_track(track, media_dir, framing)
}
\arguments{
\item{track}{A rotio video Track.}

\item{media_dir}{Base directory for relative urls, or NULL.}

\item{framing}{Framing for still rastering (NULL for slot tracks).}
}
\value{
list(file, temps), or NULL when the track has no clips.
}
\description{
The track walk + still/sequence pre-render + passthrough/concat/crossfade
pipeline, shared by the content track and layout slot tracks.
}
\keyword{internal}
