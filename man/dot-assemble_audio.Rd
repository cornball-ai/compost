% tinyrox says don't edit this manually, but it can't stop you!
\name{.assemble_audio}
\alias{.assemble_audio}
\title{Assemble one audio track into a single file}
\usage{
.assemble_audio(track, media_dir)
}
\arguments{
\item{track}{A rotio audio Track.}

\item{media_dir}{Base directory for relative urls, or NULL.}
}
\value{
list(file, temps, bed) -- \code{bed} marks the untouched
  single-clip case -- or NULL when the track has no clips.
}
\description{
A Track is a sequence whether it carries pictures or sound, so the same
walk serves: clips in order, honoring source ranges, with Gaps as
silence.
}
\details{
The single-clip case is deliberately passed through untouched. That is
the authored-bed shape (one narration file over the whole timeline), it
is what the "narration is ground truth" rule downstream is written for,
and re-encoding it would change the very thing being treated as
authoritative.

}
\keyword{internal}
