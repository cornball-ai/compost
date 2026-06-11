% tinyrox says don't edit this manually, but it can't stop you!
\name{crossfade_concat}
\alias{crossfade_concat}
\title{Crossfade-concatenate clips, optionally overlaying one audio track}
\usage{
crossfade_concat(videos, output, fade = 0.375, audio = NULL,
                 transition = "dissolve", cuts = NULL, windows = NULL,
                 overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{videos}{Character vector of video paths, in order. Must share
resolution / fps / pixel format (xfade requires it).}

\item{output}{Output video path.}

\item{fade}{Crossfade duration in seconds: a scalar applied to every join,
or a vector with one duration per join (length \code{length(videos) - 1}).
A join with \code{fade} 0 is a plain butt join -- no overlap consumed, no
dissolve -- for boundaries where the next clip has no duplicated
conditioning head (see \code{\link{detect_overlap}}). Default 0.375
(= 9 frames @ 24fps, the default conditioning overlap).}

\item{audio}{Optional path to a continuous audio track to overlay as the sole
audio (e.g. the track's full narration mp3).}

\item{transition}{xfade transition name (default \code{"dissolve"}).}

\item{cuts}{Optional logical vector, one per join (length \code{length(videos)
- 1}). \code{TRUE} makes that join a hard cut (trim the next clip's head by
that join's \code{fade} and butt-join) instead of a crossfade. \code{NULL}
(default) = all crossfades. Output length is the same either way; a cut at
a zero-\code{fade} join is just a butt join.}

\item{windows}{Optional list, one element per video: \code{c(start, end)} in
seconds selecting the part of the file to feed in (\code{NA} end = to the
end of file), or \code{NULL} for the whole file. For a crossfaded join the
incoming window should \emph{include} the head handle the fade consumes
(the fade eats \code{fade} seconds off the window's front against the
previous clip's tail). Used by \code{render_timeline} to honor OTIO
source ranges.}

\item{overwrite}{Overwrite \code{output} (default TRUE).}

\item{dry_run}{If TRUE, return the ffmpeg command string without running.}
}
\value{
Invisibly, \code{output} (or the command string when dry_run).
}
\description{
Joins \code{videos} in order, dissolving each transition over \code{fade}
seconds (the duplicated conditioning overlap, so the blend is between
near-identical frames and is invisible). When \code{audio} is given it
becomes the sole audio track -- the right move for chained chunks, whose
per-chunk audio has a silent conditioning head; the continuous narration has
no such gaps. Without \code{audio}, the first clip's audio is mapped through.
}
\details{
Each join consumes its \code{fade} seconds of overlap, so the result runs
\code{sum(durations) - sum(fades)} long -- slightly shorter than a hard
concat. One ffmpeg pass via xfade.

}
\examples{
\dontrun{
crossfade_concat(c("chunk01.mp4", "chunk02_chained.mp4", "chunk03_chained.mp4"),
                 "video.mp4", fade = 0.375, audio = "audio.mp3")
}
}
