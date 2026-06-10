% tinyrox says don't edit this manually, but it can't stop you!
\name{crossfade_concat}
\alias{crossfade_concat}
\title{Crossfade-concatenate clips, optionally overlaying one audio track}
\usage{
crossfade_concat(videos, output, fade = 0.375, audio = NULL,
                 transition = "dissolve", cuts = NULL, overwrite = TRUE,
                 dry_run = FALSE)
}
\arguments{
\item{videos}{Character vector of video paths, in order. Must share
resolution / fps / pixel format (xfade requires it).}

\item{output}{Output video path.}

\item{fade}{Crossfade duration in seconds (default 0.375 = 9 frames @ 24fps,
the default conditioning overlap).}

\item{audio}{Optional path to a continuous audio track to overlay as the sole
audio (e.g. the track's full narration mp3).}

\item{transition}{xfade transition name (default \code{"dissolve"}).}

\item{cuts}{Optional logical vector, one per join (length \code{length(videos)
- 1}). \code{TRUE} makes that join a hard cut (trim the next clip's head and
butt-join) instead of a crossfade. \code{NULL} (default) = all crossfades.
Output length is the same either way.}

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
Each crossfade consumes \code{fade} seconds of overlap, so the result runs
\code{sum(durations) - (n-1)*fade} long -- slightly shorter than a hard
concat. One ffmpeg pass via xfade.

}
\examples{
\dontrun{
crossfade_concat(c("chunk01.mp4", "chunk02_chained.mp4", "chunk03_chained.mp4"),
                 "video.mp4", fade = 0.375, audio = "audio.mp3")
}
}
