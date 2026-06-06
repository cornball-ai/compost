% tinyrox says don't edit this manually, but it can't stop you!
\name{.is_caption_track}
\alias{.is_caption_track}
\title{Is this track a caption track?}
\usage{
.is_caption_track(track)
}
\arguments{
\item{track}{A rotio Track.}
}
\value{
TRUE if the track holds captions.
}
\description{
A track is treated as captions if its cornball metadata sets
\code{role = "caption"} or its name contains "caption".
}
\keyword{internal}
