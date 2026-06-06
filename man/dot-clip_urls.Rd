% tinyrox says don't edit this manually, but it can't stop you!
\name{.clip_urls}
\alias{.clip_urls}
\title{Ordered media urls of the Clips on a track}
\usage{
.clip_urls(track, media_dir = NULL)
}
\arguments{
\item{track}{A rotio Track.}

\item{media_dir}{Base directory for relative urls, or NULL.}
}
\value{
Character vector of resolved media paths, in track order.
}
\description{
Gaps and non-clip children are skipped.
}
\keyword{internal}
