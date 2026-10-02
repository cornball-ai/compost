% tinyrox says don't edit this manually, but it can't stop you!
\name{.audio_window}
\alias{.audio_window}
\title{Cut a window of audio at exactly its length}
\usage{
.audio_window(input, output, start, duration)
}
\arguments{
\item{input}{Path to the source media.}

\item{output}{Path for the cut; the extension selects the format.}

\item{start}{Seconds into the source.}

\item{duration}{Seconds to keep.}
}
\value{
\code{output}, invisibly.
}
\description{
A window can ask for more than its file holds. An mp3's container
duration counts encoder padding the decoder drops, so a range written
from a probe of the file runs past its last sample, and a plain cut
comes out short by that much. On a bed that made the render shorter
than the clip the timeline declared; on an assembled track it would
pull every later clip earlier. Silence fills what the file does not
hold, so a cut is as long as its window.
}
\keyword{internal}
