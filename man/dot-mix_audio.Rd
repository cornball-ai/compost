% tinyrox says don't edit this manually, but it can't stop you!
\name{.mix_audio}
\alias{.mix_audio}
\title{Mix several assembled audio tracks into one}
\usage{
.mix_audio(files, output, dry_run = FALSE)
}
\arguments{
\item{files}{Assembled per-track audio, in track order.}

\item{output}{Output path.}

\item{dry_run}{If TRUE, return the command instead of running it.}
}
\value{
\code{output}, invisibly (or the command string).
}
\description{
\code{normalize=0} so the tracks are summed rather than averaged. amix
divides by the input count by default, which would halve the narration
the moment a music track appeared beside it -- not what an editor means
by adding a second track. \code{duration=longest} because a shorter
track ends, it does not end the mix.
}
\keyword{internal}
