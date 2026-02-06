% tinyrox says don't edit this manually, but it can't stop you!
\name{.run_ffmpeg}
\alias{.run_ffmpeg}
\title{Run FFmpeg Command}
\usage{
.run_ffmpeg(args, dry_run = FALSE)
}
\arguments{
\item{args}{Character vector of ffmpeg arguments.}

\item{dry_run}{If TRUE, return the command string instead of running it.}
}
\value{
On success, the processx result (invisibly). On dry_run, the command string.
}
\description{
Internal helper that executes ffmpeg with the given arguments.
Surfaces stderr on failure.
}
\keyword{internal}
