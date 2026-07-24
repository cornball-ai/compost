% tinyrox says don't edit this manually, but it can't stop you!
\name{.run_ffprobe}
\alias{.run_ffprobe}
\title{Run ffprobe Command}
\usage{
.run_ffprobe(args)
}
\arguments{
\item{args}{Character vector of ffprobe arguments.}
}
\value{
Character vector of ffprobe's stdout lines.
}
\description{
Internal helper that executes ffprobe with the given arguments and returns
its stdout. The single exec site for every ffprobe query in the package.
}
\keyword{internal}
