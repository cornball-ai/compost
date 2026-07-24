% tinyrox says don't edit this manually, but it can't stop you!
\name{.is_absolute_path}
\alias{.is_absolute_path}
\title{Is a media url an absolute filesystem path?}
\usage{
.is_absolute_path(url)
}
\arguments{
\item{url}{A media reference url.}
}
\value{
TRUE if \code{url} is an absolute filesystem path.
}
\description{
Recognizes the forms an OTIO bundle can carry regardless of the OS it was
authored on: POSIX absolute paths (leading \code{/}), Windows drive paths
(\code{C:\\} or \code{C:/}), and UNC paths (leading backslash). Anything
else is relative and gets resolved against \code{media_dir}. Deliberately
platform-agnostic: a bundle written on Linux may be rendered on Windows and
vice versa, so the test must not depend on \code{.Platform$file.sep}.
}
\keyword{internal}
