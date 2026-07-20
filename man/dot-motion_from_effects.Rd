% tinyrox says don't edit this manually, but it can't stop you!
\name{.motion_from_effects}
\alias{.motion_from_effects}
\title{Motion spec from a clip's OTIO effects}
\usage{
.motion_from_effects(effects)
}
\arguments{
\item{effects}{List of rotio Effect objects (a clip's effects).}
}
\value{
A Ken Burns metadata list for \code{\link{still_clip}}, or NULL.
}
\description{
Reads the open \code{cornball.*} effect namespace (see
\code{inst/schema/cornball-effects.md}). Effects outside the namespace
belong to other ecosystems and are ignored silently. Inside it, anything
this renderer cannot honor -- an unrecognized effect_name, enabled = FALSE,
or a metadata schema newer than supported -- degrades to a static clip with
one warning. The first usable motion effect wins; extras warn.
}
\keyword{internal}
