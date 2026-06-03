% tinyrox says don't edit this manually, but it can't stop you!
\name{.build_zoom_expr}
\alias{.build_zoom_expr}
\title{Build Piecewise Zoom Expression for FFmpeg}
\usage{
.build_zoom_expr(frame_starts, zoom_values, transition_frames)
}
\arguments{
\item{frame_starts}{Integer vector of frame numbers where each segment begins.}

\item{zoom_values}{Numeric vector of zoom values for each segment.}

\item{transition_frames}{Number of frames for each transition.}
}
\value{
Character string with the FFmpeg expression.
}
\description{
Constructs a nested if() expression that steps between zoom values
at frame boundaries, with linear interpolation during transitions.
}
\keyword{internal}
