% tinyrox says don't edit this manually, but it can't stop you!
\name{still_clip}
\alias{still_clip}
\title{Render a Still Image as a Video Clip}
\usage{
still_clip(image, output, duration, fps = 30, size = NULL, motion = NULL,
           overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{image}{Path to the input image (png, jpg, ...).}

\item{output}{Path for the output video file.}

\item{duration}{Clip length in seconds.}

\item{fps}{Output frame rate (default 30).}

\item{size}{Output dimensions as \code{c(width, height)}, or NULL (default)
to use the source dimensions rounded to even.}

\item{motion}{NULL (default) for a static clip, or a Ken Burns spec: a list
with any of \code{zoom_from}, \code{zoom_to} (numeric >= 1; values below 1
are clamped), \code{from}, \code{to} (crop-window anchor as \code{c(x, y)}
fractions of the source, top-left origin; the window is centered on the
anchor and clamped to the frame), and \code{ease} (\code{"linear"} or
\code{"smooth"}, default smooth).}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns the command
  string.
}
\description{
Turns one image into a video of \code{duration} seconds, optionally
animated by a deterministic Ken Burns pan/zoom described by \code{motion}.
Both the static and the animated case go through ffmpeg's \code{zoompan},
so the outputs are byte-compatible for \code{\link{concat}} and
\code{\link{crossfade_concat}} regardless of motion.
}
\details{
\code{zoompan} stretches its crop to \code{size} without preserving the
source aspect ratio, so \code{size} should match the source's aspect (or a
deliberate crop of it). Renderers letterbox afterwards; see
\code{\link{pad}}.

}
\examples{
\dontrun{
still_clip("slide01.png", "slide01.mp4", duration = 4.2)
still_clip("slide01.png", "slide01.mp4", duration = 4.2,
           motion = list(zoom_from = 1, zoom_to = 1.15,
                         from = c(0.5, 0.5), to = c(0.5, 0.4)))
}

}
