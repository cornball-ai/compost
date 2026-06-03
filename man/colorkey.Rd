% tinyrox says don't edit this manually, but it can't stop you!
\name{colorkey}
\alias{colorkey}
\title{Colour-Key to a ProRes 4444 File with Alpha}
\usage{
colorkey(input, output, color = "auto", similarity = 0.15, blend = 0.05,
         sample_xy = c(0, 0), overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{input}{Path to input video.}

\item{output}{Path for output .mov file.}

\item{color}{Hex colour (e.g. "0x000000"), a name ("green"), or "auto" to
sample it from a corner pixel (default).}

\item{similarity}{Key similarity threshold (default 0.15).}

\item{blend}{Key edge blend (default 0.05).}

\item{sample_xy}{Pixel c(x, y) to sample when color = "auto" (default c(0, 0)).}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Key out a flat background colour and write ProRes 4444 (\code{yuva444p10le}) so
the alpha channel survives. Where \code{\link{chromakey}} composites the keyed
foreground straight onto a background, this writes a standalone alpha clip you
overlay later. By default the key colour is auto-sampled from a corner pixel
(generated backgrounds are sometimes black, sometimes grey); pass
\code{color} to override.
}
\examples{
\dontrun{
colorkey("ltx.mp4", "ltx_alpha.mov")
colorkey("scene.mp4", "scene_alpha.mov", color = "0x000000")
}

}
