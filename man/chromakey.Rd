% tinyrox says don't edit this manually, but it can't stop you!
\name{chromakey}
\alias{chromakey}
\title{Chromakey (Green/Blue Screen) Compositing}
\usage{
chromakey(
  background,
  foreground,
  output,
  color = "0x00ff00",
  similarity = 0.1,
  blend = 0.075,
  overwrite = TRUE,
  dry_run = FALSE
)
}
\arguments{
\item{background}{Path to background video or image.}

\item{foreground}{Path to foreground video with green/blue screen.}

\item{output}{Path for output video file.}

\item{color}{Hex color to key out (default "0x00ff00" for green).}

\item{similarity}{Chromakey similarity threshold (default 0.1).
Higher values key out more of the color.}

\item{blend}{Chromakey blend amount (default 0.075).
Higher values create softer edges.}

\item{overwrite}{If TRUE (default), overwrite output file.}

\item{dry_run}{If TRUE, return the FFmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns command string.
}
\description{
Remove a colored background from foreground footage and composite
onto a background video or image using FFmpeg's chromakey filter.
}
\examples{
\dontrun{
chromakey("bg.mp4", "greenscreen.mp4", "out.mp4")
chromakey("bg.mp4", "bluescreen.mp4", "out.mp4", color = "0x0000ff")
}

}
