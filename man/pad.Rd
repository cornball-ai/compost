% tinyrox says don't edit this manually, but it can't stop you!
\name{pad}
\alias{pad}
\title{Scale to Fit and Pad onto a Canvas}
\usage{
pad(input, output, width, height, fit_width = width, fit_height = height,
    x = "(ow-iw)/2", y = "(oh-ih)/2", color = "black", overwrite = TRUE,
    dry_run = FALSE)
}
\arguments{
\item{color}{Pad color (default \code{"black"}).}

\item{overwrite}{Overwrite the output if it exists (default TRUE).}

\item{dry_run}{If TRUE, return the ffmpeg command string instead of running.}

\item{input,output}{Input and output media paths.}

\item{width,height}{Final canvas dimensions (pixels).}

\item{fit_width,fit_height}{Box to scale the input into before padding
(default the canvas dimensions).}

\item{x,y}{Position of the scaled input on the canvas as ffmpeg \code{pad}
expressions. Default centers it (\code{"(ow-iw)/2"}, \code{"(oh-ih)/2"});
pass \code{0}, \code{0} to top-left align (e.g. video at the top of a taller
canvas).}
}
\value{
The output path, invisibly (or the command string on \code{dry_run}).
}
\description{
Scales \code{input} to fit within \code{fit_width} x \code{fit_height}
(preserving aspect ratio, never upscaling past the box), then pads the result
onto a \code{width} x \code{height} canvas at \code{x},\code{y} filled with
\code{color}. This is the ffmpeg
\code{scale=...:force_original_aspect_ratio=decrease,pad=...} idiom for
letterboxing media into a fixed frame (e.g. a square clip into 1080x1920
vertical shorts with the video at the top and a black caption strip below).
}
\examples{
\dontrun{
# Letterbox a square clip into vertical shorts, video at the top:
pad("clip.mp4", "shorts.mp4", 1080, 1920,
    fit_width = 1080, fit_height = 1080, x = 0, y = 0)
}
}
