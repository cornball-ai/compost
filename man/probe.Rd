% tinyrox says don't edit this manually, but it can't stop you!
\name{probe}
\alias{probe}
\title{Probe Video Metadata}
\usage{
probe(file, field = "duration")
}
\arguments{
\item{file}{Path to media file.}

\item{field}{Field to query. One of "duration", "width", "height", "codec_name",
"r_frame_rate", or any valid ffprobe stream entry. Use "all" to return
duration, width, height, and codec as a named list.}
}
\value{
For single fields, returns a numeric value (duration, width, height)
  or character string (codec_name, etc.). For field = "all", returns a named list.
}
\description{
Query video file properties via ffprobe.
}
\examples{
\dontrun{
probe("video.mp4")                     # duration (default)
probe("video.mp4", "width")            # video width
probe("video.mp4", "all")              # list of duration, width, height, codec
}

}
