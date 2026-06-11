% tinyrox says don't edit this manually, but it can't stop you!
\name{detect_overlap}
\alias{detect_overlap}
\title{Detect the duplicated-frame overlap between two consecutive clips}
\usage{
detect_overlap(from, to, max_scan = 16L, threshold = 0.95)
}
\arguments{
\item{from}{Path to the earlier clip.}

\item{to}{Path to the following clip (whose head may replay \code{from}'s
tail).}

\item{max_scan}{Number of head frames of \code{to} to scan (default 16).}

\item{threshold}{Minimum SSIM for a frame to count as a duplicate
(default 0.95).}
}
\value{
Integer count of overlapping frames at the head of \code{to}
  (0 = none), with the per-frame SSIM curve attached as attribute
  \code{"ssim"}.
}
\description{
Compares the last frame of \code{from} against the first \code{max_scan}
frames of \code{to} (SSIM, via ffmpeg). When the best match is
duplicate-grade (\code{>= threshold}), the matching frame index marks the
end of a conditioning-head replay and the overlap is \code{index + 1}
frames; otherwise the overlap is 0 (the head is new content -- either a
trimmed chained clip or an independent generation).
}
\details{
Empirical calibration on LTX-2 talking-head chunks (256px, re-encoded
H.264): a kept 9-frame conditioning head peaks at 0.98-0.99 on frame 8;
plain continuation peaks ~0.84 on frame 0; independently generated clips
sit flat near 0.03. The default \code{threshold} of 0.95 separates these
cleanly.

}
\examples{
\dontrun{
detect_overlap("media/chunk01.mp4", "media/chunk02.mp4")
}
}
