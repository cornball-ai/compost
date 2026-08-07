% tinyrox says don't edit this manually, but it can't stop you!
\name{execute_plan}
\alias{execute_plan}
\title{Execute a composition plan}
\usage{
execute_plan(plan, output, media_dir = NULL, overwrite = TRUE, dry_run = FALSE)
}
\arguments{
\item{plan}{A plan-shaped list: `schema`, `canvas`, `fps`,
`tick_rate`, `duration`, and the `video`, `transitions`, `audio`,
`captions` data frames. Unknown extra fields are ignored.}

\item{output}{Path for the output video file.}

\item{media_dir}{Directory the plan's `media` bindings are relative
to.}

\item{overwrite}{If TRUE (default), overwrite the output file.}

\item{dry_run}{If TRUE, return the ffmpeg command without executing.}
}
\value{
Invisibly returns the output path. If dry_run, returns the
  command string.
}
\description{
Lowers an already-resolved composition plan (the contract in
cornductor's `inst/spec/composition-plan.md`) to a single ffmpeg
invocation: rows painted in layer order onto a black canvas at their
integer-tick times and destination rects, head-fed dissolves
synthesized from the incoming row's handle media, audio rows summed
at unity, captions burned last, and the output cut frame-exactly at
the plan's duration. No timeline, no metadata, no placement
decisions: the plan already made them.
}
\details{
Refusals are loud and produce no output; the refusal table lives in
the spec. In particular a viewer plan (`NA` media) refuses, absolute
media paths refuse (bindings are `media_dir`-relative), and an image
row with nonzero `source_in` refuses.

}
\examples{
\dontrun{
execute_plan(plan, "out.mp4", media_dir = "project.otiod")
}
}
