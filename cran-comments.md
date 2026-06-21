## Submission

This is a new submission.

## Test environments

- Local: Ubuntu 24.04, R release
- Windows 10: R 4.6.0 and R-devel (pre-release), via `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 1 note

The one NOTE is the expected "New submission" / Maintainer note.

## Notes for the reviewer

- **System requirement.** The package wraps the `ffmpeg` command-line tool
  (declared in `SystemRequirements:`, version >= 5.0). Every exported function
  shells out to `ffmpeg`/`ffprobe`; nothing is reimplemented in R.

- **`\dontrun` in examples.** All examples are wrapped in `\dontrun{}` rather
  than `\donttest{}`. This is deliberate: the examples require both the
  `ffmpeg` binary (which is not guaranteed to be present on the check machines)
  and user-supplied media files (video/audio that does not ship with the
  package). They cannot execute in CRAN's environment, which is the documented
  case for `\dontrun` over `\donttest`. The package's tests cover the
  command-construction logic without invoking `ffmpeg`.
