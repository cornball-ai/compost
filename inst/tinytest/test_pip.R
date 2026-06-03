# PiP corner overlay: filter geometry + command construction.

# Corner math: top-left origin, +Y down. "right"/"lower" flip to main_w/main_h.
expect_equal(
  compost:::.pip_filter(scale = 0.604, margin = 230, corner = "upper-right"),
  "[1:v]scale=iw*0.604:ih*0.604[pip];[0:v][pip]overlay=main_w-overlay_w-230:230"
)
expect_equal(
  compost:::.pip_filter(0.5, 100, "upper-left"),
  "[1:v]scale=iw*0.5:ih*0.5[pip];[0:v][pip]overlay=100:100"
)
expect_equal(
  compost:::.pip_filter(0.5, 100, "lower-right"),
  "[1:v]scale=iw*0.5:ih*0.5[pip];[0:v][pip]overlay=main_w-overlay_w-100:main_h-overlay_h-100"
)
expect_equal(
  compost:::.pip_filter(0.5, 100, "lower-left"),
  "[1:v]scale=iw*0.5:ih*0.5[pip];[0:v][pip]overlay=100:main_h-overlay_h-100"
)

# dry_run builds an ffmpeg command without touching ffmpeg. Use real input
# paths (normalizePath mustWork = TRUE) but a non-existent output.
bg <- tempfile(fileext = ".mp4"); file.create(bg)
fg <- tempfile(fileext = ".mov"); file.create(fg)

cmd <- pip(bg, fg, "out.mp4", dry_run = TRUE)
expect_true(grepl("^ffmpeg ", cmd))
expect_true(grepl("overlay=main_w-overlay_w-230:230", cmd))
expect_false(grepl("-frames:v 1", cmd)) # video output, not a still

# Still-image output adds -frames:v 1.
cmd_png <- pip(bg, fg, "out.png", dry_run = TRUE)
expect_true(grepl("-frames:v 1", cmd_png))

unlink(c(bg, fg))
