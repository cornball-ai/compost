# pad(): command construction (dry_run, no media needed).

dir <- tempfile("padtest")
dir.create(dir)
src <- file.path(dir, "clip.mp4")
file.create(src)

# Square -> vertical shorts: fit into 1080x1080, pad to 1080x1920 top-aligned.
cmd <- pad(src, file.path(dir, "out.mp4"), 1080, 1920,
           fit_width = 1080, fit_height = 1080, x = 0, y = 0, dry_run = TRUE)
expect_true(grepl("scale=1080:1080:force_original_aspect_ratio=decrease", cmd))
expect_true(grepl("pad=1080:1920:0:0:black", cmd))
expect_true(grepl("out\\.mp4$", cmd))

# Default: centered on a same-size canvas.
cmd2 <- pad(src, file.path(dir, "sq.mp4"), 1080, 1080, dry_run = TRUE)
expect_true(grepl("pad=1080:1080:\\(ow-iw\\)/2:\\(oh-ih\\)/2:black", cmd2))

# Custom color passes through.
cmd3 <- pad(src, file.path(dir, "w.mp4"), 640, 480, color = "white", dry_run = TRUE)
expect_true(grepl(":white", cmd3))
