# subclip(): command construction.

dir <- tempfile("subcliptest")
dir.create(dir)
src <- file.path(dir, "audio.mp3")
file.create(src)

# Duration form, re-encoded (accurate).
cmd <- subclip(src, file.path(dir, "chunk.mp3"), start = 2.14, duration = 5,
               dry_run = TRUE)
expect_true(grepl("-ss 2.14", cmd))
expect_true(grepl("-t 5", cmd))
expect_false(grepl("-c copy", cmd))

# End form translates to a -to duration; stream copy when reencode = FALSE.
cmd2 <- subclip(src, file.path(dir, "head.mp3"), start = 1, end = 4,
                reencode = FALSE, dry_run = TRUE)
expect_true(grepl("-to 3", cmd2))
expect_true(grepl("-c copy", cmd2))

# duration takes precedence over end.
cmd3 <- subclip(src, file.path(dir, "x.mp3"), start = 0, duration = 2, end = 9,
                dry_run = TRUE)
expect_true(grepl("-t 2", cmd3))
expect_false(grepl("-to", cmd3))
