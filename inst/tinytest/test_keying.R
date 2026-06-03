# colorkey -> ProRes 4444 alpha: command construction with an explicit colour.
# (color = "auto" would shell out to ffmpeg to sample, so test with a fixed one.)

vid <- tempfile(fileext = ".mp4"); file.create(vid)

cmd <- colorkey(vid, "out.mov", color = "0x000000", dry_run = TRUE)
expect_true(grepl("^ffmpeg ", cmd))
expect_true(grepl("colorkey=0x000000:0.15:0.05", cmd))
expect_true(grepl("prores_ks", cmd))
expect_true(grepl("-profile:v 4444", cmd))
expect_true(grepl("yuva444p10le", cmd))

# Threshold / blend flow through to the filter.
cmd2 <- colorkey(vid, "out.mov", color = "green", similarity = 0.2, blend = 0.1,
                 dry_run = TRUE)
expect_true(grepl("colorkey=green:0.2:0.1", cmd2))

unlink(vid)
