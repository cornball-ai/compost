# frame_export: seek-before-input single-frame grab.

vid <- tempfile(fileext = ".mp4"); file.create(vid)

cmd <- frame_export(vid, 4.0, "frame.png", dry_run = TRUE)
expect_true(grepl("^ffmpeg ", cmd))
expect_true(grepl("-ss 4 ", cmd))      # seek
expect_true(grepl("-frames:v 1", cmd)) # single frame

# -ss precedes -i (fast keyframe seek).
expect_true(regexpr("-ss ", cmd) < regexpr("-i ", cmd))

unlink(vid)
