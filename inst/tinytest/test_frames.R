# frames_clip(): command construction + at_home encode.

dir <- tempfile("framestest")
dir.create(dir)
file.create(file.path(dir, "frame_0001.png"))

cmd <- frames_clip(dir, file.path(dir, "out.mp4"), fps = 30, dry_run = TRUE)
expect_true(grepl("^ffmpeg -y", cmd))
expect_true(grepl("-framerate 30", cmd))
expect_true(grepl("-start_number 1", cmd))
expect_true(grepl("frame_%04d\\.png", cmd))
expect_true(grepl("format=yuv420p,setsar=1", cmd))
expect_true(grepl("-crf 18", cmd))

# size adds a scale step ahead of the format normalization.
cmd2 <- frames_clip(dir, file.path(dir, "out.mp4"), size = c(640, 480),
                    dry_run = TRUE)
expect_true(grepl("scale=640:480,format=yuv420p", cmd2))

# A custom pattern with no matching first frame errors early.
expect_error(frames_clip(dir, file.path(dir, "o.mp4"), pattern = "f_%03d.png"),
             "first frame not found")

unlink(dir, recursive = TRUE)

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    dir <- tempfile("framesrender")
    dir.create(dir)
    compost:::.run_ffmpeg(c("-y", "-f", "lavfi", "-i",
                            "testsrc2=size=320x240:rate=30", "-frames:v", "10",
                            file.path(dir, "frame_%04d.png")))

    out <- file.path(dir, "seq.mp4")
    frames_clip(dir, out, fps = 30)
    expect_equal(as.integer(probe(out, "nb_frames")), 10L)
    expect_true(abs(probe(out, "duration") - 10 / 30) < 0.05)

    unlink(dir, recursive = TRUE)
}
