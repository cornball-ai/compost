# mux_av(): stream-copy command; at_home real mux of picture and sound.

v <- tempfile(fileext = ".mp4")
a <- tempfile(fileext = ".m4a")
file.create(v, a)

cmd <- mux_av(v, a, "out.mp4", dry_run = TRUE)
expect_true(grepl("-map 0:v:0", cmd, fixed = TRUE))
expect_true(grepl("-map 1:a:0", cmd, fixed = TRUE))
expect_true(grepl("-c copy", cmd, fixed = TRUE))
expect_true(grepl("-n ", cmd, fixed = TRUE))
expect_true(grepl("-y ", mux_av(v, a, "out.mp4", overwrite = TRUE,
                                 dry_run = TRUE), fixed = TRUE))

# Missing inputs and an existing output are refused before ffmpeg runs.
expect_error(mux_av(tempfile(), a, "out.mp4", dry_run = TRUE))
expect_error(mux_av(v, a, v), "already exists")
unlink(c(v, a))

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    local({
        d <- tempfile("mux-")
        dir.create(d)
        on.exit(unlink(d, recursive = TRUE), add = TRUE)
        pic <- file.path(d, "pic.mp4")
        snd <- file.path(d, "snd.m4a")
        out <- file.path(d, "out.mp4")
        system2("ffmpeg", c("-nostdin", "-y", "-f", "lavfi", "-i",
                            "color=c=red:s=64x64:d=1:r=24", "-c:v", "libx264",
                            "-pix_fmt", "yuv420p", pic), stdout = FALSE, stderr = FALSE)
        system2("ffmpeg", c("-nostdin", "-y", "-f", "lavfi", "-i",
                            "sine=frequency=440:duration=1", "-c:a", "aac", snd),
                stdout = FALSE, stderr = FALSE)
        expect_identical(mux_av(pic, snd, out), out)
        streams <- system2("ffprobe", c("-v", "error", "-show_entries",
                                         "stream=codec_type", "-of", "csv=p=0", out),
                           stdout = TRUE)
        expect_equal(sort(streams), c("audio", "video"))
        expect_error(mux_av(pic, snd, out), "already exists")
    })
}
