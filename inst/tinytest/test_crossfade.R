# crossfade_concat: dry-run command shape + an at_home end-to-end stitch.

# --- single clip: passthrough, no xfade ------------------------------------
v1 <- tempfile(fileext = ".mp4")
file.create(v1)
cmd <- crossfade_concat(v1, "out.mp4", dry_run = TRUE)
expect_true(grepl("\\[0:v\\]null\\[vout\\]", cmd))
unlink(v1)

# --- end-to-end on generated clips (needs ffmpeg; local only) ---------------
if (at_home()) {
    mk <- function(sec, col) {
        f <- tempfile(fileext = ".mp4")
        system2("ffmpeg", shQuote(c("-y", "-f", "lavfi",
                                    "-i", sprintf("color=c=%s:s=128x128:d=%g:r=24", col, sec),
                                    "-f", "lavfi", "-i", "anullsrc=r=44100",
                                    "-shortest", "-c:v", "libx264", "-pix_fmt", "yuv420p",
                                    "-c:a", "aac", f)),
                stdout = FALSE, stderr = FALSE)
        f
    }
    a <- mk(3, "red"); b <- mk(3, "green"); c <- mk(3, "blue")
    out <- tempfile(fileext = ".mp4")
    crossfade_concat(c(a, b, c), out, fade = 0.5)
    expect_true(file.exists(out))
    # 3 clips * 3s - 2 fades * 0.5s = 8s
    expect_equal(round(as.numeric(probe(out, "duration"))), 8)

    # a hard cut on one join: same length, mixed graph still valid
    out2 <- tempfile(fileext = ".mp4")
    crossfade_concat(c(a, b, c), out2, fade = 0.5, cuts = c(TRUE, FALSE))
    expect_true(file.exists(out2))
    expect_equal(round(as.numeric(probe(out2, "duration"))), 8)

    # per-join fades: second join is a plain butt join (fade 0), so only the
    # first join consumes overlap -> 3*3 - 0.5 = 8.5s
    out3 <- tempfile(fileext = ".mp4")
    crossfade_concat(c(a, b, c), out3, fade = c(0.5, 0))
    expect_true(file.exists(out3))
    expect_equal(round(as.numeric(probe(out3, "duration")) * 2) / 2, 8.5)

    # fade vector of the wrong length errors
    expect_error(crossfade_concat(c(a, b, c), out3, fade = c(0.5, 0, 0.5)),
                 pattern = "one value per join")

    unlink(c(a, b, c, out, out2, out3))
}
