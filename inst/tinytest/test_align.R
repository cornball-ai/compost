# align_audio / align_overlaps: place chunk media on the narration clock by
# the audio each chunk carries. Fixture: a noise narration (white noise has a
# delta autocorrelation, so alignment is unambiguous) sliced into chunk audio
# muxed over synthetic video.

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    narr <- tempfile(fileext = ".wav")
    system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi",
                                "-i", "anoisesrc=duration=6:sample_rate=24000:amplitude=0.5",
                                narr)), stdout = FALSE, stderr = FALSE)
    mk_chunk <- function(astart, adur, frames) {
        f <- tempfile(fileext = ".mp4")
        system2("ffmpeg", shQuote(c("-nostdin", "-y",
                                    "-f", "lavfi",
                                    "-i", sprintf("testsrc2=duration=%g:size=64x64:rate=24", frames / 24),
                                    "-ss", astart, "-t", adur, "-i", narr,
                                    "-map", "0:v", "-map", "1:a",
                                    "-c:v", "libx264", "-qp", "0",
                                    "-pix_fmt", "yuv420p", "-c:a", "aac",
                                    "-shortest", f)),
                stdout = FALSE, stderr = FALSE)
        f
    }
    a <- mk_chunk(0, 2, 48)        # at 0.000
    b <- mk_chunk(1.625, 2, 48)    # at 1.625: A's video tail overruns by 9f
    cc <- mk_chunk(4, 1.5, 36)     # at 4.000: gap after B -> butt

    off_b <- align_audio(b, narr)
    expect_true(abs(off_b - 1.625) <= 0.02)
    # Noise envelopes are nearly flat, so even the true lag correlates weakly
    # (~0.5); real speech scores 0.75+. The offset accuracy is the real test.
    expect_true(attr(off_b, "correlation") > 0.3)

    m <- align_overlaps(c(a, b, cc), narr)
    expect_equal(m$overlap, c(0L, 9L, 0L))
    expect_true(all(abs(m$offset - c(0, 1.625, 4)) <= 0.02))
    expect_equal(m$frames, c(48L, 48L, 36L))

    unlink(c(narr, a, b, cc))
}
