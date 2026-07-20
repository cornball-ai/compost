# audio_concat(): filter graph builder + command; at_home real concat.

# --- .audio_concat_filter --------------------------------------------------

# Two inputs with gap and tail: normalize both, two silences, one concat.
f <- compost:::.audio_concat_filter(2L, 0.5, 0.25, 48000L, 1L)
expect_true(grepl(
        "[0:a]aresample=48000,aformat=sample_fmts=fltp:channel_layouts=mono[c0]",
        f, fixed = TRUE))
expect_true(grepl("[1:a]aresample=48000", f, fixed = TRUE))
expect_true(grepl("anullsrc=r=48000:cl=mono,atrim=end=0.500000[g1]",
                  f, fixed = TRUE))
expect_true(grepl("atrim=end=0.250000[gt]", f, fixed = TRUE))
expect_true(grepl("[c0][g1][c1][gt]concat=n=4:v=0:a=1[aout]", f, fixed = TRUE))

# No silences: straight normalize + concat.
f2 <- compost:::.audio_concat_filter(2L, 0, 0, 44100L, 2L)
expect_false(grepl("anullsrc", f2))
expect_true(grepl("channel_layouts=stereo", f2, fixed = TRUE))
expect_true(grepl("[c0][c1]concat=n=2:v=0:a=1[aout]", f2, fixed = TRUE))

# Single input: tail still concats; bare normalize renames to [aout].
f3 <- compost:::.audio_concat_filter(1L, 0.35, 0.5, 48000L, 1L)
expect_true(grepl("[c0][gt]concat=n=2:v=0:a=1[aout]", f3, fixed = TRUE))
expect_false(grepl("[g1]", f3, fixed = TRUE))
f4 <- compost:::.audio_concat_filter(1L, 0, 0, 48000L, 1L)
expect_true(grepl("\\[aout\\]$", f4))
expect_false(grepl("concat", f4))

# Only mono and stereo are supported.
expect_error(compost:::.audio_concat_filter(2L, 0, 0, 48000L, 6L), "channels")

# --- command construction --------------------------------------------------

w <- tempfile(fileext = ".wav")
file.create(w)
cmd <- audio_concat(c(w, w), tempfile(fileext = ".mp3"), gap = 0.35,
                    sample_rate = 48000, channels = 1, bitrate = "192k",
                    dry_run = TRUE)
expect_true(grepl("^ffmpeg -y", cmd))
expect_true(grepl("-filter_complex", cmd))
expect_true(grepl("-map \\[aout\\]", cmd))
expect_true(grepl("-b:a 192k", cmd))
unlink(w)

# --- real concat -----------------------------------------------------------

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    dir <- tempfile("aconcat")
    dir.create(dir)
    w1 <- file.path(dir, "a.wav")
    w2 <- file.path(dir, "b.wav")
    compost:::.run_ffmpeg(c("-y", "-f", "lavfi", "-i",
                            "sine=frequency=440:duration=1", "-ar", "48000",
                            "-ac", "1", w1))
    compost:::.run_ffmpeg(c("-y", "-f", "lavfi", "-i",
                            "sine=frequency=220:duration=1", "-ar", "48000",
                            "-ac", "1", w2))

    # 1 + 0.5 + 1 + 0.25 seconds, within mp3 encoder padding.
    out <- file.path(dir, "bed.mp3")
    audio_concat(c(w1, w2), out, gap = 0.5, tail = 0.25)
    expect_true(abs(probe(out, "duration") - 2.75) < 0.1)

    # Defaults probed from the first input still produce a valid file.
    out2 <- file.path(dir, "plain.wav")
    audio_concat(c(w1, w2), out2)
    expect_true(abs(probe(out2, "duration") - 2) < 0.05)

    unlink(dir, recursive = TRUE)
}
