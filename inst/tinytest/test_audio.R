# Broadcast-clean audio chain: filter graph builder + command construction.

# Full chain, with a known duration -> in and out fades present.
af <- compost:::.broadcast_audio_filter(dehum = TRUE, target_lufs = -14,
                                        fade = 0.06, duration = 10, highpass = 80)
expect_true(grepl("^highpass=f=80,", af))
expect_true(grepl("equalizer=f=60:", af))
expect_true(grepl("equalizer=f=120:", af))
expect_true(grepl("equalizer=f=180:", af))
expect_true(grepl("afftdn=nr=12:nf=-25", af))
expect_true(grepl("acompressor=", af))
expect_true(grepl("loudnorm=I=-14:TP=-1.5:LRA=11", af))
expect_true(grepl("afade=t=in:st=0:d=0.06", af))
expect_true(grepl("afade=t=out:st=9.94:d=0.06", af)) # duration - fade

# dehum = FALSE drops the notches and the denoise.
af2 <- compost:::.broadcast_audio_filter(dehum = FALSE, duration = 10)
expect_false(grepl("equalizer", af2))
expect_false(grepl("afftdn", af2))

# Unknown duration (NA) -> in-fade only, no out-fade.
af3 <- compost:::.broadcast_audio_filter(duration = NA_real_)
expect_true(grepl("afade=t=in", af3))
expect_false(grepl("afade=t=out", af3))

# fade = 0 -> no fades at all.
af4 <- compost:::.broadcast_audio_filter(fade = 0, duration = 10)
expect_false(grepl("afade", af4))

# dry_run command construction.
wav <- tempfile(fileext = ".wav"); file.create(wav)
cmd <- broadcast_audio(wav, "clean.wav", dry_run = TRUE)
expect_true(grepl("^ffmpeg ", cmd))
expect_true(grepl("-af ", cmd))

# normalize_audio: the loudnorm-only pass.
cmdn <- normalize_audio(wav, "level.wav", dry_run = TRUE)
expect_true(grepl("loudnorm=I=-16:LRA=11:TP=-1.5", cmdn))
expect_true(grepl("-vn", cmdn))
expect_true(grepl("-b:a 192k", cmdn))
cmdn2 <- normalize_audio(wav, "level.wav", integrated = -14, bitrate = NULL,
                         dry_run = TRUE)
expect_true(grepl("loudnorm=I=-14:", cmdn2))
expect_false(grepl("-b:a", cmdn2))

unlink(wav)

# In-place normalize on a real sine (at_home).
if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    w <- tempfile(fileext = ".wav")
    compost:::.run_ffmpeg(c("-y", "-f", "lavfi", "-i",
                            "sine=frequency=440:duration=2", "-ar", "48000",
                            "-ac", "1", w))
    d0 <- probe(w, "duration")
    normalize_audio(w)
    expect_true(file.exists(w))
    expect_true(abs(probe(w, "duration") - d0) < 0.1)
    unlink(w)
}
