# audio_convert(): command construction.

dir <- tempfile("cvtest")
dir.create(dir)
src <- file.path(dir, "audio.mp3")
file.create(src)

# Speech-to-text prep: resample to 16 kHz, no channel/bitrate forcing.
cmd <- audio_convert(src, file.path(dir, "speech.wav"),
                     sample_rate = 16000, dry_run = TRUE)
expect_true(grepl("-ar 16000", cmd))
expect_true(grepl("-vn", cmd))
expect_false(grepl("-ac ", cmd))
expect_false(grepl("-b:a ", cmd))
expect_true(grepl("speech\\.wav$", cmd))

# Mono + bitrate + explicit codec all flow through.
cmd2 <- audio_convert(src, file.path(dir, "out.mp3"), channels = 1,
                      bitrate = "192k", codec = "libmp3lame", dry_run = TRUE)
expect_true(grepl("-ac 1", cmd2))
expect_true(grepl("-b:a 192k", cmd2))
expect_true(grepl("-c:a libmp3lame", cmd2))

# Straight transcode: no optional flags.
cmd3 <- audio_convert(src, file.path(dir, "copy.wav"), dry_run = TRUE)
expect_false(grepl("-ar ", cmd3))
expect_false(grepl("-ac ", cmd3))
