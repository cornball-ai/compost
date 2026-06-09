# rms_curve: filter construction and ametadata parsing (no ffmpeg run), plus an
# at_home end-to-end pass on a generated tone.

# --- filter chain construction ----------------------------------------------
af <- compost:::.rms_filter(1024, "/tmp/x.txt")
expect_true(grepl("asetnsamples=1024", af))
expect_true(grepl("astats=metadata=1:reset=1", af))
expect_true(grepl("RMS_level:file=/tmp/x.txt", af))

# --- ametadata parsing: pts_time then RMS_level pairs; -inf -> -120 ----------
lines <- c("frame:0    pts:0     pts_time:0",
           "lavfi.astats.Overall.RMS_level=-23.5",
           "frame:1    pts:1024  pts_time:0.0213",
           "lavfi.astats.Overall.RMS_level=-inf")
p <- compost:::.parse_rms(lines)
expect_equal(length(p$time), 2L)
expect_equal(p$time, c(0, 0.0213))
expect_equal(p$rms, c(-23.5, -120))

# garbage in -> empty out
expect_equal(length(compost:::.parse_rms(c("nothing", "here"))$rms), 0L)

# --- end-to-end on a generated tone (needs ffmpeg; local only) ---------------
if (at_home()) {
    wav <- tempfile(fileext = ".wav")
    system2("ffmpeg", shQuote(c("-y", "-f", "lavfi",
                                "-i", "sine=frequency=200:duration=2",
                                "-ar", "16000", wav)),
            stdout = FALSE, stderr = FALSE)
    cur <- rms_curve(wav)
    expect_true(length(cur$time) > 10)
    expect_equal(length(cur$time), length(cur$rms))
    expect_true(all(is.finite(cur$rms)))
    unlink(wav)
}
