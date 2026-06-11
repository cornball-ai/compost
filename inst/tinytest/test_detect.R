# detect_overlap: slice one lossless master so the "chained" clip's head is a
# pixel-exact replay of the previous clip's tail (9 frames), mirroring a kept
# conditioning head; a slice from further along is continuation-without-replay.

if (at_home()) {
    master <- tempfile(fileext = ".mp4")
    system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi",
                                "-i", "testsrc2=duration=4:size=128x128:rate=24",
                                "-c:v", "libx264", "-qp", "0",
                                "-pix_fmt", "yuv420p", master)),
            stdout = FALSE, stderr = FALSE)
    slice <- function(from, to) {
        f <- tempfile(fileext = ".mp4")
        vf <- sprintf("trim=start_frame=%d:end_frame=%d,setpts=PTS-STARTPTS",
                      from, to)
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-i", master, "-vf", vf,
                                    "-c:v", "libx264", "-qp", "0",
                                    "-pix_fmt", "yuv420p", f)),
                stdout = FALSE, stderr = FALSE)
        f
    }
    a <- slice(0, 48)   # frames 0..47
    b <- slice(39, 87)  # head = frames 39..47 -> 9-frame replay of a's tail
    cc <- slice(60, 96) # no replay: starts past a's end

    ov <- detect_overlap(a, b)
    expect_equal(as.integer(ov), 9L)
    expect_true(max(attr(ov, "ssim")) >= 0.95)
    expect_equal(which.max(attr(ov, "ssim")), 9L)

    expect_equal(as.integer(detect_overlap(a, cc)), 0L)

    # max_scan shorter than the replay still finds the in-window peak or
    # returns 0 -- never errors.
    ov4 <- detect_overlap(a, b, max_scan = 4L)
    expect_true(as.integer(ov4) %in% 0:4)

    unlink(c(master, a, b, cc))
}
