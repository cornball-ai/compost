# still_clip(): zoompan filter construction + command; at_home real renders.

# --- .kenburns_filter ------------------------------------------------------

# Static: constant zoom, pinned crop, exact filter.
f <- compost:::.kenburns_filter(NULL, 60L, 1080L, 1920L, 30)
expect_equal(f, "zoompan=z='1':x='0':y='0':d=60:s=1080x1920:fps=30")

# Full spec, smooth easing: zoom ramps, x anchor constant, y anchor descends.
m <- list(zoom_from = 1, zoom_to = 1.15, from = c(0.5, 0.5), to = c(0.5, 0.4),
          ease = "smooth")
f2 <- compost:::.kenburns_filter(m, 60L, 1080L, 1920L, 30)
expect_true(grepl("z='1.000000+0.150000*pow(min(on/59,1),2)*(3-2*min(on/59,1))'",
                  f2, fixed = TRUE))
expect_true(grepl("x='clip((0.500000)*iw-iw/zoom/2,0,iw-iw/zoom)'",
                  f2, fixed = TRUE))
expect_true(grepl("y='clip((0.500000-0.100000*pow", f2, fixed = TRUE))
expect_true(grepl("d=60:s=1080x1920:fps=30", f2, fixed = TRUE))

# Linear easing has no pow.
m$ease <- "linear"
f3 <- compost:::.kenburns_filter(m, 60L, 1080L, 1920L, 30)
expect_true(grepl("min(on/59,1)", f3, fixed = TRUE))
expect_false(grepl("pow", f3))

# Zoom below 1 is clamped; ease defaults to smooth; to defaults to from.
f4 <- compost:::.kenburns_filter(list(zoom_from = 0.5, zoom_to = 1), 60L,
                                 640L, 480L, 30)
expect_true(grepl("z='1.000000'", f4, fixed = TRUE))

# Deserialization shape: lists of numbers coerce.
m5 <- list(zoom_from = 1L, zoom_to = list(1.2), from = list(0.5, 0.5),
           to = list(0.4, 0.5))
f5 <- compost:::.kenburns_filter(m5, 30L, 640L, 480L, 30)
expect_true(grepl("0.500000-0.100000", f5, fixed = TRUE))

# Bad inputs error.
expect_error(compost:::.kenburns_filter(list(ease = "bounce"), 60L, 640L,
                                        480L, 30), "unknown ease")
expect_error(compost:::.kenburns_filter(list(from = 0.5), 60L, 640L, 480L, 30),
             "two numbers")

# --- .still_size -----------------------------------------------------------

# No framing: even-round the source.
expect_equal(compost:::.still_size(320, 240), c(320L, 240L))
expect_equal(compost:::.still_size(321, 239), c(320L, 240L))

# Landscape slide into a square box: width-bound, even-rounded height.
expect_equal(compost:::.still_size(1890, 1063, list(pad = c(1080, 1080))),
             c(1080L, 608L))

# Portrait slide into shorts: fills the box.
expect_equal(compost:::.still_size(1417, 2520, list(pad = c(1080, 1920))),
             c(1080L, 1920L))

# --- still_clip command construction ---------------------------------------

dir <- tempfile("stilltest")
dir.create(dir)
png <- file.path(dir, "src.png")
file.create(png)

cmd <- still_clip(png, file.path(dir, "out.mp4"), duration = 4.2,
                  size = c(1080, 1920), dry_run = TRUE)
expect_true(grepl("^ffmpeg -y", cmd))
expect_true(grepl("zoompan=", cmd))
expect_true(grepl("format=yuv420p,setsar=1", cmd))
expect_true(grepl("-frames:v 126", cmd))
expect_true(grepl("-crf 18", cmd))
expect_true(grepl("\\+faststart", cmd))
expect_true(grepl("out\\.mp4$", cmd))

# A duration shorter than one frame errors.
expect_error(still_clip(png, file.path(dir, "o.mp4"), duration = 0.001,
                        size = c(640, 480), dry_run = TRUE),
             "at least one frame")
unlink(dir, recursive = TRUE)

# --- real renders ----------------------------------------------------------

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    dir <- tempfile("stillrender")
    dir.create(dir)
    png <- file.path(dir, "src.png")
    compost:::.run_ffmpeg(c("-y", "-f", "lavfi", "-i", "testsrc2=size=320x240",
                            "-frames:v", "1", png))

    # Static: exact frame count, source dimensions.
    out <- file.path(dir, "static.mp4")
    still_clip(png, out, duration = 2)
    expect_equal(as.integer(probe(out, "nb_frames")), 60L)
    expect_equal(probe(out, "width"), 320)
    expect_equal(probe(out, "height"), 240)

    # Ken Burns: same frame count, but the picture actually moves.
    out2 <- file.path(dir, "kb.mp4")
    still_clip(png, out2, duration = 2,
               motion = list(zoom_from = 1, zoom_to = 1.3,
                             from = c(0.5, 0.5), to = c(0.3, 0.3)))
    expect_equal(as.integer(probe(out2, "nb_frames")), 60L)
    a <- file.path(dir, "a.png")
    b <- file.path(dir, "b.png")
    frame_export(out2, 0, a)
    frame_export(out2, 1.9, b)
    expect_false(unname(tools::md5sum(a)) == unname(tools::md5sum(b)))

    unlink(dir, recursive = TRUE)
}
