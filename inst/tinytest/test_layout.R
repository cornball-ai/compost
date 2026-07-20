# compose_layout(): validation, filter construction, and (at_home) real
# composes with pixel-color assertions per arrangement.

vertical <- list(schema = 1L, name = "vertical", canvas = c(1080L, 1920L),
                 slots = list(
                     narrator = list(rect = c(0L, 0L, 1080L, 960L),
                                     fit = "fill"),
                     visual = list(rect = c(0L, 960L, 1080L, 960L),
                                   fit = "fit")))
explainer <- list(schema = 1L, name = "explainer", canvas = c(1920L, 1080L),
                  slots = list(
                      visual = list(rect = c(0L, 0L, 1920L, 1080L),
                                    fit = "fit"),
                      narrator = list(rect = c(1476L, 636L, 384L, 384L),
                                      fit = "fill")))
newscast <- list(schema = 1L, name = "newscast", canvas = c(1920L, 1080L),
                 slots = list(
                     narrator = list(rect = c(0L, 0L, 1920L, 1080L),
                                     fit = "fill"),
                     visual = list(rect = c(1284L, 60L, 576L, 324L),
                                   fit = "fit")))

# --- .layout_slots: coercion + validation ------------------------------------

# JSON-deserialized shape (lists of numbers) coerces to integers.
jsonish <- list(schema = list(1), canvas = list(1080, 1920),
                slots = list(narrator = list(rect = list(0, 0, 1080, 960),
                                             fit = "fill"),
                             visual = list(rect = list(0, 960, 1080, 960),
                                           fit = "fit")))
vs <- compost:::.layout_slots(jsonish)
expect_equal(vs$canvas, c(1080L, 1920L))
expect_equal(vs$slots$visual$rect, c(0L, 960L, 1080L, 960L))

# Missing fit defaults to "fit"; missing schema reads as 1.
vs2 <- compost:::.layout_slots(list(canvas = c(100L, 100L),
                                    slots = list(a = list(rect = c(0L, 0L,
                                                                   100L, 100L)))))
expect_equal(vs2$slots$a$fit, "fit")

# Schema too new: one warning, NULL (render degradation).
expect_warning(v <- compost:::.layout_slots(
        list(schema = 2L, canvas = c(100L, 100L),
             slots = list(a = list(rect = c(0L, 0L, 100L, 100L))))),
        "newer")
expect_null(v)

# Malformed geometry is loud.
expect_error(compost:::.layout_slots(list(canvas = c(101L, 100L),
                                          slots = list())), "even")
expect_error(compost:::.layout_slots(
        list(canvas = c(100L, 100L),
             slots = list(a = list(rect = c(0L, 0L, 50L, 51L))))), "even")
expect_error(compost:::.layout_slots(
        list(canvas = c(100L, 100L),
             slots = list(a = list(rect = c(60L, 0L, 60L, 60L))))), "bounds")
expect_error(compost:::.layout_slots(
        list(canvas = c(100L, 100L),
             slots = list(a = list(rect = c(0L, 0L, 60L, 60L),
                                   fit = "stretch")))), "fit")
expect_error(compost:::.layout_slots(
        list(canvas = c(100L, 100L),
             slots = list(list(rect = c(0L, 0L, 60L, 60L))))), "named")

# --- .layout_filter: exact graphs --------------------------------------------

f <- compost:::.layout_filter(compost:::.layout_slots(vertical), 30, "visual")
expect_true(grepl("color=c=black:s=1080x1920:r=30,settb=AVTB[cv]", f,
                  fixed = TRUE))
# Narrator (input 0, paint first): fill = scale:increase + crop, frozen.
expect_true(grepl(paste0("[0:v]fps=30,scale=1080:960:force_original_aspect_",
                         "ratio=increase,crop=1080:960,setsar=1,",
                         "format=yuv420p,settb=AVTB,",
                         "tpad=stop_mode=clone:stop=-1[s1]"), f, fixed = TRUE))
# Visual (input 1, reference): fit = scale:decrease + pad, NO freeze.
expect_true(grepl(paste0("[1:v]fps=30,scale=1080:960:force_original_aspect_",
                         "ratio=decrease,pad=1080:960:(ow-iw)/2:(oh-ih)/2:",
                         "black,setsar=1,format=yuv420p,settb=AVTB[s2]"),
                  f, fixed = TRUE))
expect_true(grepl("[cv][s1]overlay=0:0[t1]", f, fixed = TRUE))
expect_true(grepl("[t1][s2]overlay=0:960[vout]", f, fixed = TRUE))

# Explainer: visual painted first (bottom), narrator PiP on top at its rect.
fe <- compost:::.layout_filter(compost:::.layout_slots(explainer), 30,
                               "visual")
expect_true(grepl("[t1][s2]overlay=1476:636[vout]", fe, fixed = TRUE))
expect_false(grepl("\\[s1\\][^;]*tpad", fe)) # visual chain (s1) unfrozen
expect_true(grepl("tpad", fe))               # narrator chain frozen

# Newscast: slides box overlaid last at its corner.
fn <- compost:::.layout_filter(compost:::.layout_slots(newscast), 30,
                               "visual")
expect_true(grepl("overlay=1284:60[vout]", fn, fixed = TRUE))

# --- compose_layout argument validation --------------------------------------

dir <- tempfile("layouttest")
dir.create(dir)
va <- file.path(dir, "v.mp4")
na <- file.path(dir, "n.mp4")
file.create(va, na)
expect_error(compose_layout(c(visual = va), file.path(dir, "o.mp4"),
                            vertical), "no source for slot")
expect_error(compose_layout(c(visual = va, narrator = na),
                            file.path(dir, "o.mp4"), vertical,
                            reference = "nope"), "not a source")
unlink(dir, recursive = TRUE)

# --- rotio round-trip: coercion + slot paint order ---------------------------

tl <- rotio::Timeline("lt")
rotio::metadata(tl) <- list(cornball = list(layout = explainer))
tl2 <- rotio::from_json_string(rotio::to_json_string(tl))
lay2 <- rotio::metadata(tl2)$cornball$layout
vs3 <- compost:::.layout_slots(lay2)
expect_equal(names(vs3$slots), c("visual", "narrator")) # paint order kept
expect_equal(vs3$slots$narrator$rect, c(1476L, 636L, 384L, 384L))

# --- real composes -----------------------------------------------------------

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    dir <- tempfile("layoutrender")
    dir.create(dir)
    mk <- function(name, col, secs, rate) {
        f <- file.path(dir, name)
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                                    sprintf("color=c=%s:s=640x360:d=%d:r=%d",
                                            col, secs, rate),
                                    "-c:v", "libx264", "-pix_fmt", "yuv420p",
                                    f)), stdout = FALSE, stderr = FALSE)
        f
    }
    head <- mk("head.mp4", "red", 2, 24)     # short + 24fps: freeze + resample
    slides <- mk("slides.mp4", "blue", 3, 30)

    channel <- function(hex, i) {
        strtoi(substr(hex, 3 + 2 * (i - 1), 4 + 2 * (i - 1)), 16L)
    }
    sample_at <- function(video, t, x, y) {
        png <- tempfile(fileext = ".png")
        on.exit(unlink(png))
        frame_export(video, t, png)
        compost:::.sample_corner_color(png, x, y)
    }

    out <- file.path(dir, "vertical.mp4")
    compose_layout(c(narrator = head, visual = slides), out, vertical,
                   reference = "visual")
    expect_equal(probe(out, "width"), 1080)
    expect_equal(probe(out, "height"), 1920)
    expect_equal(as.integer(probe(out, "nb_frames")), 90L) # 3s @ visual 30fps
    # Late frame: narrator slot still red (frozen past its 2s), visual blue.
    top <- sample_at(out, 2.5, 540, 480)
    bot <- sample_at(out, 2.5, 540, 1440)
    expect_true(channel(top, 1) > 200 && channel(top, 3) < 80)
    expect_true(channel(bot, 3) > 200 && channel(bot, 1) < 80)

    oute <- file.path(dir, "explainer.mp4")
    compose_layout(c(visual = slides, narrator = head), oute, explainer,
                   reference = "visual")
    expect_equal(probe(oute, "width"), 1920)
    # PiP center is red (painted over the blue visual), far corner blue.
    pip <- sample_at(oute, 1, 1476L + 192L, 636L + 192L)
    corner <- sample_at(oute, 1, 200, 200)
    expect_true(channel(pip, 1) > 200)
    expect_true(channel(corner, 3) > 200)

    unlink(dir, recursive = TRUE)
}
