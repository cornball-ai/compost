# execute_plan(): the refusal table from cornductor's composition-plan
# spec, dry-run lowering, and (at_home) real executions with pixel and
# frame-count assertions.

library(compost)

mdir <- tempfile("planmedia")
dir.create(file.path(mdir, "media"), recursive = TRUE)
touch <- function(name) {
    f <- file.path(mdir, name)
    file.create(f)
    name
}
vfile <- touch("media/a.mp4")
vfile2 <- touch("media/b.mp4")
afile <- touch("media/n.wav")

vrow <- function(layer = 1L, media = vfile, kind = "video", start = 0L,
                 duration = 45L, source_in = 0L, dest = c(0L, 0L, 640L, 360L),
                 opacity = 1) {
    data.frame(layer = layer, asset_id = "a1", media = media, kind = kind,
               start = start, duration = duration, source_in = source_in,
               dest_x = dest[1], dest_y = dest[2], dest_w = dest[3],
               dest_h = dest[4], opacity = opacity, stringsAsFactors = FALSE)
}
no_tx <- data.frame(layer = integer(0), at = integer(0),
                    duration = integer(0), type = character(0),
                    stringsAsFactors = FALSE)
no_aud <- data.frame(asset_id = character(0), media = character(0),
                     start = integer(0), duration = integer(0),
                     source_in = integer(0), stringsAsFactors = FALSE)
no_cap <- data.frame(asset_id = character(0), media = character(0),
                     stringsAsFactors = FALSE)
mkplan <- function(video = vrow(), transitions = no_tx, audio = no_aud,
                   captions = no_cap, duration = 75L, canvas = c(640L, 360L),
                   fps = "30/1", tick_rate = 30L, schema = 1L) {
    list(schema = schema, canvas = canvas, fps = fps, tick_rate = tick_rate,
         duration = duration, video = video, transitions = transitions,
         audio = audio, captions = captions)
}
run <- function(plan, ...) {
    execute_plan(plan, tempfile(fileext = ".mp4"), media_dir = mdir, ...)
}

# --- refusal table ----------------------------------------------------

expect_error(run(mkplan(schema = 2L)), "newer")
expect_error(run(mkplan(canvas = c(641L, 360L))), "even")
expect_error(run(mkplan(canvas = c(640.5, 360))), "even")
expect_error(run(mkplan(fps = "30")), "rational")
expect_error(run(mkplan(duration = 0L)), "positive")
expect_error(execute_plan(mkplan()[-9], tempfile()), "required field")
expect_error(run(mkplan(video = vrow()[, -12])), "required column")

# media bindings
expect_error(run(mkplan(video = vrow(media = NA_character_))),
             "viewer plan")
expect_error(run(mkplan(video = vrow(media = file.path(mdir, vfile)))),
             "absolute")
expect_error(execute_plan(mkplan(), tempfile()), "no media_dir")
expect_error(run(mkplan(video = vrow(media = "media/missing.mp4"))),
             "does not exist")

# rows
expect_error(run(mkplan(video = vrow(start = -1L))), "negative")
expect_error(run(mkplan(video = vrow(duration = 0L))), "positive")
expect_error(run(mkplan(video = vrow(start = 1.5))), "integer count")
expect_error(run(mkplan(video = vrow(opacity = 1.5))), "0, 1")
expect_error(run(mkplan(video = vrow(kind = "image", source_in = 15L))),
             "image row")
expect_error(run(mkplan(video = rbind(vrow(start = 0L, duration = 45L),
                                      vrow(start = 30L, duration = 30L)))),
             "overlapping")

# transitions
two <- rbind(vrow(start = 0L, duration = 45L),
             vrow(media = vfile2, start = 45L, duration = 30L,
                  source_in = 15L))
tx <- function(at = 45L, duration = 9L, type = "dissolve", layer = 1L) {
    data.frame(layer = layer, at = at, duration = duration, type = type,
               stringsAsFactors = FALSE)
}
expect_error(run(mkplan(video = two, transitions = tx(type = "wipe"))),
             "dissolve")
expect_error(run(mkplan(video = two, transitions = tx(at = 40L))),
             "not a cut")
expect_error(run(mkplan(video = two, transitions = tx(duration = 20L))),
             "head handle")
expect_error(run(mkplan(video = two,
                        transitions = tx(duration = 40L))),
             "longer|head handle")

# captions must be subtitle files
expect_error(run(mkplan(captions = data.frame(asset_id = "c", media = vfile,
                                              stringsAsFactors = FALSE))),
             "ass or")

# a relative path may not climb out of media_dir
outside <- file.path(dirname(mdir), "escapee.mp4")
file.create(outside)
expect_error(run(mkplan(video = vrow(media = "../escapee.mp4"))),
             "escapes")
unlink(outside)

# rows past the plan duration refuse; the executor never silently
# truncates plan content at the background end
expect_error(run(mkplan(video = vrow(start = 60L, duration = 45L))),
             "past the plan duration")
expect_error(run(mkplan(audio = data.frame(asset_id = "n", media = afile,
                                           start = 0L, duration = 90L,
                                           source_in = 0L,
                                           stringsAsFactors = FALSE))),
             "past the plan duration")

# audio placement is sample-accurate: 15 ticks at 30/s on the 44.1kHz
# grid is exactly 22050 samples, not a rounded millisecond count
aud1 <- data.frame(asset_id = "n", media = afile, start = 15L,
                   duration = 30L, source_in = 0L, stringsAsFactors = FALSE)
cmd_a <- run(mkplan(audio = aud1), dry_run = TRUE)
expect_true(grepl("adelay=22050S:all=1", cmd_a, fixed = TRUE))

# the plan duration must map to a whole positive frame count: 5 ticks
# at 90/s is 1.67 frames at 30fps, and 1 tick rounds below one frame
expect_error(run(mkplan(duration = 5L, tick_rate = 90L,
                        video = vrow(duration = 5L))), "frame")
expect_error(run(mkplan(duration = 1L, tick_rate = 90L,
                        video = vrow(duration = 1L))), "frame")
# ...while 3 ticks at 90/s is exactly 1 frame
cmd_f <- run(mkplan(duration = 3L, tick_rate = 90L,
                    video = vrow(duration = 3L)), dry_run = TRUE)
expect_true(grepl("-frames:v 1", cmd_f, fixed = TRUE))

# --- dry-run lowering -------------------------------------------------

cmd <- run(mkplan(video = two, transitions = tx()), dry_run = TRUE)
expect_true(grepl("-filter_complex", cmd, fixed = TRUE))
expect_true(grepl("-frames:v 75", cmd, fixed = TRUE))
expect_true(grepl("fade=t=in", cmd, fixed = TRUE)) # the dissolve handle
expect_true(grepl("color=c=black:s=640x360:r=30/1", cmd, fixed = TRUE))
# head-fed: the handle input seeks source_in - duration = 0.2s into b
expect_true(grepl("-ss 0.200000 -t 0.300000", cmd, fixed = TRUE))

# --- real executions --------------------------------------------------

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    mk <- function(name, col, secs, rate) {
        f <- file.path(mdir, "media", name)
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                                    sprintf("color=c=%s:s=640x360:d=%d:r=%d",
                                            col, secs, rate),
                                    "-c:v", "libx264", "-pix_fmt", "yuv420p",
                                    f)), stdout = FALSE, stderr = FALSE)
        file.path("media", name)
    }
    mka <- function(name, secs) {
        f <- file.path(mdir, "media", name)
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                                    sprintf("sine=frequency=440:duration=%d",
                                            secs), f)),
                stdout = FALSE, stderr = FALSE)
        file.path("media", name)
    }
    red <- mk("red.mp4", "red", 2, 30)
    blue <- mk("blue.mp4", "blue", 2, 30)
    sine <- mka("sine.wav", 3)

    channel <- function(hex, i) {
        strtoi(substr(hex, 3 + 2 * (i - 1), 4 + 2 * (i - 1)), 16L)
    }
    sample_at <- function(video, t, x, y) {
        png <- tempfile(fileext = ".png")
        on.exit(unlink(png))
        frame_export(video, t, png)
        compost:::.sample_corner_color(png, x, y)
    }

    plan <- mkplan(
        video = rbind(
            vrow(media = red, start = 0L, duration = 45L),
            vrow(media = blue, start = 45L, duration = 30L,
                 source_in = 15L),
            vrow(layer = 2L, media = blue, start = 15L, duration = 15L,
                 dest = c(10L, 10L, 160L, 90L))),
        transitions = tx(at = 45L, duration = 9L),
        audio = data.frame(asset_id = "n1", media = sine, start = 0L,
                           duration = 75L, source_in = 0L,
                           stringsAsFactors = FALSE))
    out <- file.path(mdir, "out.mp4")
    execute_plan(plan, out, media_dir = mdir)

    expect_equal(probe(out, "width"), 640)
    expect_equal(probe(out, "height"), 360)
    expect_equal(as.integer(probe(out, "nb_frames")), 75L)
    # base layer: red early, blue late
    c1 <- sample_at(out, 0.5, 600, 300)
    expect_true(channel(c1, 1) > 200 && channel(c1, 3) < 80)
    c2 <- sample_at(out, 2.2, 600, 300)
    expect_true(channel(c2, 3) > 200 && channel(c2, 1) < 80)
    # the layer-2 box paints blue over red inside its window
    c3 <- sample_at(out, 0.7, 40, 40)
    expect_true(channel(c3, 3) > 200 && channel(c3, 1) < 80)
    # ...and is gone outside it
    c4 <- sample_at(out, 1.2, 40, 40)
    expect_true(channel(c4, 1) > 200)
    # audio present
    astreams <- system2("ffprobe", shQuote(c("-v", "error",
                "-select_streams", "a", "-show_entries",
                "stream=codec_type", "-of", "csv=p=0", out)),
        stdout = TRUE)
    expect_equal(astreams, "audio")

    # captions burn: a minimal ASS file renders without error
    ass <- file.path(mdir, "media", "c.ass")
    writeLines(c("[Script Info]", "PlayResX: 640", "PlayResY: 360", "",
                 "[V4+ Styles]",
                 paste0("Style: Default,DejaVu Sans,24,&H00FFFFFF,",
                        "&H000000FF,&H00000000,&H00000000,0,0,0,0,",
                        "100,100,0,0,1,2,0,2,10,10,10,1"), "",
                 "[Events]",
                 "Dialogue: 0,0:00:00.00,0:00:02.00,Default,,0,0,0,,hello"),
               ass)
    plan2 <- mkplan(video = vrow(media = red, duration = 60L),
                    duration = 60L,
                    captions = data.frame(asset_id = "c1",
                                          media = "media/c.ass",
                                          stringsAsFactors = FALSE))
    out2 <- file.path(mdir, "cap.mp4")
    execute_plan(plan2, out2, media_dir = mdir)
    expect_equal(as.integer(probe(out2, "nb_frames")), 60L)
}

unlink(mdir, recursive = TRUE)
