library(tinytest)
library(compost)

if (tinytest::at_home() && nzchar(Sys.which("ffmpeg"))) local({
    root <- tempfile("ntsc-cut-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    for (color in c("red", "blue")) {
        compost:::.run_ffmpeg(c("-nostdin", "-y", "-f", "lavfi", "-i",
            paste0("color=", color, ":s=64x48"), "-frames:v", "1",
            file.path(root, paste0(color, ".png"))))
    }
    p <- list(schema = 1L, canvas = c(64L, 48L), fps = "30000/1001",
        tick_rate = 30000L, duration = 420420L,
        video = data.frame(layer = c(1L, 2L), asset_id = c("red", "blue"),
            media = c("red.png", "blue.png"), kind = "image",
            start = c(0L, 360360L), duration = c(420420L, 30030L), source_in = 0L,
            dest_x = 0L, dest_y = 0L, dest_w = 64L, dest_h = 48L, opacity = 1),
        audio = data.frame(asset_id = character(), media = character(), start = integer(),
            duration = integer(), source_in = integer()),
        transitions = data.frame(layer = integer(), at = integer(), duration = integer(), type = character()),
        captions = data.frame(asset_id = character(), media = character()))
    out <- file.path(root, "cut.mp4")
    execute_plan(p, out, media_dir = root)
    raw <- file.path(root, "cut.rgb")
    compost:::.run_ffmpeg(c("-nostdin", "-y", "-i", out, "-vf",
        "select=eq(n\\,359)+eq(n\\,360)+eq(n\\,390)", "-fps_mode", "passthrough",
        "-frames:v", "3", "-pix_fmt", "rgb24", "-f", "rawvideo", raw))
    rgb <- as.integer(readBin(raw, "raw", 64 * 48 * 3 * 3))
    expect_equal(length(rgb), 64L * 48L * 3L * 3L)
    frames <- matrix(rgb, ncol = 3)
    expect_true(mean(frames[seq(1, nrow(frames), by = 3), 1]) > 200,
                info = "upper image must not appear before frame 360")
    expect_true(mean(frames[seq(3, nrow(frames), by = 3), 2]) > 200,
                info = "upper image must appear on frame 360, not frame 361")
    expect_true(mean(frames[seq(1, nrow(frames), by = 3), 3]) > 200,
                info = "upper image must be absent at its exclusive end")
})
