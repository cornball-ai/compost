# Real ffmpeg execution. The dry_run/builder tests can't catch shell-quoting
# regressions (system2 runs via sh -c, so a filtergraph's ; [ ] must survive
# intact). These run at_home() only and need ffmpeg/ffprobe.
library(tinytest)

if (!at_home()) {
  exit_file("execution tests run at_home() only")
}
if (Sys.which("ffmpeg") == "" || Sys.which("ffprobe") == "") {
  exit_file("ffmpeg/ffprobe not available")
}

td <- tempfile("compost_exec")
dir.create(td)
vid   <- file.path(td, "v.mp4")
png   <- file.path(td, "fg.png")
av    <- file.path(td, "av.mp4")

# A test-pattern video, a still foreground, and a clip that actually has audio.
system2("ffmpeg", c("-v", "error", "-y", "-f", "lavfi", "-i",
                    "testsrc=size=640x360:rate=24:duration=1",
                    "-pix_fmt", "yuv420p", vid))
system2("ffmpeg", c("-v", "error", "-y", "-f", "lavfi", "-i",
                    "color=c=green:size=200x200", "-frames:v", "1", png))
system2("ffmpeg", c("-v", "error", "-y", "-f", "lavfi", "-i",
                    "testsrc=size=320x240:rate=24:duration=1", "-f", "lavfi",
                    "-i", "sine=frequency=440:duration=1", "-shortest",
                    "-pix_fmt", "yuv420p", av))
expect_true(file.exists(vid) && file.exists(png) && file.exists(av))

made <- function(f) file.exists(f) && file.size(f) > 0

# pip: the filtergraph has ; and [ ] -- the case the shell would split. Both a
# still->still and video->video path.
o <- file.path(td, "pip.png")
pip(png, png, o, corner = "lower-right")
expect_true(made(o))

o <- file.path(td, "pip.mp4")
pip(vid, vid, o, corner = "upper-right")
expect_true(made(o))
expect_equal(probe(o, "width"), 640) # background dims preserved

# overlay (also a [ ] filtergraph).
o <- file.path(td, "ovl.mp4")
overlay(vid, png, o, x = 10, y = 10)
expect_true(made(o))

# colorkey -> ProRes 4444 alpha.
o <- file.path(td, "keyed.mov")
colorkey(png, o, color = "green")
expect_true(made(o))
expect_equal(probe(o, "codec_name"), "prores")

# frame_export grabs a single still.
o <- file.path(td, "frame.png")
frame_export(vid, 0.5, o)
expect_true(made(o))
expect_equal(probe(o, "width"), 640)

# broadcast_audio runs the full chain on a clip that has audio.
o <- file.path(td, "clean.wav")
broadcast_audio(av, o)
expect_true(made(o))

# A missing input raises rather than returning a phantom path.
expect_error(frame_export(file.path(td, "nope.mp4"), 1, file.path(td, "x.png")))

unlink(td, recursive = TRUE)
