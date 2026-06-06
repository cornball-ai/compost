# OTIO -> ffmpeg lowering: framing/caption helpers and command construction.

library(rotio)

# --- framing filter chain ---------------------------------------------------

# Shorts: scale to a 1080 box, pad to 1080x1920 anchored top-left.
vf <- compost:::.framing_vf(list(scale = 1080, pad = c(1080, 1920),
                                 pos = c(0, 0)))
expect_equal(vf[1], "scale=1080:1080:force_original_aspect_ratio=decrease")
expect_equal(vf[2], "pad=1080:1920:0:0:black")

# Square: centered pad via ffmpeg expressions, default pos is top-left.
vf_sq <- compost:::.framing_vf(list(scale = 1080, pad = c(1080, 1080),
                                    pos = c("(ow-iw)/2", "(oh-ih)/2")))
expect_equal(vf_sq[2], "pad=1080:1080:(ow-iw)/2:(oh-ih)/2:black")

vf_default <- compost:::.framing_vf(list(pad = c(1080, 1080)))
expect_equal(vf_default[1], "pad=1080:1080:0:0:black")

# No framing -> empty chain.
expect_equal(length(compost:::.framing_vf(NULL)), 0L)

# --- subtitles filter escaping ----------------------------------------------

expect_equal(compost:::.subtitles_filter("/a/b/captions.ass"),
             "subtitles='/a/b/captions.ass'")
# Windows-style colon gets escaped for the subtitles filter.
expect_true(grepl("C\\\\:", compost:::.subtitles_filter("C:/x/captions.ass")))

# --- caption-track detection ------------------------------------------------

cap_by_role <- Track("anything", kind = "Video")
metadata(cap_by_role) <- list(cornball = list(role = "caption"))
expect_true(compost:::.is_caption_track(cap_by_role))

expect_true(compost:::.is_caption_track(Track("captions", kind = "Video")))
expect_false(compost:::.is_caption_track(Track("V1", kind = "Video")))

# --- full render command (dry run) ------------------------------------------

dir <- tempfile("rttest")
dir.create(dir)
file.create(file.path(dir, "primary.mp4"))
file.create(file.path(dir, "captions.ass"))

tl <- Timeline("t41")
v <- Track("V1", kind = "Video")
ref <- ExternalReference("primary.mp4")
metadata(ref) <- list(cornball = list(
  framing = list(scale = 1080, pad = c(1080, 1920), pos = c(0, 0))
))
append_child(v, Clip("primary", ref,
                     source_range = TimeRange(RationalTime(0, 30),
                                              RationalTime(300, 30))))
cap <- Track("captions", kind = "Video")
metadata(cap) <- list(cornball = list(role = "caption"))
append_child(cap, Clip("cap", ExternalReference("captions.ass"),
                       source_range = TimeRange(RationalTime(0, 30),
                                                RationalTime(300, 30))))
append_child(tracks(tl), v)
append_child(tracks(tl), cap)

cmd <- render_timeline(tl, file.path(dir, "video.mp4"),
                       media_dir = dir, dry_run = TRUE)

# Single encode pass carrying scale, pad, and the subtitle burn together.
expect_true(grepl("scale=1080:1080:force_original_aspect_ratio=decrease", cmd))
expect_true(grepl("pad=1080:1920:0:0:black", cmd))
expect_true(grepl("subtitles=", cmd))
expect_true(grepl("-c:v libx264", cmd))
expect_true(grepl("-movflags \\+faststart", cmd))
# No separate audio track -> audio comes from the video itself, optionally.
expect_true(grepl("-map 0:a:0\\?", cmd))

# A missing video track is an error.
empty <- Timeline("empty")
expect_error(render_timeline(empty, file.path(dir, "out.mp4"), dry_run = TRUE),
             "no video track")
