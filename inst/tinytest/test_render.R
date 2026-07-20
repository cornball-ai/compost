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

# --- .motion_from_effects: the cornball.* namespace --------------------------

# The real deserialized shape: a clip's effect round-tripped through JSON.
kb <- Effect("motion", "cornball.kenburns",
             metadata = list(schema = 1L, zoom_from = 1, zoom_to = 1.15,
                             from = c(0.5, 0.5), to = c(0.5, 0.4),
                             ease = "smooth"))
cl <- Clip("s", ExternalReference("s.png"),
           source_range = TimeRange(RationalTime(0, 30), RationalTime(60, 30)))
cl <- add_effect(cl, kb)
cl2 <- from_json_string(to_json_string(cl))
m <- compost:::.motion_from_effects(effects(cl2))
expect_false(is.null(m))
expect_equal(as.numeric(unlist(m$zoom_to)), 1.15)
# ...and the filter builder digests that shape directly.
fkb <- compost:::.kenburns_filter(m, 60L, 1080L, 1920L, 30)
expect_true(grepl("0.150000", fkb, fixed = TRUE))

# Foreign namespaces are ignored silently.
expect_silent(compost:::.motion_from_effects(
        list(Effect("blur", "SomeVendor.GaussianBlur",
                    metadata = list(size = 4)))))
expect_null(compost:::.motion_from_effects(
        list(Effect("blur", "SomeVendor.GaussianBlur"))))

# Unknown cornball.* warns and degrades to static.
expect_warning(
        v <- compost:::.motion_from_effects(
                list(Effect("x", "cornball.wobble"))),
        "unrecognized")
expect_null(v)

# Disabled effects warn and are skipped.
expect_warning(
        v <- compost:::.motion_from_effects(
                list(Effect("m", "cornball.kenburns", enabled = FALSE))),
        "disabled")
expect_null(v)

# A schema newer than supported warns and degrades.
expect_warning(
        v <- compost:::.motion_from_effects(
                list(Effect("m", "cornball.kenburns",
                            metadata = list(schema = 2L)))),
        "schema")
expect_null(v)

# First usable motion wins; extras warn.
expect_warning(
        v <- compost:::.motion_from_effects(list(
                Effect("a", "cornball.kenburns",
                       metadata = list(schema = 1L, zoom_to = 1.1)),
                Effect("b", "cornball.kenburns",
                       metadata = list(schema = 1L, zoom_to = 1.9)))),
        "first")
expect_equal(as.numeric(unlist(v$zoom_to)), 1.1)

# --- still clips need a source_range -----------------------------------------

fake_png <- file.path(dir, "slide.png")
file.create(fake_png)
tl_nosr <- Timeline("nosr")
v_nosr <- Track("V1", kind = "Video")
append_child(v_nosr, Clip("s1", ExternalReference(fake_png)))
append_child(tracks(tl_nosr), v_nosr)
expect_error(render_timeline(tl_nosr, file.path(dir, "o.mp4"), dry_run = TRUE),
             "source_range")

# --- Transition lowering (at_home: real ffmpeg fixtures) ----------------------
if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    master2 <- tempfile(fileext = ".mp4")
    system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi",
                                "-i", "testsrc2=duration=5:size=128x128:rate=24",
                                "-c:v", "libx264", "-qp", "0",
                                "-pix_fmt", "yuv420p", master2)),
            stdout = FALSE, stderr = FALSE)
    sl <- function(from, to) {
        f <- tempfile(fileext = ".mp4")
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-i", master2, "-vf",
                                    sprintf("trim=start_frame=%d:end_frame=%d,setpts=PTS-STARTPTS", from, to),
                                    "-c:v", "libx264", "-qp", "0",
                                    "-pix_fmt", "yuv420p", f)),
                stdout = FALSE, stderr = FALSE)
        f
    }
    a <- sl(0, 48)   # 2s
    b <- sl(39, 96)  # 9-frame replay head + 48 new frames

    tl2 <- Timeline("chained")
    v2 <- Track("V1", kind = "Video")
    append_child(v2, Clip("a", ExternalReference(a),
                          source_range = TimeRange(RationalTime(0, 24),
                                                   RationalTime(48, 24))))
    append_child(v2, Transition(name = "join01",
                                transition_type = "SMPTE_Dissolve",
                                in_offset = RationalTime(9, 24),
                                out_offset = RationalTime(0, 24)))
    append_child(v2, Clip("b", ExternalReference(b),
                          source_range = TimeRange(RationalTime(9, 24),
                                                   RationalTime(48, 24))))
    append_child(tracks(tl2), v2)

    outv <- tempfile(fileext = ".mp4")
    render_timeline(tl2, outv)
    expect_true(file.exists(outv))
    # Played durations sum: 2 + 2 = 4s. The dissolve overlays the cut using
    # b's head handle; transitions never consume timeline time.
    dur <- as.numeric(probe(outv, "duration"))
    expect_true(abs(dur - 4) < 0.1)

    # out_offset > 0 is not lowered
    v3 <- Track("V1", kind = "Video")
    append_child(v3, Clip("a", ExternalReference(a)))
    append_child(v3, Transition(in_offset = RationalTime(0, 24),
                                out_offset = RationalTime(9, 24)))
    append_child(v3, Clip("b", ExternalReference(b)))
    tl3 <- Timeline("bad")
    append_child(tracks(tl3), v3)
    expect_error(render_timeline(tl3, tempfile(fileext = ".mp4")),
                 pattern = "out_offset")

    unlink(c(master2, a, b, outv))
}

# --- still + effect + sequence lowering (at_home: real ffmpeg) ---------------
if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    dirS <- tempfile("slides")
    dir.create(dirS)
    mkpng <- function(name, col) {
        f <- file.path(dirS, name)
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                                    sprintf("color=c=%s:s=320x240", col),
                                    "-frames:v", "1", f)),
                stdout = FALSE, stderr = FALSE)
        f
    }
    png1 <- mkpng("s1.png", "red")
    png2 <- mkpng("s2.png", "blue")
    bed <- file.path(dirS, "bed.mp3")
    system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                                "sine=frequency=440:duration=4", "-ar",
                                "48000", "-ac", "1", bed)),
            stdout = FALSE, stderr = FALSE)

    # Two 2s slide clips at 30fps; the second turns over a 6-frame dissolve
    # riding its head handle; the first carries a Ken Burns effect. The audio
    # bed is the ground truth for the output length.
    tls <- Timeline("slides")
    metadata(tls) <- list(cornball = list(framing = list(
        scale = 240, pad = c(240, 240), pos = c("(ow-iw)/2", "(oh-ih)/2"))))
    vs <- Track("V1", kind = "Video")
    c1 <- Clip("s1", ExternalReference(png1),
               source_range = TimeRange(RationalTime(0, 30),
                                        RationalTime(60, 30)))
    c1 <- add_effect(c1, Effect("motion", "cornball.kenburns",
                                metadata = list(schema = 1L, zoom_from = 1,
                                                zoom_to = 1.2,
                                                from = c(0.5, 0.5),
                                                to = c(0.4, 0.4))))
    append_child(vs, c1)
    append_child(vs, Transition(name = "turn", transition_type = "SMPTE_Dissolve",
                                in_offset = RationalTime(6, 30),
                                out_offset = RationalTime(0, 30)))
    append_child(vs, Clip("s2", ExternalReference(png2),
                          source_range = TimeRange(RationalTime(6, 30),
                                                   RationalTime(60, 30))))
    as_ <- Track("A1", kind = "Audio")
    append_child(as_, Clip("audio", ExternalReference(bed),
                           source_range = TimeRange(RationalTime(0, 30),
                                                    RationalTime(120, 30))))
    append_child(tracks(tls), vs)
    append_child(tracks(tls), as_)

    outs <- file.path(dirS, "slides.mp4")
    render_timeline(tls, outs)
    # Video padded/cut to exactly the bed's duration at 30fps (the mp3
    # container reports encoder padding beyond the 4s sine, so derive the
    # expectation from the probe rather than assuming 120), framed to the box.
    adur <- as.numeric(probe(bed, "duration"))
    expect_equal(as.integer(probe(outs, "nb_frames")),
                 as.integer(round(adur * 30)))
    expect_equal(probe(outs, "width"), 240)
    expect_equal(probe(outs, "height"), 240)

    # An ImageSequenceReference clip lowers through frames_clip().
    dirF <- file.path(dirS, "seq")
    dir.create(dirF)
    system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                                "testsrc2=size=320x240:rate=30", "-frames:v",
                                "10", file.path(dirF, "frame_%04d.png"))),
            stdout = FALSE, stderr = FALSE)
    tli <- Timeline("scene")
    vi <- Track("V1", kind = "Video")
    append_child(vi, Clip("sc", ImageSequenceReference(
            target_url_base = dirF, name_prefix = "frame_",
            name_suffix = ".png", start_frame = 1L, rate = 30,
            frame_zero_padding = 4L)))
    append_child(tracks(tli), vi)
    outi <- file.path(dirS, "scene.mp4")
    render_timeline(tli, outi)
    expect_equal(as.integer(probe(outi, "nb_frames")), 10L)

    unlink(dirS, recursive = TRUE)
}
