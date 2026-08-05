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

# Real burn-in: the escaping only matters if ffmpeg actually accepts it. On
# Windows the srt sits at a drive path (C:\...) whose colon must be escaped for
# the filtergraph; a string test can't prove ffmpeg swallows it. This burns a
# temp-path srt onto a solid color and checks the graph was accepted. (This is
# the check that would have caught the ametadata file= path bug had rms had one.)
if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    srt <- tempfile(fileext = ".srt")
    writeLines(c("1", "00:00:00,000 --> 00:00:01,000", "HELLO", ""), srt)
    outsub <- tempfile(fileext = ".mp4")
    st <- system2("ffmpeg", shQuote(c("-y", "-f", "lavfi",
                                      "-i", "color=c=black:s=320x240:d=1",
                                      "-vf", compost:::.subtitles_filter(srt), outsub)),
                  stdout = FALSE, stderr = FALSE)
    expect_true(is.null(attr(st, "status")) || attr(st, "status") == 0L)
    expect_true(file.exists(outsub) && file.size(outsub) > 0)
    unlink(c(srt, outsub))
}

# --- media url resolution (platform-agnostic) -------------------------------

# Absolute urls pass through untouched; media_dir is only for relative ones.
# These must hold on every OS: a bundle authored on one platform can render on
# another, so the absolute-path test can't lean on .Platform$file.sep.
expect_true(compost:::.is_absolute_path("/tmp/v.mp4"))       # POSIX
expect_true(compost:::.is_absolute_path("C:\\media\\v.mp4")) # Windows drive, backslash
expect_true(compost:::.is_absolute_path("C:/media/v.mp4"))   # Windows drive, forward slash
expect_true(compost:::.is_absolute_path("\\\\srv\\share\\v.mp4")) # UNC
expect_false(compost:::.is_absolute_path("v.mp4"))
expect_false(compost:::.is_absolute_path("sub/v.mp4"))

# An absolute url is returned as-is, not joined onto media_dir (the bug that
# doubled the path on Windows: media_dir + "C:\..." -> "media_dir/C:\...").
expect_equal(compost:::.resolve_media("/abs/v.mp4", "/base"), "/abs/v.mp4")
expect_equal(compost:::.resolve_media("C:/abs/v.mp4", "/base"), "C:/abs/v.mp4")
expect_equal(compost:::.resolve_media("C:\\abs\\v.mp4", "/base"), "C:\\abs\\v.mp4")
# A relative url is joined onto media_dir.
expect_equal(compost:::.resolve_media("v.mp4", "/base"), "/base/v.mp4")
# No media_dir: url returned unchanged.
expect_equal(compost:::.resolve_media("v.mp4", NULL), "v.mp4")

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

# --- layout lowering: degradation paths --------------------------------------

lay_v <- list(schema = 1L, name = "vertical", canvas = c(1080L, 1920L),
              slots = list(
                  narrator = list(rect = c(0L, 0L, 1080L, 960L),
                                  fit = "fill"),
                  visual = list(rect = c(0L, 960L, 1080L, 960L),
                                fit = "fit")))
ldir <- tempfile("laydeg")
dir.create(ldir)
lv <- file.path(ldir, "v.mp4")
file.create(lv)

# A layout whose narrator slot has no track: warns, single-stream render.
tl_l1 <- Timeline("lay1")
metadata(tl_l1) <- list(cornball = list(layout = lay_v))
vt1 <- Track("V1", kind = "Video")
metadata(vt1) <- list(cornball = list(role = "visual"))
append_child(vt1, Clip("c", ExternalReference(lv)))
append_child(tracks(tl_l1), vt1)
expect_warning(
        cmd_l1 <- render_timeline(tl_l1, file.path(ldir, "o1.mp4"),
                                  media_dir = ldir, dry_run = TRUE),
        "no matching track")
expect_true(grepl("^ffmpeg", cmd_l1))

# A roled track without any layout: warns, track ignored, content renders.
tl_l2 <- Timeline("lay2")
vt2 <- Track("V1", kind = "Video")
append_child(vt2, Clip("c", ExternalReference(lv)))
nt2 <- Track("narrator", kind = "Video")
metadata(nt2) <- list(cornball = list(role = "narrator"))
append_child(nt2, Clip("n", ExternalReference(lv)))
append_child(tracks(tl_l2), vt2)
append_child(tracks(tl_l2), nt2)
expect_warning(
        cmd_l2 <- render_timeline(tl_l2, file.path(ldir, "o2.mp4"),
                                  media_dir = ldir, dry_run = TRUE),
        "no layout slot")
expect_true(grepl("^ffmpeg", cmd_l2))

unlink(ldir, recursive = TRUE)

# --- layout lowering: full compose (at_home) ---------------------------------
if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    ldir <- tempfile("layfull")
    dir.create(ldir)
    mklv <- function(name, col, secs, rate) {
        f <- file.path(ldir, name)
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                                    sprintf("color=c=%s:s=640x360:d=%d:r=%d",
                                            col, secs, rate),
                                    "-c:v", "libx264", "-pix_fmt", "yuv420p",
                                    f)), stdout = FALSE, stderr = FALSE)
        f
    }
    lvis <- mklv("vis.mp4", "blue", 3, 30)
    lnar <- mklv("nar.mp4", "red", 2, 24)
    lbed <- file.path(ldir, "bed.mp3")
    system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                                "sine=frequency=440:duration=4", "-ar",
                                "48000", "-ac", "1", lbed)),
            stdout = FALSE, stderr = FALSE)

    tl_l3 <- Timeline("lay3")
    metadata(tl_l3) <- list(cornball = list(layout = lay_v))
    vt3 <- Track("V1", kind = "Video")
    metadata(vt3) <- list(cornball = list(role = "visual"))
    append_child(vt3, Clip("c", ExternalReference(lvis),
                           source_range = TimeRange(RationalTime(0, 30),
                                                    RationalTime(90, 30))))
    nt3 <- Track("narrator", kind = "Video")
    metadata(nt3) <- list(cornball = list(role = "narrator"))
    append_child(nt3, Clip("n", ExternalReference(lnar)))
    at3 <- Track("A1", kind = "Audio")
    append_child(at3, Clip("audio", ExternalReference(lbed),
                           source_range = TimeRange(RationalTime(0, 30),
                                                    RationalTime(120, 30))))
    append_child(tracks(tl_l3), vt3)
    append_child(tracks(tl_l3), nt3)
    append_child(tracks(tl_l3), at3)

    outl <- file.path(ldir, "out.mp4")
    render_timeline(tl_l3, outl)
    expect_equal(probe(outl, "width"), 1080)
    expect_equal(probe(outl, "height"), 1920)
    adur3 <- as.numeric(probe(lbed, "duration"))
    expect_equal(as.integer(probe(outl, "nb_frames")),
                 as.integer(round(adur3 * 30)))

    unlink(ldir, recursive = TRUE)
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

# --- proxy scaling ----------------------------------------------------------
# A preview render is the full-size composition written at fewer pixels,
# not a differently composed one, so the downscale is the LAST filter.
cmd_s <- render_timeline(tl, file.path(dir, "small.mp4"), media_dir = dir,
                         dry_run = TRUE, scale = 0.5)
expect_true(grepl("scale=trunc(iw*0.500000/2)*2:trunc(ih*0.500000/2)*2", cmd_s,
                  fixed = TRUE))
# Even dimensions, because yuv420p requires them.
expect_true(grepl("trunc(", cmd_s, fixed = TRUE))
# It comes after framing and after the caption burn: captions are burned at
# design size and then shrunk, which is what they look like on a phone.
vf_s <- sub(".*-vf ", "", cmd_s)
vf_s <- sub(" -map.*", "", vf_s)
expect_true(regexpr("subtitles=", vf_s, fixed = TRUE) <
            regexpr("scale=trunc", vf_s, fixed = TRUE))
expect_true(regexpr("force_original_aspect_ratio", vf_s, fixed = TRUE) <
            regexpr("scale=trunc", vf_s, fixed = TRUE))

# scale = 1 is the default and adds nothing at all.
expect_false(grepl("scale=trunc", cmd, fixed = TRUE))
cmd_one <- render_timeline(tl, file.path(dir, "one.mp4"), media_dir = dir,
                           dry_run = TRUE, scale = 1)
expect_false(grepl("scale=trunc", cmd_one, fixed = TRUE))

# Refusals, so a typo cannot silently render at full size or at nothing.
for (bad in list(0, -0.5, 1.5, NA_real_, Inf, c(0.5, 0.5), "half", NULL)) {
    expect_error(render_timeline(tl, file.path(dir, "b.mp4"), media_dir = dir,
                                 dry_run = TRUE, scale = bad),
                 "scale must be a single number")
}

# Scaling forces a re-encode even when nothing else would have: -c:v copy
# cannot resize.
tl_p <- Timeline("plain")
v_p <- Track("v", kind = "Video")
append_child(v_p, Clip("a", ExternalReference("primary.mp4"),
                       source_range = TimeRange(RationalTime(0, 30),
                                                RationalTime(60, 30))))
append_child(tracks(tl_p), v_p)
cmd_p <- render_timeline(tl_p, file.path(dir, "p.mp4"), media_dir = dir,
                         dry_run = TRUE)
cmd_ps <- render_timeline(tl_p, file.path(dir, "ps.mp4"), media_dir = dir,
                          dry_run = TRUE, scale = 0.25)
expect_true(grepl("-c:v copy", cmd_p, fixed = TRUE))
expect_true(grepl("-c:v libx264", cmd_ps, fixed = TRUE))
expect_false(grepl("-c:v copy", cmd_ps, fixed = TRUE))

# --- a range starting at 0 is not thereby the whole file ---------------------
# Legacy timelines wrote nominal [0, dur] over whole-file media, and taking
# a zero start as "no trim" was how they were tolerated. A real edit writes
# [0, dur] whenever it wants the first dur seconds and no more, which is
# every take trimmed from its own first frame. Guessing from the start time
# played the whole source and the output ran longer than the timeline.
if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    src <- tempfile(fileext = ".mp4")
    system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi",
                                "-i", "testsrc2=duration=4:size=128x128:rate=30",
                                "-c:v", "libx264", "-qp", "0",
                                "-pix_fmt", "yuv420p", src)),
            stdout = FALSE, stderr = FALSE)

    head_tl <- Timeline("headtrim")
    hv <- Track("V1", kind = "Video")
    append_child(hv, Clip("first second", ExternalReference(src),
                          source_range = TimeRange(RationalTime(0, 30),
                                                   RationalTime(30, 30))))
    append_child(tracks(head_tl), hv)
    outh <- tempfile(fileext = ".mp4")
    render_timeline(head_tl, outh)
    expect_true(abs(as.numeric(probe(outh, "duration")) - 1) < 0.15)

    # Two head-trimmed clips off the same source concatenate to their own
    # durations, not to two whole files. This is the shape an A/B edit
    # makes and what was silently running long.
    two <- Timeline("twoheads")
    tv <- Track("V1", kind = "Video")
    append_child(tv, Clip("a", ExternalReference(src),
                          source_range = TimeRange(RationalTime(0, 30),
                                                   RationalTime(30, 30))))
    append_child(tv, Clip("b", ExternalReference(src),
                          source_range = TimeRange(RationalTime(60, 30),
                                                   RationalTime(30, 30))))
    append_child(tracks(two), tv)
    out2 <- tempfile(fileext = ".mp4")
    render_timeline(two, out2)
    expect_true(abs(as.numeric(probe(out2, "duration")) - 2) < 0.2)

    # A range that really does cover the file still takes the cheap
    # whole-file path: trimming there is a no-op, so nothing changes.
    whole <- Timeline("whole")
    wv <- Track("V1", kind = "Video")
    append_child(wv, Clip("all", ExternalReference(src),
                          source_range = TimeRange(RationalTime(0, 30),
                                                   RationalTime(120, 30))))
    append_child(tracks(whole), wv)
    sq <- compost:::.video_sequence(wv, NULL)
    expect_true(is.null(sq$windows[[1]]))
    # ... while the short one gets a window.
    sq2 <- compost:::.video_sequence(hv, NULL)
    expect_equal(sq2$windows[[1]], c(0, 1))
    unlink(c(src, outh, out2))
}

# --- Gaps occupy time ---------------------------------------------------------
# A Gap was skipped as "not a Clip", which does not ignore an empty region:
# it deletes time and slides every later clip earlier.

# The walk collects them without needing media: leading, internal, trailing.
gtrk <- function(...) {
    tr <- Track("V1", kind = "Video")
    for (kd in list(...)) append_child(tr, kd)
    tr
}
gclip <- function(nm, s = 0, d = 2) {
    Clip(nm, ExternalReference("g.mp4"),
         source_range = TimeRange(RationalTime(s * 30, 30),
                                  RationalTime(d * 30, 30)))
}
# gaps is one longer than files: before each clip, then the trailing blank.
sq_lead <- compost:::.video_sequence(gtrk(Gap(RationalTime(30, 30)),
                                          gclip("a")), NULL)
expect_equal(sq_lead$gaps, c(1, 0))
sq_mid <- compost:::.video_sequence(gtrk(gclip("a"), Gap(RationalTime(60, 30)),
                                         gclip("b")), NULL)
expect_equal(sq_mid$gaps, c(0, 2, 0))
sq_tail <- compost:::.video_sequence(gtrk(gclip("a"),
                                          Gap(RationalTime(45, 30))), NULL)
expect_equal(sq_tail$gaps, c(0, 1.5))
# Adjacent gaps accumulate rather than the last one winning.
sq_two <- compost:::.video_sequence(gtrk(Gap(RationalTime(30, 30)),
                                         Gap(RationalTime(30, 30)),
                                         gclip("a")), NULL)
expect_equal(sq_two$gaps, c(2, 0))
# No gaps at all still reports the (zero) trailing entry, so callers never
# index past the end.
sq_none <- compost:::.video_sequence(gtrk(gclip("a"), gclip("b")), NULL)
expect_equal(sq_none$gaps, c(0, 0, 0))

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    solid <- function(dur, color) {
        f <- tempfile(fileext = ".mp4")
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-f", "lavfi", "-i",
                    sprintf("color=c=%s:s=128x128:r=30:d=%s", color, dur),
                    "-c:v", "libx264", "-qp", "0", "-pix_fmt", "yuv420p", f)),
                stdout = FALSE, stderr = FALSE)
        f
    }
    # Mean RGB of one frame, read as raw bytes: no image package, and
    # structural rather than exact because the frame has been through x264.
    frame_rgb <- function(video, t) {
        rf <- tempfile(fileext = ".raw")
        on.exit(unlink(rf), add = TRUE)
        system2("ffmpeg", shQuote(c("-nostdin", "-y", "-ss", format(t),
                    "-i", video, "-frames:v", "1", "-pix_fmt", "rgb24",
                    "-f", "rawvideo", rf)), stdout = FALSE, stderr = FALSE)
        v <- as.integer(readBin(rf, "raw", file.size(rf)))
        c(mean(v[seq(1, length(v), 3)]), mean(v[seq(2, length(v), 3)]),
          mean(v[seq(3, length(v), 3)]))
    }
    gred <- solid(3, "red")
    ggrn <- solid(3, "green")
    gsrc <- function(f, d = 2) {
        Clip("c", ExternalReference(f),
             source_range = TimeRange(RationalTime(0, 30),
                                      RationalTime(d * 30, 30)))
    }
    grender <- function(...) {
        tl <- Timeline("gaps")
        append_child(tracks(tl), gtrk(...))
        out <- tempfile(fileext = ".mp4")
        render_timeline(tl, out)
        out
    }
    gdur <- function(f) as.numeric(probe(f, "duration"))

    # Each position fails differently and so is checked on its own. A
    # leading gap shifts the whole track; a trailing one has no successor
    # to reveal that it vanished, so only the duration catches it.
    o_mid <- grender(gsrc(gred), Gap(RationalTime(60, 30)), gsrc(ggrn))
    expect_true(abs(gdur(o_mid) - 6) < 0.15)
    o_lead <- grender(Gap(RationalTime(30, 30)), gsrc(gred))
    expect_true(abs(gdur(o_lead) - 3) < 0.15)
    o_tail <- grender(gsrc(gred), Gap(RationalTime(45, 30)))
    expect_true(abs(gdur(o_tail) - 3.5) < 0.15)
    # Unchanged where there is no gap.
    o_none <- grender(gsrc(gred), gsrc(ggrn))
    expect_true(abs(gdur(o_none) - 4) < 0.15)

    # Duration alone would pass for blank in the WRONG place, so check what
    # is actually on screen. Sampled mid-clip: a frame at a cut boundary is
    # legitimately ambiguous by a frame either way.
    # ffmpeg's "red" is (255,0,0) but its "green" is (0,128,0), so each
    # channel gets its own floor rather than one shared threshold.
    dominant <- function(rgb, ch, floor) rgb[ch] > floor && all(rgb[-ch] < 40)
    expect_true(dominant(frame_rgb(o_mid, 1), 1, 180))   # red
    expect_true(all(frame_rgb(o_mid, 3) < 20))           # the gap is blank
    expect_true(dominant(frame_rgb(o_mid, 5), 2, 90))    # green
    # A leading gap really is at the front, not swallowed.
    expect_true(all(frame_rgb(o_lead, 0.5) < 20))
    expect_true(dominant(frame_rgb(o_lead, 2), 1, 180))
    unlink(c(gred, ggrn, o_mid, o_lead, o_tail, o_none))
}

# A dissolve needs its two clips adjacent; blank between them is not a join.
expect_error(compost:::.splice_gaps(c("a.mp4", "b.mp4"),
                                    list(gaps = c(0, 1, 0),
                                         windows = list(NULL, NULL),
                                         fades = 0.5)),
             "spans a Gap")

# --- REVIEW (#27): a track of nothing but Gap ---------------------------------
# .video_sequence() recorded the gap correctly, but .assemble_track()
# returned NULL on `length(files) == 0` and the duration vanished. That is
# the same silent omission the Gap work exists to remove, so it now either
# materializes or refuses -- never disappears.
sq_only <- compost:::.video_sequence(gtrk(Gap(RationalTime(90, 30))), NULL)
expect_equal(length(sq_only$files), 0L)
expect_equal(sq_only$gaps, 3)
# The Gap's duration is rational, so the rate is read rather than invented.
expect_equal(sq_only$gap_fps, 30)
sq_empty <- compost:::.video_sequence(gtrk(), NULL)
expect_equal(sum(sq_empty$gaps), 0)

# No frame size to render it at, and none is invented.
expect_error(compost:::.assemble_track(gtrk(Gap(RationalTime(90, 30))),
                                       NULL, NULL),
             "no frame size")
expect_error(compost:::.assemble_track(gtrk(Gap(RationalTime(90, 30))),
                                       NULL, list(scale = 640)),
             "no frame size")
# A truly empty track is still nothing, not an error.
expect_null(compost:::.assemble_track(gtrk(), NULL, NULL))
expect_null(compost:::.assemble_track(gtrk(), NULL, list(pad = c(64, 64))))

if (at_home() && nzchar(Sys.which("ffmpeg"))) {
    # With a canvas it renders, and its duration survives.
    only <- compost:::.assemble_track(gtrk(Gap(RationalTime(90, 30))), NULL,
                                      list(pad = c(64, 64)))
    expect_false(is.null(only))
    expect_true(abs(as.numeric(probe(only$file, "duration")) - 3) < 0.15)
    expect_equal(as.integer(probe(only$file, "width")), 64L)
    unlink(only$temps)

    # ... and through the full render, where dropping it would have taken
    # three seconds off a timeline that asked for them. With no framing to
    # supply a canvas this refuses loudly rather than rendering short.
    gonly <- Timeline("gaponly")
    append_child(tracks(gonly), gtrk(Gap(RationalTime(90, 30))))
    expect_error(render_timeline(gonly, tempfile(fileext = ".mp4")),
                 "no frame size")
}
