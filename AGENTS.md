# compost — role in the cornyverse NLE stack

Where compost sits in relation to nle.api, cornductor, kerNLE, and
blendR. The canonical version of this is
[cornelius/vault/wiki/nle-stack.md](https://github.com/cornball-ai/cornelius/blob/master/vault/wiki/nle-stack.md);
this file is the local copy for agents working inside the compost repo.

## TL;DR

```
cornductor    project intent + characters + GenAI asset metadata
     ↘
      nle.api    neutral sequence model + edit verbs + driver registry
     ↗      ↓
kerNLE       blendR driver  (+ future ffmpeg / OTIO / FCP drivers)
   (UI / server                ↓
    over nle.api)              Blender VSE

compost      concrete ffmpeg composition primitives
             called by kerNLE (render) and by future drivers
             that need to produce a media file
```

## compost owns

- ffmpeg `filter_complex` recipes wrapped as clean R functions.
- The "render this sequence to an mp4 / mov / png" path that goes
  through ffmpeg rather than through Blender VSE.
- Concrete composition primitives:
  - **overlay** — PiP placement, alpha-channel compositing
  - **concat** — joining clips back-to-back
  - **stack** — h/v stacking layouts
  - **probe** — ffprobe wrapper for source dimensions / fps / duration
  - **chromakey / colorkey → ProRes 4444 alpha** _(to add)_ — the
    workflow from `cornelius/vault/wiki/ai-character-overlay.md`
  - **broadcast audio chain** _(to add)_ — highpass + 60/120/180 Hz
    notches + afftdn + acompressor + loudnorm; currently scattered in
    `cornductor/inst/` and `kerNLE/R/audio.R`
  - **terminal-style backdrop composition** _(to add)_ — currently in
    `kerNLE/R/backdrop.R` and `cornductor/inst/`

The above "(to add)" items are pulled out of kerNLE and cornductor
during the cornyverse NLE-stack consolidation; this is the canonical
home for them going forward.

## compost does NOT own

- The sequence model (`nle_sequence`, tracks, clips). That's
  [nle.api](https://github.com/cornball-ai/nle.api).
- Project intent, character management, GenAI orchestration. That's
  [cornductor](https://github.com/cornball-ai/cornductor).
- The non-linear editor UI / HTTP API. That's
  [kerNLE](https://github.com/cornball-ai/kerNLE).
- The Blender VSE driver. That's
  [blendR](https://github.com/cornball-ai/blendR).

compost is **call-by-call** — each function takes input file paths, a
list of placement / filter parameters, and an output path, and runs
ffmpeg. It does not maintain a timeline, an asset cache, or any
session state. That kind of state belongs upstream in nle.api or
kerNLE.

## How compost interacts with nle.api

A future *render driver* (an ffmpeg-backed counterpart to blendR's
Blender driver) will register with `nle.api::nle_register_driver`:

```r
# In some future package -- maybe compost itself, maybe a sibling
.onLoad <- function(libname, pkgname) {
    if (requireNamespace("nle.api", quietly = TRUE)) {
        nle.api::nle_register_driver(
            "ffmpeg",
            dump  = NULL,                  # ffmpeg has no live timeline to dump
            apply = render_sequence_ffmpeg,  # turns nle_sequence -> mp4
            capabilities = ffmpeg_capabilities
        )
    }
}
```

`apply_sequence("ffmpeg", seq, out = "final.mp4")` would walk the
sequence, build an `ffmpeg -filter_complex` graph using compost's
primitives, and write the final video.

compost itself stays driver-agnostic: it doesn't depend on nle.api.
The driver wrapper sits in whichever package owns the
nle.api-↔-ffmpeg translation (TBD; likely compost when the
chromakey / broadcast / backdrop migrations land).

## Format conventions

When compost functions take *time* arguments, they accept seconds
(floating point). When they're invoked from nle.api land, the caller
converts from rational time:

```r
compost::overlay(
    base   = "ch07_v.mp4",
    pip    = "cornelius.mov",
    pip_x  = clip$pos_x,                            # canonical top-left, +Y down
    pip_y  = clip$pos_y,
    in_s   = nle.api::to_seconds(clip$tl_in),
    out_s  = nle.api::to_seconds(clip$tl_out),
    output = "ch07_with_cornelius.mp4"
)
```

Coordinates inside compost are **top-left + Y down**, matching ffmpeg's
overlay filter conventions and the nle.api canonical wire format. No
conversion needed at the boundary (unlike Blender, which is centre +
Y up).

## See also

- `nle.api/inst/schema/SEQUENCE_SCHEMA.md` — the cornball.sequence.v1
  spec compost consumes when called from an nle.api-driven render path
- `nle.api/AGENTS.md` — visual-content fidelity rules (verbatim source
  spacing, match the user's tooling visuals)
- `cornelius/vault/wiki/nle-stack.md` — canonical role-clarification
  note for the whole NLE stack
