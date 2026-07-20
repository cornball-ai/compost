# The cornball layout contract

compost's `render_timeline()` can compose several video tracks onto one
canvas — a talking-head over slides, a stacked short, an over-shoulder box —
driven entirely by data on the OTIO timeline. Writers (any tool emitting OTIO
via [rotio](https://github.com/cornball-ai/rotio)) declare WHERE things go;
compost executes the rects generically and knows no layout names.

## Schema 1

Timeline metadata:

```r
metadata(timeline)$cornball$layout <- list(
    schema = 1L,
    name   = "vertical",                 # informational label
    canvas = c(1080L, 1920L),            # output size, even pixels
    slots  = list(                       # list order = paint order (first = bottom)
        narrator = list(rect = c(0L, 0L, 1080L, 960L),   fit = "fill"),
        visual   = list(rect = c(0L, 960L, 1080L, 960L), fit = "fit")))
```

- `rect = c(x, y, w, h)` in **integer pixels** of the canvas; `w`/`h` even.
- `fit = "fill"` covers the rect (scale up + center-crop — heads);
  `fit = "fit"` contains (scale down + centered black pad — content).
- The slot named **`visual`** binds to the timeline's content track (the
  first non-caption, non-roled video track), assembled exactly like a
  single-stream render (source ranges, transitions, still/sequence
  pre-render, motion effects), and is the **timing reference**: the composed
  output has its frame rate and length.
- Every other slot name binds to a Video track carrying
  `metadata$cornball$role == "<slot name>"`. Slot tracks are assembled the
  same way, normalized (fps, SAR, yuv420p), and **muted by construction**:
  the compose emits video only, and the final render maps the timeline's
  audio-bed track — a layout can never hijack the narration.
- A slot source shorter than the reference freezes its last frame; longer is
  cut. Any number of slots is allowed (b-roll, screencap, logo — just more
  roled tracks with rects).

## Degradation

Mirrors the `cornball.*` effects namespace: a renderer that cannot honor the
layout still renders the content.

| condition | behavior |
|---|---|
| `schema` > supported | one warning, layout ignored, single-stream render |
| layout slot with no matching track | one warning, composition skipped |
| roled track with no layout slot | one warning, track ignored |
| older compost (pre-layout) | roled tracks silently dropped; framing applies — a correct letterboxed single-stream render |

## See also

`cornball-effects.md` — the `cornball.*` per-clip effect namespace
(Ken Burns motion on still clips).
