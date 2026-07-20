# The `cornball.*` effect namespace

compost's `render_timeline()` recognizes an open namespace of OTIO `Effect`
objects on clips: any effect whose `effect_name` starts with `cornball.`.
Writers (any tool emitting OTIO via [rotio](https://github.com/cornball-ai/rotio))
attach these to describe deterministic, renderer-lowered motion; compost lowers
the ones it knows into ffmpeg filters at render time.

## Dispatch and degradation

- Effects whose `effect_name` does **not** start with `cornball.` belong to
  other ecosystems and are **ignored silently**.
- A `cornball.*` effect that is unrecognized, has `enabled = false`, or whose
  `metadata.schema` is newer than the renderer supports is **skipped with one
  `warning()`** and the clip renders static. Static rendering is always a
  valid degradation: a bundle written for a newer compost still renders.
- Only the **first** applicable motion effect per clip is used; extras warn.
- Motion effects apply to still-image clips only. Image-sequence clips animate
  themselves; video clips are never re-timed by this namespace.

## `cornball.kenburns` (schema 1)

A deterministic Ken Burns pan/zoom on a still-image clip.

```r
rotio::Effect(
  name        = "motion",              # instance label, informational
  effect_name = "cornball.kenburns",   # dispatch key
  enabled     = TRUE,
  metadata    = list(
    schema    = 1L,      # integer contract version
    zoom_from = 1.0,     # numeric >= 1; the renderer clamps values below 1
    zoom_to   = 1.15,    # numeric >= 1
    from      = c(0.5, 0.5),  # crop-window anchor at p = 0: (x, y) fractions
    to        = c(0.5, 0.40), #   of the source, top-left origin; the window
                              #   is centered on the anchor, clamped to frame
    ease      = "smooth" # "linear" | "smooth" (smoothstep p*p*(3-2p))
  ))
```

Every field except `schema` is optional: `zoom_from`/`zoom_to` default to 1,
`from` defaults to the center, `to` defaults to `from`, `ease` to `"smooth"`.
A missing `schema` is read as 1.

### Semantics

- Motion progress `p` runs 0 → 1 over the clip's **media window**
  `[0, source_range start + duration]`: `p = 0` at media time 0, `p = 1` at
  the last frame. With a transition head handle (a `source_range` starting
  past 0, widened by a preceding `Transition`'s `in_offset`), motion is
  already underway during the dissolve — intended.
- Render rate = the clip's `source_range` rate (fallback 30 fps). The raster
  is the source aspect fitted inside the timeline framing's `pad` box, so the
  pan/zoom never distorts.
- Zoom and anchor interpolate along the easing curve; the crop window is
  centered on the anchor and clamped to the frame, so anchors near an edge
  saturate instead of showing out-of-frame content.

## Reserved names

`cornball.callout` (timed box/underline overlays) is reserved for a future
schema; renderers that don't know it degrade as above.
