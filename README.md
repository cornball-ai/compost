# compost

Video compositing via FFmpeg, wrapped in clean R functions.

`compost` turns common `ffmpeg` `filter_complex` patterns (overlay, chromakey,
concat, scale, stack) into small, composable R functions, and lowers an
[OpenTimelineIO](https://opentimeline.io) timeline (through
[rotio](https://github.com/cornball-ai/rotio)) to a single rendered video.

Every function shells out to `ffmpeg`/`ffprobe`. Nothing is reimplemented in R;
the package's job is to build correct filtergraphs so you don't have to.

## Install

```r
remotes::install_github("cornball-ai/compost")
```

Requires `ffmpeg` (>= 5.0) on your `PATH`. The only R dependency is `rotio`.

## Conventions

A few patterns hold across the API:

- Output format is inferred from the output file's extension.
- `overwrite = TRUE` (default) passes `-y` to ffmpeg.
- `dry_run = TRUE` returns the ffmpeg command string instead of running it, so
  you can inspect or test the filtergraph without rendering.
- Functions return the output path invisibly.

## What's in it

### Compositing

| Function | Does |
|---|---|
| `overlay()` | Composite a PNG (with alpha) or video over a background |
| `pip()` | Picture-in-picture a clip into a corner |
| `chromakey()` | Key out a colored background and composite |
| `colorkey()` | Key out a flat background, write ProRes 4444 (alpha preserved) |
| `hstack()` | Stack two videos side by side |
| `vstack()` | Stack two videos vertically |
| `pad()` | Scale to fit and pad onto a canvas |
| `zoom()` | Stepped zoom that changes level at STT line boundaries |

### Joining and transitions

| Function | Does |
|---|---|
| `concat()` | Join segments sequentially via the concat demuxer |
| `crossfade_concat()` | Dissolve between clips, optionally overlay one audio track |

### Audio

| Function | Does |
|---|---|
| `audio_convert()` | Re-encode / resample / remix to a new container or codec |
| `broadcast_audio()` | Broadcast-clean chain: high-pass, de-hum, loudness, fades |
| `align_audio()` | Locate a clip's audio within a longer narration |
| `align_overlaps()` | Place chunks on the narration clock and derive overlaps |
| `subclip()` | Cut a `[start, end]` range out of a media file |
| `rms_curve()` | Per-block RMS levels across a file |

### Inspection and frames

| Function | Does |
|---|---|
| `probe()` | Query video properties via ffprobe |
| `frame_export()` | Pull a single frame at a given time as an image |

### Timeline

| Function | Does |
|---|---|
| `render_timeline()` | Lower a rotio (OTIO) timeline to one ffmpeg render |

## Examples

Picture-in-picture a webcam into the bottom-right of a screen recording:

```r
library(compost)

pip("screen.mp4", "webcam.mp4", "out.mp4",
    scale = 0.25, corner = "lower-right")
```

Convert a meeting recording to STT-ready audio in one pass:

```r
audio_convert("recording.mp4", "speech.wav",
              sample_rate = 16000, channels = 1)
```

Crossfade a sequence of clips and lay one music bed over the top:

```r
crossfade_concat(c("a.mp4", "b.mp4", "c.mp4"), "reel.mp4",
                 fade = 0.5, audio = "music.mp3")
```

Inspect the ffmpeg command without rendering (returns the command string):

```r
overlay("bg.mp4", "logo.png", "out.mp4", x = 40, y = 40, dry_run = TRUE)
```

## License

MIT. Copyright cornball.ai.
