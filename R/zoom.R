#' Zoom Video with Stepped Levels
#'
#' Apply a stepped zoom effect to a video, changing zoom level at STT line
#' boundaries. Uses 3 levels (no zoom, single, double) with smooth transitions
#' between them. The pattern randomly alternates between zoom-in, hold, and
#' zoom-out phases.
#'
#' @param input Path to input video.
#' @param output Path for output video file.
#' @param stt A whisper_transcription object or data frame with \code{from} and
#'   \code{to} timestamp columns (HH:MM:SS.mmm format).
#' @param amount Zoom per level (default 0.05). Level 1 = 1+amount, level 2 = 1+2*amount.
#' @param levels Number of zoom levels (default 3: none, single, double).
#' @param transition Transition duration in seconds between zoom levels (default 0.3).
#' @param seed Random seed for reproducible zoom patterns.
#' @param width Output width in pixels. If NULL, detected from input.
#' @param height Output height in pixels. If NULL, detected from input.
#' @param overwrite If TRUE (default), overwrite output file.
#' @param dry_run If TRUE, return the FFmpeg command without executing.
#'
#' @return Invisibly returns the output path. If dry_run, returns command string.
#'
#' @examples
#' \dontrun{
#' stt <- readRDS("srt_text.RDS")
#' zoom("talking_head.mp4", "zoomed.mp4", stt = stt)
#' zoom("head.mp4", "zoomed.mp4", stt = stt, amount = 0.05, seed = 42)
#' }
#'
#' @export
zoom <- function(
  input,
  output,
  stt,
  amount = 0.05,
  levels = 3,
  transition = 0.3,
  seed = NULL,
  width = NULL,
  height = NULL,
  overwrite = TRUE,
  dry_run = FALSE
) {

  input <- normalizePath(input, mustWork = TRUE)
  output <- normalizePath(output, mustWork = FALSE)

  if (missing(stt) || is.null(stt)) {
    stop("stt is required (data frame or whisper_transcription with from/to timestamps)")
  }

  # Extract data frame from whisper_transcription if needed
  stt_data <- if (is.data.frame(stt)) stt else stt$data

  # Detect dimensions if not provided
  if (is.null(width))  width  <- probe(input, "width")
  if (is.null(height)) height <- probe(input, "height")

  # Get frame rate
  fps_raw <- .probe_field(input, "r_frame_rate")
  fps <- eval(parse(text = fps_raw))

  # Parse STT timestamps to frame numbers
  starts <- vapply(stt_data$from, .parse_timestamp, numeric(1))
  frame_starts <- round(starts * fps)

  # Generate zoom pattern
  n <- nrow(stt_data)
  zoom_levels <- .generate_zoom_pattern(n, levels, seed)
  zoom_values <- 1.0 + zoom_levels * amount

  # Build piecewise zoom expression
  transition_frames <- max(1L, round(transition * fps))
  z_expr <- .build_zoom_expr(frame_starts, zoom_values, transition_frames)

  # zoompan filter with stepped zoom expression
  zp <- sprintf("zoompan=z='%s':x='(iw-iw/zoom)/2':y='(ih-ih/zoom)/2':d=1:s=%dx%d:fps=%d",
                z_expr, width, height, fps)

  filter <- paste0(zp, ",setsar=1")

  args <- c(
    if (overwrite) "-y",
    "-i", input,
    "-vf", filter,
    "-c:v", "libx264", "-preset", "fast",
    "-c:a", "copy",
    output
  )

  if (dry_run) return(.run_ffmpeg(args, dry_run = TRUE))
  .run_ffmpeg(args)
  invisible(output)
}


#' Parse HH:MM:SS.mmm timestamp to seconds
#' @param x Character timestamp
#' @return Numeric seconds
#' @keywords internal
.parse_timestamp <- function(x) {
  parts <- strsplit(x, ":")[[1]]
  as.numeric(parts[1]) * 3600 + as.numeric(parts[2]) * 60 + as.numeric(parts[3])
}


#' Generate Stepped Zoom Pattern
#'
#' Creates a zoom level sequence that alternates between rest (level 0),
#' zoom-in ramps, holds at target, and zoom-out ramps.
#'
#' @param n Number of segments (STT lines).
#' @param levels Number of zoom levels (default 3).
#' @param seed Optional random seed.
#' @return Integer vector of zoom levels (0 to levels-1).
#' @keywords internal
.generate_zoom_pattern <- function(n, levels = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  max_level <- levels - 1L
  pattern <- integer(n)
  i <- 1L

  while (i <= n) {
    # Rest at level 0 for 1-3 lines
    rest <- sample(1:3, 1)
    end_rest <- min(i + rest - 1L, n)
    pattern[i:end_rest] <- 0L
    i <- end_rest + 1L
    if (i > n) break

    # Pick target level for this phrase
    target <- sample(1:max_level, 1)

    # Zoom in: step up one level per line
    for (step in seq_len(target)) {
      if (i > n) break
      pattern[i] <- step
      i <- i + 1L
    }

    # Hold at target for 1-3 lines
    hold <- sample(1:3, 1)
    end_hold <- min(i + hold - 1L, n)
    if (i <= n) {
      pattern[i:end_hold] <- target
      i <- end_hold + 1L
    }

    # Zoom out: step down one level per line
    if (target >= 1L) {
      for (step in (target - 1L):0L) {
        if (i > n) break
        pattern[i] <- step
        i <- i + 1L
      }
    }
  }

  pattern
}


#' Build Piecewise Zoom Expression for FFmpeg
#'
#' Constructs a nested if() expression that steps between zoom values
#' at frame boundaries, with linear interpolation during transitions.
#'
#' @param frame_starts Integer vector of frame numbers where each segment begins.
#' @param zoom_values Numeric vector of zoom values for each segment.
#' @param transition_frames Number of frames for each transition.
#' @return Character string with the FFmpeg expression.
#' @keywords internal
.build_zoom_expr <- function(frame_starts, zoom_values, transition_frames) {
  n <- length(frame_starts)

  # Expression for a segment: either constant or lerp from previous
  seg_expr <- function(i) {
    z <- zoom_values[i]
    if (i == 1L) return(sprintf("%.4f", z))
    z_prev <- zoom_values[i - 1L]
    if (abs(z - z_prev) < 0.001) return(sprintf("%.4f", z))
    # Linear interpolation from z_prev to z, clamped at z
    sprintf("%.4f+%.4f*min((on-%d)/%d,1)",
            z_prev, z - z_prev, frame_starts[i], transition_frames)
  }

  # Build nested ifs from last segment backwards
  expr <- seg_expr(n)
  if (n > 1L) {
    for (i in (n - 1L):1L) {
      expr <- sprintf("if(lt(on,%d),%s,%s)", frame_starts[i + 1L], seg_expr(i), expr)
    }
  }

  expr
}
