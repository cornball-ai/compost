# detect.R
# Detect duplicated conditioning frames at a chunk boundary. Chained generation
# (cornductor::generate_stv with keep_overlap = TRUE) prepends a replay of the
# previous chunk's tail to each chunk; older pipelines trimmed it or generated
# chunks independently. Nothing on disk records which, so measure the pixels:
# the previous clip's last frame, if replayed, sits somewhere in the next
# clip's head with near-identical content (SSIM ~0.98 after re-encode), while
# mere continuation frames score visibly lower (~0.85-0.94) and unrelated
# clips collapse (~0.03).

#' Detect the duplicated-frame overlap between two consecutive clips
#'
#' Compares the last frame of \code{from} against the first \code{max_scan}
#' frames of \code{to} (SSIM, via ffmpeg). When the best match is
#' duplicate-grade (\code{>= threshold}), the matching frame index marks the
#' end of a conditioning-head replay and the overlap is \code{index + 1}
#' frames; otherwise the overlap is 0 (the head is new content -- either a
#' trimmed chained clip or an independent generation).
#'
#' Empirical calibration on LTX-2 talking-head chunks (256px, re-encoded
#' H.264): a kept 9-frame conditioning head peaks at 0.98-0.99 on frame 8;
#' plain continuation peaks ~0.84 on frame 0; independently generated clips
#' sit flat near 0.03. The default \code{threshold} of 0.95 separates these
#' cleanly.
#'
#' @param from Path to the earlier clip.
#' @param to Path to the following clip (whose head may replay \code{from}'s
#'   tail).
#' @param max_scan Number of head frames of \code{to} to scan (default 16).
#' @param threshold Minimum SSIM for a frame to count as a duplicate
#'   (default 0.95).
#' @return Integer count of overlapping frames at the head of \code{to}
#'   (0 = none), with the per-frame SSIM curve attached as attribute
#'   \code{"ssim"}.
#' @examples
#' \dontrun{
#' detect_overlap("media/chunk01.mp4", "media/chunk02.mp4")
#' }
#' @export
detect_overlap <- function(from, to, max_scan = 16L, threshold = 0.95) {
    from <- normalizePath(from, mustWork = TRUE)
    to <- normalizePath(to, mustWork = TRUE)
    max_scan <- as.integer(max_scan)

    work <- tempfile("detect_overlap_")
    dir.create(work)
    on.exit(unlink(work, recursive = TRUE), add = TRUE)

    # Last frame of `from`, downscaled: SSIM ranks identically at 128px and
    # the comparison loop runs ~10x faster than at full resolution.
    last_png <- file.path(work, "last.png")
    n_from <- .video_frame_count(from)
    .run_ffmpeg(c("-nostdin", "-y", "-i", from,
                  "-vf", sprintf("select='eq(n,%d)',scale=128:128", n_from - 1L),
                  "-frames:v", "1", last_png))

    # First max_scan frames of `to`.
    head_pat <- file.path(work, "head_%03d.png")
    .run_ffmpeg(c("-nostdin", "-y", "-i", to,
                  "-vf", sprintf("select='lt(n,%d)',scale=128:128", max_scan),
                  "-vsync", "0", head_pat))
    heads <- sort(Sys.glob(file.path(work, "head_*.png")))
    if (length(heads) == 0L) {
        stop("detect_overlap(): could not extract head frames from ", to,
             call. = FALSE)
    }

    ssim <- vapply(heads, function(h) .frame_ssim(h, last_png), numeric(1))
    names(ssim) <- NULL

    peak <- which.max(ssim)
    overlap <- if (ssim[peak] >= threshold) peak else 0L
    structure(as.integer(overlap), ssim = ssim)
}

# Video frame count: stream metadata when the container carries it (mp4 does),
# full decode count otherwise.
.video_frame_count <- function(file) {
    n <- suppressWarnings(as.integer(.probe_field(file, "nb_frames")))
    if (!is.na(n) && n > 0L) {
        return(n)
    }
    out <- system2("ffprobe",
                   shQuote(c("-v", "error", "-count_frames",
                             "-select_streams", "v",
                             "-show_entries", "stream=nb_read_frames",
                             "-of", "csv=p=0", file)),
                   stdout = TRUE, stderr = FALSE)
    n <- suppressWarnings(as.integer(out[1]))
    if (is.na(n)) {
        stop("detect_overlap(): cannot determine frame count of ", file,
             call. = FALSE)
    }
    n
}

# SSIM between two same-sized images. ffmpeg prints the score at info level:
# "[Parsed_ssim_0 @ ...] SSIM R:... G:... B:... All:0.982859 (...)".
.frame_ssim <- function(a, b) {
    out <- suppressWarnings(system2("ffmpeg",
                                    shQuote(c("-nostdin", "-i", a, "-i", b,
                                              "-filter_complex", "ssim",
                                              "-f", "null", "-")),
                                    stdout = TRUE, stderr = TRUE))
    m <- regmatches(out, regexpr("All:[0-9.]+", out))
    if (length(m) == 0L) {
        stop("detect_overlap(): no SSIM score from ffmpeg for ", a,
             call. = FALSE)
    }
    as.numeric(sub("All:", "", m[length(m)]))
}
