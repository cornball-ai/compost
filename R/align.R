# align.R
# Place chunk media on the narration clock by matching the audio each chunk
# carries against the continuous narration. The narration is the ground truth:
# a chunk belongs wherever its words sit in the narration, regardless of how
# the video was generated (chained, restarted, undersized). This supersedes
# frame matching, which false-positives whenever a restarted chunk's start
# pose resembles the previous chunk's end pose. Transitory by design: future
# bundles carry the generation decisions in content.otio and need no
# measurement at all.

# Mono RMS-ish envelope of a media file's audio at sr/hop points per second.
.audio_envelope <- function(file, sr = 8000L, hop = 80L) {
    pcm <- tempfile(fileext = ".pcm")
    on.exit(unlink(pcm), add = TRUE)
    .run_ffmpeg(c("-nostdin", "-y", "-i", file, "-vn", "-ac", "1",
                  "-ar", sr, "-f", "s16le", pcm))
    n_samp <- file.size(pcm) %/% 2L
    if (is.na(n_samp) || n_samp < hop) {
        stop("align_audio(): no decodable audio in ", file, call. = FALSE)
    }
    x <- readBin(pcm, "integer", n = n_samp, size = 2L, signed = TRUE,
                 endian = "little")
    n <- (length(x) %/% hop) * hop
    colMeans(matrix(abs(as.numeric(x[seq_len(n)])), nrow = hop))
}

#' Locate a clip's audio within a longer narration
#'
#' Normalized cross-correlation of amplitude envelopes (10ms resolution by
#' default): returns the offset in seconds at which \code{clip}'s audio best
#' matches \code{narration}. Works on video files (their audio stream) and
#' plain audio files alike.
#'
#' @param clip Media file whose audio to place.
#' @param narration The continuous reference audio (e.g. the track's
#'   \code{audio.mp3}).
#' @param sr Decode sample rate (default 8000).
#' @param hop Envelope hop in samples (default 80 = 10ms at 8kHz).
#' @return Offset in seconds (numeric), with attributes \code{correlation}
#'   (peak normalized correlation, 0-1ish) and \code{curve} (correlation per
#'   candidate lag).
#' @examples
#' \dontrun{
#' align_audio("media/chunk02.mp4", "audio.mp3")
#' }
#' @export
align_audio <- function(clip, narration, sr = 8000L, hop = 80L) {
    e1 <- .audio_envelope(clip, sr, hop)
    e0 <- .audio_envelope(narration, sr, hop)
    max_lag <- length(e0) - length(e1)
    if (max_lag < 0L) {
        # Clip audio longer than the narration: compare what fits from 0.
        e1 <- e1[seq_along(e0)]
        max_lag <- 0L
    }
    m1 <- e1 - mean(e1)
    s1 <- sqrt(sum(m1^2))
    if (s1 == 0) {
        stop("align_audio(): clip audio is silent: ", clip, call. = FALSE)
    }
    cors <- vapply(0:max_lag, function(k) {
        seg <- e0[(k + 1L):(k + length(e1))]
        m0 <- seg - mean(seg)
        s0 <- sqrt(sum(m0^2))
        if (s0 == 0) {
            return(0)
        }
        sum(m0 * m1) / (s0 * s1)
    }, numeric(1))
    best <- which.max(cors)
    structure((best - 1L) * hop / sr,
              correlation = cors[best], curve = cors)
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
        stop("cannot determine frame count of ", file, call. = FALSE)
    }
    n
}

#' Align a chunk sequence to its narration and derive per-join overlaps
#'
#' Places each chunk on the narration clock via [align_audio], then derives
#' each join's overlap from the positions: the seconds by which chunk
#' \code{j}'s video runs past where chunk \code{j+1}'s content begins
#' (clamped at 0 = butt join). This is layout truth by construction -- a
#' chained chunk's duplicated head and an undersized chunk's shortfall both
#' fall out of where the words actually are.
#'
#' @param files Character vector of chunk media paths, in order.
#' @param narration The continuous narration audio.
#' @param fps Frame rate for the \code{overlap} frame counts (default 24).
#' @return A data frame with one row per chunk: \code{chunk}, \code{file},
#'   \code{frames}, \code{fps}, \code{offset} (narration seconds),
#'   \code{correlation}, and \code{overlap} (frames; 0 for the first chunk).
#' @export
align_overlaps <- function(files, narration, fps = 24) {
    n <- length(files)
    if (n == 0L) {
        stop("align_overlaps(): no files", call. = FALSE)
    }
    frames <- vapply(files, .video_frame_count, integer(1))
    al <- lapply(files, align_audio, narration = narration)
    offset <- vapply(al, as.numeric, numeric(1))
    correlation <- vapply(al, attr, numeric(1), "correlation")
    overlap <- integer(n)
    if (n > 1L) {
        for (j in 2:n) {
            tail_end <- offset[j - 1L] + frames[j - 1L] / fps
            overlap[j] <- max(0L, as.integer(round((tail_end - offset[j]) * fps)))
        }
    }
    data.frame(chunk = seq_len(n), file = basename(files), frames = frames,
               fps = fps, offset = offset, correlation = correlation,
               overlap = overlap, row.names = NULL)
}
