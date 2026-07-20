# rms.R
# Per-window RMS curve of an audio file, via one ffmpeg astats pass. Useful for
# finding quiet spots (pauses) to cut on. Returns the raw curve; callers decide
# what to do with it.

#' ffmpeg filter chain for a per-window RMS readout
#'
#' Slices the audio into \code{window}-sample blocks and prints each block's
#' overall RMS level (dB) as ametadata to \code{mfile}.
#' @param window Window size in samples.
#' @param mfile Path the ametadata print stream is written to.
#' @return A single ffmpeg filter-chain string.
#' @keywords internal
.rms_filter <- function(window, mfile) {
    sprintf(paste0("asetnsamples=%d,astats=metadata=1:reset=1,",
                   "ametadata=print:key=lavfi.astats.Overall.RMS_level:file=%s"),
            as.integer(window), mfile)
}

#' Parse ffmpeg ametadata output into time/rms vectors
#'
#' The print stream alternates a \code{pts_time:<t>} line and an
#' \code{RMS_level=<db>} line per window. Digital silence prints \code{-inf},
#' mapped to -120.
#' @param lines Character vector of ffmpeg ametadata print output.
#' @return A list with numeric \code{time} (window centre, seconds) and
#'   \code{rms} (RMS level in dB).
#' @keywords internal
.parse_rms <- function(lines) {
    tt <- rr <- numeric(length(lines))
    k <- 0L
    cur <- NA_real_
    for (ln in lines) {
        p <- regmatches(ln, regexpr("pts_time:[0-9.]+", ln))
        if (length(p)) {
            cur <- as.numeric(sub("pts_time:", "", p))
            next
        }
        v <- regmatches(ln, regexpr("RMS_level=[-0-9.einf]+", ln))
        if (length(v)) {
            s <- sub("RMS_level=", "", v)
            k <- k + 1L
            tt[k] <- cur
            if (grepl("inf", s)) {
                rr[k] <- -120
            } else {
                rr[k] <- as.numeric(s)
            }
        }
    }
    list(time = tt[seq_len(k)], rms = rr[seq_len(k)])
}

#' Per-window RMS curve of an audio file
#'
#' Runs one ffmpeg \code{astats} pass over \code{file} and returns the RMS level
#' of each fixed-size window. Quieter windows (more-negative dB) mark pauses, so
#' the curve is a cheap way to find good places to cut. About 140x realtime.
#'
#' @param file Path to an audio (or audio-bearing video) file.
#' @param window Window size in samples (default 1024; ~64ms at 16kHz). Smaller
#'   windows give finer time resolution at more rows.
#' @return A list with numeric \code{time} (window centre, seconds) and
#'   \code{rms} (RMS level in dB; digital silence is -120). Empty vectors if the
#'   pass produced nothing.
#' @examples
#' \dontrun{
#' cur <- rms_curve("speech.mp3")
#' plot(cur$time, cur$rms, type = "l")   # dips are pauses
#' }
#' @export
rms_curve <- function(file, window = 1024L) {
    mfile <- tempfile(fileext = ".txt")
    on.exit(unlink(mfile), add = TRUE)
    args <- c("-hide_banner", "-nostats", "-i", file, "-af",
              .rms_filter(window, mfile), "-f", "null", "-")
    suppressWarnings(system2("ffmpeg", shQuote(args), stdout = FALSE, stderr = FALSE))
    if (!file.exists(mfile)) {
        return(list(time = numeric(0), rms = numeric(0)))
    }
    .parse_rms(readLines(mfile, warn = FALSE))
}
