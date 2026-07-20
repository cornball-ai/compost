#' Probe Video Metadata
#'
#' Query video file properties via ffprobe.
#'
#' @param file Path to media file.
#' @param field Field to query. One of "duration", "width", "height", "codec_name",
#'   "r_frame_rate", or any valid ffprobe stream entry. Use "all" to return
#'   duration, width, height, and codec as a named list.
#'
#' @return For single fields, returns a numeric value (duration, width, height)
#'   or character string (codec_name, etc.). For field = "all", returns a named list.
#'
#' @examples
#' \dontrun{
#' probe("video.mp4")                     # duration (default)
#' probe("video.mp4", "width")            # video width
#' probe("video.mp4", "all")              # list of duration, width, height, codec
#' }
#'
#' @export
probe <- function(file, field = "duration") {
    if (field == "all") {
        return(list(duration = probe(file, "duration"),
                    width = probe(file, "width"),
                    height = probe(file, "height"),
                    codec = probe(file, "codec_name")))
    }

    # Duration lives at the container level, so it resolves for audio-only
    # files too (a video-stream query returns nothing for an mp3).
    val <- if (field == "duration") {
        .probe_format_field(file, "duration")
    } else {
        .probe_field(file, field)
    }

    # Convert numeric fields
    if (field %in% c("duration", "width", "height")) {
        val <- as.numeric(val)
    }

    val
}

# Video frame rate from ffprobe's "num/den" r_frame_rate.
.video_fps <- function(file) {
    r <- strsplit(.probe_field(file, "r_frame_rate"), "/", fixed = TRUE)[[1]]
    if (length(r) > 1L) {
        den <- as.numeric(r[2])
    } else {
        den <- 1
    }
    as.numeric(r[1]) / den
}
