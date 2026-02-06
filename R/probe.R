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
    return(list(
      duration = probe(file, "duration"),
      width    = probe(file, "width"),
      height   = probe(file, "height"),
      codec    = probe(file, "codec_name")
    ))
  }

  val <- .probe_field(file, field)

  # Convert numeric fields
  if (field %in% c("duration", "width", "height")) {
    val <- as.numeric(val)
  }

  val
}
