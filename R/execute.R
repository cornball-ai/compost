# Execute a composition plan: lower already-resolved rows to one ffmpeg
# invocation. The contract is cornductor's inst/spec/composition-plan.md:
# the plan derived elsewhere says what plays where; this file interprets
# no timeline and no metadata, and probes media only to decode it. All
# refusals are loud with no partial output -- plans are machine-written,
# so a malformed one is a writer bug.

.plan_num <- function(x, what, integer = TRUE, positive = FALSE,
                      nonneg = FALSE) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) == 0L || anyNA(v) || !all(is.finite(v))) {
        stop("execute_plan(): ", what, " is missing or not a number",
             call. = FALSE)
    }
    if (integer && any(v != round(v))) {
        stop("execute_plan(): ", what, " is not an integer count of ticks",
             call. = FALSE)
    }
    if (positive && any(v <= 0)) {
        stop("execute_plan(): ", what, " must be positive", call. = FALSE)
    }
    if (nonneg && any(v < 0)) {
        stop("execute_plan(): ", what, " must not be negative", call. = FALSE)
    }
    v
}

.plan_df <- function(plan, name, cols) {
    df <- plan[[name]]
    if (!is.data.frame(df)) {
        stop("execute_plan(): plan$", name, " must be a data frame",
             call. = FALSE)
    }
    missing <- setdiff(cols, names(df))
    if (length(missing) > 0L) {
        stop("execute_plan(): plan$", name, " lacks required column(s) ",
             paste(missing, collapse = ", "), call. = FALSE)
    }
    df
}

.plan_media <- function(mp, media_dir, what) {
    if (is.na(mp) || !nzchar(mp)) {
        stop("execute_plan(): ", what, " has no media binding ",
             "(a viewer plan is not executable)", call. = FALSE)
    }
    if (grepl("^(/|[A-Za-z]:)", mp)) {
        stop("execute_plan(): ", what, " carries an absolute media path; ",
             "plan media must be media_dir-relative", call. = FALSE)
    }
    if (is.null(media_dir)) {
        stop("execute_plan(): plan media is media_dir-relative but no ",
             "media_dir was given", call. = FALSE)
    }
    # A relative path may still climb out (../outside.mp4); the binding
    # contract is "beneath media_dir", so normalize and require it.
    base <- normalizePath(media_dir, mustWork = FALSE)
    p <- normalizePath(file.path(media_dir, mp), mustWork = FALSE)
    if (!startsWith(p, paste0(base, "/"))) {
        stop("execute_plan(): ", what, " media '", mp,
             "' escapes media_dir", call. = FALSE)
    }
    if (!file.exists(p)) {
        stop("execute_plan(): ", what, " media '", mp,
             "' does not exist under media_dir", call. = FALSE)
    }
    p
}

# Validate a plan against the refusal table and resolve media bindings.
# Returns the coerced plan with resolved paths attached per table.
.plan_check <- function(plan, media_dir) {
    if (!is.list(plan)) {
        stop("execute_plan(): plan must be a list", call. = FALSE)
    }
    required <- c("schema", "canvas", "fps", "tick_rate", "duration",
                  "video", "transitions", "audio", "captions")
    missing <- setdiff(required, names(plan))
    if (length(missing) > 0L) {
        stop("execute_plan(): plan lacks required field(s) ",
             paste(missing, collapse = ", "), call. = FALSE)
    }
    schema <- .plan_num(plan$schema, "schema", positive = TRUE)
    if (schema > 1) {
        stop("execute_plan(): plan schema ", schema,
             " is newer than this compost supports (1)", call. = FALSE)
    }
    canvas <- .plan_num(plan$canvas, "canvas", integer = FALSE, positive = TRUE)
    if (length(canvas) != 2L || any(canvas != round(canvas)) ||
        any(canvas %% 2 != 0)) {
        stop("execute_plan(): canvas must be two positive even integers",
             call. = FALSE)
    }
    fps <- as.character(plan$fps)
    if (length(fps) != 1L || !grepl("^[0-9]+/[0-9]+$", fps)) {
        stop("execute_plan(): fps must be a rational string like \"30/1\"",
             call. = FALSE)
    }
    fpsn <- as.integer(sub("/.*$", "", fps))
    fpsd <- as.integer(sub("^.*/", "", fps))
    if (fpsn <= 0L || fpsd <= 0L) {
        stop("execute_plan(): fps must be a positive rational", call. = FALSE)
    }
    tick_rate <- .plan_num(plan$tick_rate, "tick_rate", positive = TRUE)
    duration <- .plan_num(plan$duration, "duration", positive = TRUE)
    overrun <- function(df, what) {
        if (nrow(df) > 0L && any(df$start + df$duration > duration)) {
            stop("execute_plan(): a ", what, " row extends past the plan ",
                 "duration; the executor never silently truncates",
                 call. = FALSE)
        }
    }

    video <- .plan_df(plan, "video", c("layer", "asset_id", "media", "kind",
                                       "start", "duration", "source_in",
                                       "dest_x", "dest_y", "dest_w",
                                       "dest_h", "opacity"))
    if (nrow(video) > 0L) {
        .plan_num(video$layer, "video$layer", positive = TRUE)
        .plan_num(video$start, "video$start", nonneg = TRUE)
        .plan_num(video$duration, "video$duration", positive = TRUE)
        .plan_num(video$source_in, "video$source_in", nonneg = TRUE)
        .plan_num(video$dest_x, "video$dest_x")
        .plan_num(video$dest_y, "video$dest_y")
        .plan_num(video$dest_w, "video$dest_w", positive = TRUE)
        .plan_num(video$dest_h, "video$dest_h", positive = TRUE)
        op <- suppressWarnings(as.numeric(video$opacity))
        if (anyNA(op) || any(op < 0) || any(op > 1)) {
            stop("execute_plan(): video$opacity must be in [0, 1]",
                 call. = FALSE)
        }
        if (!all(video$kind %in% c("video", "image"))) {
            stop("execute_plan(): video$kind must be \"video\" or \"image\"",
                 call. = FALSE)
        }
        if (any(video$kind == "image" & video$source_in != 0)) {
            stop("execute_plan(): an image row cannot have a nonzero ",
                 "source_in (an image has no time axis)", call. = FALSE)
        }
        video <- video[order(video$layer, video$start),, drop = FALSE]
        for (l in unique(video$layer)) {
            rows <- video[video$layer == l,, drop = FALSE]
            if (nrow(rows) > 1L) {
                ends <- rows$start + rows$duration
                if (any(rows$start[-1L] < ends[-nrow(rows)])) {
                    stop("execute_plan(): overlapping rows within layer ", l,
                         " (dissolves are transition rows, never overlaps)",
                         call. = FALSE)
                }
            }
        }
        overrun(video, "video")
        video$path <- vapply(seq_len(nrow(video)), function(i) {
            .plan_media(video$media[i], media_dir, sprintf("video row %d", i))
        }, character(1))
    }

    transitions <- .plan_df(plan, "transitions",
                            c("layer", "at", "duration", "type"))
    if (nrow(transitions) > 0L) {
        .plan_num(transitions$at, "transitions$at", nonneg = TRUE)
        .plan_num(transitions$duration, "transitions$duration", positive = TRUE)
        if (!all(transitions$type == "dissolve")) {
            stop("execute_plan(): transition type must be \"dissolve\" in ",
                 "plan schema 1", call. = FALSE)
        }
        for (i in seq_len(nrow(transitions))) {
            t1 <- transitions[i,]
            rows <- video[video$layer == t1$layer,, drop = FALSE]
            j <- which(rows$start == t1$at)
            if (length(j) != 1L || j == 1L ||
                rows$start[j - 1L] + rows$duration[j - 1L] != t1$at) {
                stop("execute_plan(): transition at ", t1$at,
                     " is not a cut between adjacent rows on layer ",
                     t1$layer, call. = FALSE)
            }
            if (t1$duration > rows$duration[j] ||
                t1$duration > rows$duration[j - 1L]) {
                stop("execute_plan(): a dissolve is longer than a ",
                     "neighbouring row", call. = FALSE)
            }
            if (t1$duration > rows$source_in[j]) {
                stop("execute_plan(): a dissolve needs more head handle ",
                     "than the incoming row's source_in provides",
                     call. = FALSE)
            }
        }
    }

    audio <- .plan_df(plan, "audio", c("asset_id", "media", "start",
                                       "duration", "source_in"))
    if (nrow(audio) > 0L) {
        .plan_num(audio$start, "audio$start", nonneg = TRUE)
        .plan_num(audio$duration, "audio$duration", positive = TRUE)
        .plan_num(audio$source_in, "audio$source_in", nonneg = TRUE)
        audio <- audio[order(audio$start, audio$asset_id),, drop = FALSE]
        overrun(audio, "audio")
        audio$path <- vapply(seq_len(nrow(audio)), function(i) {
            .plan_media(audio$media[i], media_dir, sprintf("audio row %d", i))
        }, character(1))
    }

    captions <- .plan_df(plan, "captions", c("asset_id", "media"))
    if (nrow(captions) > 0L) {
        captions$path <- vapply(seq_len(nrow(captions)), function(i) {
            p <- .plan_media(captions$media[i], media_dir,
                             sprintf("caption row %d", i))
            if (!grepl("\\.(ass|srt)$", p, ignore.case = TRUE)) {
                stop("execute_plan(): caption media must be .ass or .srt",
                     call. = FALSE)
            }
            p
        }, character(1))
    }

    list(canvas = as.integer(canvas), fps = fps, fpsn = fpsn, fpsd = fpsd,
         tick_rate = tick_rate, duration = duration, video = video,
         transitions = transitions, audio = audio, captions = captions)
}

# Escape a path for use inside an ffmpeg filter argument.
.filter_path <- function(p) {
    p <- gsub("\\", "/", p, fixed = TRUE)
    p <- gsub("'", "\\\\'", p)
    gsub(":", "\\\\:", p)
}

#' Execute a composition plan
#'
#' Lowers an already-resolved composition plan (the contract in
#' cornductor's `inst/spec/composition-plan.md`) to a single ffmpeg
#' invocation: rows painted in layer order onto a black canvas at their
#' integer-tick times and destination rects, head-fed dissolves
#' synthesized from the incoming row's handle media, audio rows summed
#' at unity, captions burned last, and the output cut frame-exactly at
#' the plan's duration. No timeline, no metadata, no placement
#' decisions: the plan already made them.
#'
#' Refusals are loud and produce no output; the refusal table lives in
#' the spec. In particular a viewer plan (`NA` media) refuses, absolute
#' media paths refuse (bindings are `media_dir`-relative), and an image
#' row with nonzero `source_in` refuses.
#'
#' @param plan A plan-shaped list: `schema`, `canvas`, `fps`,
#'   `tick_rate`, `duration`, and the `video`, `transitions`, `audio`,
#'   `captions` data frames. Unknown extra fields are ignored.
#' @param output Path for the output video file.
#' @param media_dir Directory the plan's `media` bindings are relative
#'   to.
#' @param overwrite If TRUE (default), overwrite the output file.
#' @param dry_run If TRUE, return the ffmpeg command without executing.
#' @return Invisibly returns the output path. If dry_run, returns the
#'   command string.
#' @examples
#' \dontrun{
#' execute_plan(plan, "out.mp4", media_dir = "project.otiod")
#' }
#' @export
execute_plan <- function(plan, output, media_dir = NULL, overwrite = TRUE,
                         dry_run = FALSE) {
    pl <- .plan_check(plan, media_dir)
    secs <- function(t) sprintf("%.6f", t / pl$tick_rate)
    total <- pl$duration / pl$tick_rate
    frames <- as.integer(round(pl$duration * pl$fpsn /
                               (pl$tick_rate * pl$fpsd)))

    ins <- character(0)
    chains <- character(0)
    n_in <- 0L

    add_input <- function(pre, file) {
        ins <<- c(ins, pre, "-i", file)
        n_in <<- n_in + 1L
        n_in - 1L
    }

    # Paint items in layer order: each layer's rows, then its dissolves
    # (a dissolve paints over the outgoing row's tail on its own layer,
    # and higher layers still cover it).
    # The base must be finite: an endless color source keeps the shared
    # filtergraph spinning after -frames:v closes the video stream, and
    # with an audio output alive ffmpeg then never exits. d= bounds it
    # in seconds; -frames:v still cuts frame-exactly.
    prev <- "[bg]"
    chains <- sprintf("color=c=black:s=%dx%d:r=%s:d=%s,settb=AVTB[bg]",
                      pl$canvas[1], pl$canvas[2], pl$fps,
                      sprintf("%.6f", total))
    ov <- 0L
    paint <- function(lbl, x, y, s, e) {
        ov <<- ov + 1L
        out <- sprintf("[ov%d]", ov)
        chains <<- c(chains, sprintf(
                                     "%s%soverlay=%d:%d:enable='between(t,%s,%s)':eof_action=pass:repeatlast=0%s",
                                     prev, lbl, x, y, secs(s), secs(e), out))
        prev <<- out
    }

    v <- pl$video
    for (l in unique(v$layer)) {
        rows <- v[v$layer == l,, drop = FALSE]
        labs <- character(nrow(rows))
        for (i in seq_len(nrow(rows))) {
            r <- rows[i,]
            pre <- if (r$kind == "image") {
                c("-loop", "1", "-t", secs(r$duration))
            } else {
                c("-ss", secs(r$source_in), "-t", secs(r$duration))
            }
            idx <- add_input(pre, r$path)
            op <- if (r$opacity < 1) {
                sprintf(",format=rgba,colorchannelmixer=aa=%.6f", r$opacity)
            } else {
                ""
            }
            lab <- sprintf("[r%d]", idx)
            labs[i] <- lab
            chains <- c(chains, sprintf(
                                        "[%d:v]scale=%d:%d,setsar=1,fps=%s%s,setpts=PTS-STARTPTS+%s/TB%s",
                                        idx, r$dest_w, r$dest_h, pl$fps, op,
                                        secs(r$start), lab))
            paint(lab, r$dest_x, r$dest_y, r$start, r$start + r$duration)
        }
        tx <- pl$transitions[pl$transitions$layer == l,, drop = FALSE]
        for (i in seq_len(nrow(tx))) {
            t1 <- tx[i,]
            j <- which(rows$start == t1$at)
            b <- rows[j,]
            idx <- add_input(c("-ss", secs(b$source_in - t1$duration),
                               "-t", secs(t1$duration)), b$path)
            lab <- sprintf("[d%d]", idx)
            chains <- c(chains, sprintf(
                                        "[%d:v]scale=%d:%d,setsar=1,fps=%s,format=yuva420p,fade=t=in:st=0:d=%s:alpha=1,setpts=PTS-STARTPTS+%s/TB%s",
                                        idx, b$dest_w, b$dest_h, pl$fps,
                                        secs(t1$duration),
                                        secs(t1$at - t1$duration), lab))
            paint(lab, b$dest_x, b$dest_y, t1$at - t1$duration, t1$at)
        }
    }

    # Captions burn after the last paint.
    for (i in seq_len(nrow(pl$captions))) {
        ov <- ov + 1L
        out <- sprintf("[ov%d]", ov)
        chains <- c(chains, sprintf("%ssubtitles=filename='%s'%s", prev,
                                    .filter_path(pl$captions$path[i]), out))
        prev <- out
    }

    a <- pl$audio
    amaps <- character(0)
    if (nrow(a) > 0L) {
        alabs <- character(nrow(a))
        for (i in seq_len(nrow(a))) {
            r <- a[i,]
            idx <- add_input(c("-ss", secs(r$source_in), "-t",
                               secs(r$duration)), r$path)
            lab <- sprintf("[a%d]", idx)
            alabs[i] <- lab
            # Sample-accurate placement: adelay's default unit is the
            # millisecond, which rounds a 33.333ms tick; the S suffix
            # delays in samples, quantizing tick positions to the 44.1kHz
            # mix grid (error at most half a sample).
            smp <- as.integer(round(r$start * 44100 / pl$tick_rate))
            chains <- c(chains, sprintf(
                                        "[%d:a]aformat=sample_rates=44100:channel_layouts=stereo,asetpts=PTS-STARTPTS,adelay=%dS:all=1%s",
                                        idx, smp, lab))
        }
        # normalize=0: rows are summed at unity, never averaged. Trim the
        # finite mix first, then pad to the exact plan length with
        # whole_dur, which emits EOF; a bare apad is infinite and a trim
        # after it consumes forever without ending the stream.
        chains <- c(chains, sprintf(
                                    "%samix=inputs=%d:duration=longest:normalize=0,atrim=0:%s,asetpts=PTS-STARTPTS,apad=whole_dur=%s[aout]",
                                    paste(alabs, collapse = ""), nrow(a),
                                    sprintf("%.6f", total),
                                    sprintf("%.6f", total)))
        amaps <- c("-map", "[aout]", "-c:a", "aac", "-b:a", "192k")
    }

    args <- c(if (overwrite) "-y", ins,
              "-filter_complex", paste(chains, collapse = ";"),
              "-map", prev, amaps,
              "-frames:v", frames,
              "-c:v", "libx264", "-preset", "fast", "-pix_fmt", "yuv420p",
              "-movflags", "+faststart",
              output)
    if (dry_run) {
        return(.run_ffmpeg(args, dry_run = TRUE))
    }
    .run_ffmpeg(args)
    invisible(output)
}
