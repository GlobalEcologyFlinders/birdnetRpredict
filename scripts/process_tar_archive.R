get_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_args <- grep("^--file=", command_args, value = TRUE)

  if (length(file_args) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_args[1]))))
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(dirname(normalizePath(frame$ofile)))
    }
  }

  normalizePath(".")
}

script_dir <- get_script_dir()
source(file.path(script_dir, "birdnet_helpers.R"))

if (!requireNamespace("processx", quietly = TRUE)) {
  stop("The processx package is required for live progress monitoring. Install it with install.packages('processx').")
}

if (!requireNamespace("callr", quietly = TRUE)) {
  stop("The callr package is required for monitored BirdNET execution. Install it with install.packages('callr').")
}

archive_file <- normalizePath(
  "/Volumes/bradshaw/acoustic/GEL_A/GEL_A202508202025_12312025.tar.zst",
  mustWork = TRUE
)

species_csv <- normalizePath(
  file.path(
    script_dir,
    "..",
    "data",
    "species_lists",
    "regional",
    "lower_murray",
    "BirdNet_SA_LowerMurray_Tolderol_matches.csv"
  ),
  mustWork = TRUE
)

archive_name <- sub("\\.tar\\.zst$", "", basename(archive_file), ignore.case = TRUE)
output_root <- file.path(
  script_dir,
  "..",
  "out",
  paste0(archive_name, "_birdnet_output")
)
extract_root <- file.path(tempdir(), archive_name, "extract")
manifest_csv <- file.path(output_root, paste0(archive_name, "_processing_manifest.csv"))
file_results_txt <- file.path(output_root, paste0(archive_name, "_file_results.txt"))
summary_of_summaries_txt <- file.path(output_root, paste0(archive_name, "_summary_of_summaries.txt"))
stage_heartbeat_seconds <- 5
stage_timeout_seconds <- 3600

pipeline_settings <- list(
  species_csv = species_csv,
  timezone = "Australia/Adelaide",
  fallback_latitude = -35.52235,
  fallback_longitude = 139.10576,
  prediction_min_confidence = 0.05,
  summary_confidence_threshold = 0.05,
  use_arrow = TRUE
)

timestamp_text <- function(x = Sys.time()) {
  format(x, "%Y-%m-%d %H:%M:%S")
}

format_duration <- function(seconds) {
  if (is.null(seconds) || is.na(seconds) || !is.finite(seconds)) {
    return("n/a")
  }

  total_seconds <- max(0L, as.integer(round(seconds)))
  hours <- total_seconds %/% 3600L
  minutes <- (total_seconds %% 3600L) %/% 60L
  secs <- total_seconds %% 60L

  sprintf("%02d:%02d:%02d", hours, minutes, secs)
}

emit_console <- function(message) {
  cat(sprintf("[%s] %s\n", timestamp_text(), message))
  flush.console()
}

progress_label <- function(index, total_files) {
  if (total_files <= 0) {
    return(sprintf("%d/%d (0.0%%)", index, total_files))
  }

  sprintf("%d/%d (%.1f%%)", index, total_files, 100 * index / total_files)
}

estimate_remaining_seconds <- function(start_time, completed_files, total_files) {
  if (completed_files <= 0) {
    return(NA_real_)
  }

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  average_seconds_per_file <- elapsed / completed_files
  remaining_files <- max(0L, total_files - completed_files)

  average_seconds_per_file * remaining_files
}

safe_read_summary_csv <- function(summary_csv) {
  if (!file.exists(summary_csv) || file.info(summary_csv)$size == 0) {
    return(empty_summary_table())
  }

  summary_df <- tryCatch(
    read.csv(summary_csv, stringsAsFactors = FALSE),
    error = function(error) empty_summary_table()
  )

  required_columns <- c(
    "date_time",
    "scientific_name",
    "common_name",
    "confidence",
    "cumulative_number_of_new_species_detected",
    "total_number_of_species_identified"
  )

  missing_columns <- setdiff(required_columns, names(summary_df))
  for (column_name in missing_columns) {
    if (nrow(summary_df) == 0) {
      summary_df[[column_name]] <- empty_summary_table()[[column_name]]
    } else if (column_name %in% c("date_time", "scientific_name", "common_name")) {
      summary_df[[column_name]] <- rep("", nrow(summary_df))
    } else {
      summary_df[[column_name]] <- rep(NA_real_, nrow(summary_df))
    }
  }

  summary_df[, required_columns, drop = FALSE]
}

collapse_species <- function(summary_table, limit = 12L) {
  if (nrow(summary_table) == 0) {
    return("none")
  }

  species_labels <- unique(
    paste(summary_table$common_name, sprintf("(%s)", summary_table$scientific_name))
  )
  species_labels <- species_labels[seq_len(min(length(species_labels), limit))]
  collapse_text <- paste(species_labels, collapse = "; ")

  if (length(unique(paste(summary_table$common_name, summary_table$scientific_name))) > limit) {
    paste0(collapse_text, "; ...")
  } else {
    collapse_text
  }
}

stage_progress <- function(stage) {
  switch(
    stage,
    listing_archive = 0,
    starting_file = 0,
    skipped_existing = 100,
    extracting_flac = 10,
    extracted_flac = 20,
    converting_wav = 30,
    converted_wav = 40,
    building_range_filter = 55,
    running_birdnet = 80,
    writing_empty_summary = 95,
    writing_summary = 95,
    cleaning_temp = 98,
    completed_file = 100,
    error = 100,
    complete = 100,
    0
  )
}

sanitize_stream_lines <- function(lines) {
  if (length(lines) == 0) {
    return(character())
  }

  cleaned <- gsub("\r", "", lines, fixed = TRUE)
  cleaned <- gsub("[[:cntrl:]]", "", cleaned)
  cleaned[nzchar(cleaned)]
}

split_stream_text <- function(text) {
  if (length(text) == 0 || is.null(text) || !nzchar(text)) {
    return(character())
  }

  pieces <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  sanitize_stream_lines(pieces)
}

make_ffmpeg_progress_parser <- function(input_name, output_name) {
  latest_out_time <- NULL
  latest_progress <- NULL

  function(lines) {
    if (length(lines) > 0) {
      for (line in lines) {
        if (grepl("^out_time=", line)) {
          latest_out_time <<- sub("^out_time=", "", line)
        } else if (grepl("^progress=", line)) {
          latest_progress <<- sub("^progress=", "", line)
        }
      }
    }

    detail <- sprintf("converting %s -> %s", input_name, output_name)

    if (!is.null(latest_out_time) && nzchar(latest_out_time)) {
      detail <- paste0(detail, " | encoded=", latest_out_time)
    }

    if (!is.null(latest_progress) && nzchar(latest_progress)) {
      detail <- paste0(detail, " | ffmpeg=", latest_progress)
    }

    detail
  }
}

run_monitored_command <- function(command,
                                  args,
                                  phase_prefix,
                                  archive_member,
                                  stage,
                                  start_time,
                                  total_files,
                                  manifest,
                                  detail_text,
                                  heartbeat_seconds = 5,
                                  timeout_seconds = Inf,
                                  detail_parser = NULL) {
  process <- processx::process$new(
    command = command,
    args = args,
    stdout = "|",
    stderr = "|",
    cleanup_tree = TRUE
  )

  start_stage_at <- Sys.time()
  last_report_at <- as.POSIXct(NA)
  collected_output <- character()
  current_detail <- detail_text

  on.exit({
    if (process$is_alive()) {
      process$kill_tree()
    }
  }, add = TRUE)

  repeat {
    process$poll_io(1000)

    stdout_lines <- process$read_output_lines()
    stderr_lines <- process$read_error_lines()

    if (length(stdout_lines) > 0 || length(stderr_lines) > 0) {
      collected_output <- c(collected_output, stdout_lines, stderr_lines)
      if (!is.null(detail_parser)) {
        current_detail <- detail_parser(c(stdout_lines, stderr_lines))
      }
    }

    now <- Sys.time()
    stage_elapsed_seconds <- as.numeric(difftime(now, start_stage_at, units = "secs"))

    if (is.na(last_report_at) ||
        as.numeric(difftime(now, last_report_at, units = "secs")) >= heartbeat_seconds) {
      emit_console(
        sprintf(
          "%s [file %.1f%%] %s | stage_elapsed=%s",
          phase_prefix,
          stage_progress(stage),
          current_detail,
          format_duration(stage_elapsed_seconds)
        )
      )
      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = stage,
        current_detail = paste0(current_detail, " | stage_elapsed=", format_duration(stage_elapsed_seconds)),
        start_time = start_time,
        total_files = total_files
      )
      last_report_at <- now
    }

    if (!process$is_alive()) {
      break
    }

    if (is.finite(timeout_seconds) && stage_elapsed_seconds > timeout_seconds) {
      process$kill_tree()
      stop(
        sprintf(
          "%s timed out after %s for %s",
          stage,
          format_duration(stage_elapsed_seconds),
          archive_member
        )
      )
    }
  }

  collected_output <- c(collected_output, process$read_output_lines(), process$read_error_lines())

  if (!identical(process$get_exit_status(), 0L)) {
    stop(
      paste(
        c(
          sprintf("%s failed for %s", command, archive_member),
          utils::tail(collected_output, 20)
        ),
        collapse = "\n"
      )
    )
  }

  invisible(collected_output)
}

run_monitored_birdnet_analysis <- function(script_dir,
                                           pipeline_settings,
                                           wav_path,
                                           output_dir,
                                           output_stem,
                                           phase_prefix,
                                           archive_member,
                                           start_time,
                                           total_files,
                                           manifest,
                                           heartbeat_seconds = 5,
                                           timeout_seconds = Inf) {
  result_rds <- tempfile(pattern = "birdnet-result-", fileext = ".rds")

  bg <- callr::r_bg(
    func = function(script_dir,
                    pipeline_settings,
                    wav_path,
                    output_dir,
                    output_stem,
                    result_rds) {
      suppressPackageStartupMessages(source(file.path(script_dir, "birdnet_helpers.R")))

      pipeline <- do.call(initialize_birdnet_pipeline, pipeline_settings)
      result <- process_audio_file(
        pipeline = pipeline,
        audio_file = wav_path,
        output_dir = output_dir,
        output_stem = output_stem,
        allow_empty_summary = TRUE
      )

      saveRDS(result, result_rds)
      invisible(NULL)
    },
    args = list(
      script_dir = script_dir,
      pipeline_settings = pipeline_settings,
      wav_path = wav_path,
      output_dir = output_dir,
      output_stem = output_stem,
      result_rds = result_rds
    ),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )

  start_stage_at <- Sys.time()
  last_report_at <- as.POSIXct(NA)
  output_lines <- character()

  on.exit({
    if (bg$is_alive()) {
      bg$kill()
    }
    if (file.exists(result_rds)) {
      unlink(result_rds)
    }
  }, add = TRUE)

  repeat {
    bg$poll_io(1000)

    stdout_lines <- bg$read_output_lines()
    stderr_lines <- bg$read_error_lines()
    if (length(stdout_lines) > 0 || length(stderr_lines) > 0) {
      output_lines <- c(output_lines, stdout_lines, stderr_lines)
    }

    now <- Sys.time()
    stage_elapsed_seconds <- as.numeric(difftime(now, start_stage_at, units = "secs"))
    recent_output <- trimws(paste(utils::tail(output_lines[nzchar(output_lines)], 2), collapse = " | "))
    current_detail <- sprintf("analysing %s", basename(wav_path))
    if (nzchar(recent_output)) {
      current_detail <- paste0(current_detail, " | last_output=", recent_output)
    }

    if (is.na(last_report_at) ||
        as.numeric(difftime(now, last_report_at, units = "secs")) >= heartbeat_seconds) {
      emit_console(
        sprintf(
          "%s [file %.1f%%] %s | stage_elapsed=%s",
          phase_prefix,
          stage_progress("running_birdnet"),
          current_detail,
          format_duration(stage_elapsed_seconds)
        )
      )
      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = "running_birdnet",
        current_detail = paste0(current_detail, " | stage_elapsed=", format_duration(stage_elapsed_seconds)),
        start_time = start_time,
        total_files = total_files
      )
      last_report_at <- now
    }

    if (!bg$is_alive()) {
      break
    }

    if (is.finite(timeout_seconds) && stage_elapsed_seconds > timeout_seconds) {
      bg$kill()
      stop(
        sprintf(
          "running_birdnet timed out after %s for %s",
          format_duration(stage_elapsed_seconds),
          archive_member
        )
      )
    }
  }

  stdout_lines <- bg$read_output_lines()
  stderr_lines <- bg$read_error_lines()
  output_lines <- c(output_lines, stdout_lines, stderr_lines)

  status <- bg$get_exit_status()
  if (!identical(status, 0L)) {
    stop(
      paste(
        c(
          sprintf("BirdNET analysis failed for %s", archive_member),
          utils::tail(output_lines, 20)
        ),
        collapse = "\n"
      )
    )
  }

  if (!file.exists(result_rds)) {
    stop("BirdNET analysis completed without producing a result file for ", archive_member)
  }

  readRDS(result_rds)
}

list_archive_flac_members <- function(archive_path,
                                      manifest,
                                      start_time,
                                      heartbeat_seconds = 5,
                                      timeout_seconds = Inf) {
  process <- processx::process$new(
    command = "tar",
    args = c("--zstd", "-tf", archive_path),
    stdout = NULL,
    stderr = NULL,
    cleanup_tree = TRUE,
    pty = TRUE
  )

  start_stage_at <- Sys.time()
  last_report_at <- as.POSIXct(NA)
  listing_lines <- character()

  on.exit({
    if (process$is_alive()) {
      process$kill_tree()
    }
  }, add = TRUE)

  repeat {
    process$poll_io(1000)

    output_text <- process$read_output()
    output_lines <- split_stream_text(output_text)
    if (length(output_lines) > 0) {
      listing_lines <- c(listing_lines, output_lines)
    }

    now <- Sys.time()
    stage_elapsed_seconds <- as.numeric(difftime(now, start_stage_at, units = "secs"))
    flac_count <- sum(grepl("\\.flac$", listing_lines, ignore.case = TRUE))
    detail_text <- sprintf("scanning archive members | flac_found_so_far=%d", flac_count)

    if (is.na(last_report_at) ||
        as.numeric(difftime(now, last_report_at, units = "secs")) >= heartbeat_seconds) {
      emit_console(
        sprintf(
          "[archive scan] %s | stage_elapsed=%s",
          detail_text,
          format_duration(stage_elapsed_seconds)
        )
      )
      update_live_progress(
        manifest = manifest,
        current_member = "",
        current_phase = "listing_archive",
        current_detail = paste0(detail_text, " | stage_elapsed=", format_duration(stage_elapsed_seconds)),
        start_time = start_time,
        total_files = 0
      )
      last_report_at <- now
    }

    if (!process$is_alive()) {
      break
    }

    if (is.finite(timeout_seconds) && stage_elapsed_seconds > timeout_seconds) {
      process$kill_tree()
      stop(
        sprintf(
          "listing_archive timed out after %s",
          format_duration(stage_elapsed_seconds)
        )
      )
    }
  }

  listing_lines <- c(
    listing_lines,
    split_stream_text(process$read_output())
  )

  if (!identical(process$get_exit_status(), 0L)) {
    stop(
      paste(
        c(
          "tar archive listing failed",
          utils::tail(listing_lines, 20)
        ),
        collapse = "\n"
      )
    )
  }

  listing_lines[grepl("\\.flac$", listing_lines, ignore.case = TRUE)]
}

extract_archive_member <- function(archive_path, archive_member, target_dir) {
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  tar_status <- system2(
    "tar",
    args = c("--zstd", "-xf", archive_path, "-C", target_dir, archive_member)
  )

  if (!identical(tar_status, 0L)) {
    stop("tar extraction failed for ", archive_member)
  }

  file.path(target_dir, archive_member)
}

convert_flac_to_wav <- function(flac_path, wav_path) {
  dir.create(dirname(wav_path), recursive = TRUE, showWarnings = FALSE)
  ffmpeg_status <- system2(
    "ffmpeg",
    args = c("-hide_banner", "-loglevel", "error", "-y", "-i", flac_path, wav_path)
  )

  if (!identical(ffmpeg_status, 0L)) {
    stop("ffmpeg conversion failed for ", flac_path)
  }
}

output_paths_for_member <- function(output_dir, archive_member) {
  relative_stem <- tools::file_path_sans_ext(archive_member)

  list(
    predictions_csv = file.path(output_dir, paste0(relative_stem, "_birdnet_predictions.csv")),
    summary_csv = file.path(output_dir, paste0(relative_stem, "_birdnet_species_summary.csv"))
  )
}

write_file_results_txt <- function(manifest, output_path) {
  lines <- c(
    "BirdNET archive processing results by file",
    paste("Updated:", timestamp_text()),
    ""
  )

  if (nrow(manifest) == 0) {
    lines <- c(lines, "No files processed yet.")
    writeLines(lines, output_path)
    return(invisible(NULL))
  }

  for (row_index in seq_len(nrow(manifest))) {
    row <- manifest[row_index, , drop = FALSE]

    lines <- c(
      lines,
      sprintf("[%d] %s", row_index, row$archive_member),
      sprintf("  status: %s", row$status),
      sprintf("  started_at: %s", row$started_at),
      sprintf("  finished_at: %s", row$finished_at),
      sprintf("  processing_time: %s", row$processing_time_hms),
      sprintf("  recording_date: %s", row$recording_date),
      sprintf("  latitude: %s", row$recording_latitude),
      sprintf("  longitude: %s", row$recording_longitude),
      sprintf("  summary_rows: %s", row$summary_rows),
      sprintf("  total_species_identified: %s", row$total_species_identified),
      sprintf("  species_detected: %s", row$species_detected),
      sprintf("  predictions_csv: %s", row$predictions_csv),
      sprintf("  summary_csv: %s", row$summary_csv),
      sprintf("  error_message: %s", row$error_message),
      ""
    )
  }

  writeLines(lines, output_path)
}

write_summary_of_summaries_txt <- function(manifest,
                                           output_path,
                                           archive_file,
                                           current_member,
                                           current_phase,
                                           current_detail,
                                           current_file_progress,
                                           start_time,
                                           total_files) {
  completed_files <- nrow(manifest)
  ok_files <- sum(manifest$status == "ok")
  skipped_files <- sum(manifest$status == "skipped_existing")
  empty_files <- sum(manifest$status %in% c("no_summary_detections", "no_usable_detections"))
  error_files <- sum(manifest$status == "error")
  total_summary_rows <- sum(manifest$summary_rows, na.rm = TRUE)
  total_species_hits <- sum(manifest$total_species_identified, na.rm = TRUE)

  cumulative_species <- character()
  if (nrow(manifest) > 0) {
    for (summary_csv in manifest$summary_csv[file.exists(manifest$summary_csv)]) {
      summary_df <- safe_read_summary_csv(summary_csv)
      cumulative_species <- unique(c(cumulative_species, summary_df$scientific_name))
    }
  }
  cumulative_species <- stats::na.omit(cumulative_species)
  cumulative_species <- cumulative_species[nzchar(cumulative_species)]

  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  remaining_seconds <- estimate_remaining_seconds(start_time, completed_files, total_files)
  eta_text <- if (is.na(remaining_seconds)) {
    "n/a"
  } else {
    timestamp_text(Sys.time() + remaining_seconds)
  }

  lines <- c(
    "BirdNET archive summary of summaries",
    paste("Archive:", archive_file),
    paste("Updated:", timestamp_text()),
    paste("Current file:", if (nzchar(current_member)) current_member else "none"),
    paste("Current phase:", if (nzchar(current_phase)) current_phase else "idle"),
    paste("Current detail:", if (nzchar(current_detail)) current_detail else "none"),
    sprintf("Current file stage progress: %.1f%%", current_file_progress),
    "",
    sprintf(
      "Progress: %d/%d files complete (%.1f%%)",
      completed_files,
      total_files,
      if (total_files > 0) 100 * completed_files / total_files else 0
    ),
    sprintf("Elapsed time: %s", format_duration(elapsed_seconds)),
    sprintf("Estimated time remaining: %s", format_duration(remaining_seconds)),
    sprintf("Estimated completion time: %s", eta_text),
    "",
    sprintf("Successful files: %d", ok_files),
    sprintf("Skipped existing files: %d", skipped_files),
    sprintf("Files with no summary detections: %d", empty_files),
    sprintf("Errored files: %d", error_files),
    sprintf("Total summary rows across files: %d", total_summary_rows),
    sprintf("Sum of per-file species counts: %d", total_species_hits),
    sprintf("Cumulative unique species across all summaries: %d", length(cumulative_species)),
    ""
  )

  if (length(cumulative_species) > 0) {
    lines <- c(
      lines,
      "Cumulative species list:",
      paste(sort(cumulative_species), collapse = ", "),
      ""
    )
  }

  writeLines(lines, output_path)
}

update_live_progress <- function(manifest,
                                 current_member,
                                 current_phase,
                                 current_detail,
                                 start_time,
                                 total_files) {
  write_summary_of_summaries_txt(
    manifest = manifest,
    output_path = summary_of_summaries_txt,
    archive_file = archive_file,
    current_member = current_member,
    current_phase = current_phase,
    current_detail = current_detail,
    current_file_progress = stage_progress(current_phase),
    start_time = start_time,
    total_files = total_files
  )
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
start_time <- Sys.time()
total_files <- 0L

manifest <- data.frame(
  archive_member = character(),
  status = character(),
  started_at = character(),
  finished_at = character(),
  processing_seconds = numeric(),
  processing_time_hms = character(),
  recording_date = character(),
  recording_latitude = numeric(),
  recording_longitude = numeric(),
  predictions_csv = character(),
  summary_csv = character(),
  summary_rows = integer(),
  total_species_identified = integer(),
  species_detected = character(),
  error_message = character(),
  stringsAsFactors = FALSE
)

write_file_results_txt(manifest, file_results_txt)
write_summary_of_summaries_txt(
  manifest = manifest,
  output_path = summary_of_summaries_txt,
  archive_file = archive_file,
  current_member = "",
  current_phase = "starting",
  current_detail = "initializing archive run",
  current_file_progress = 0,
  start_time = start_time,
  total_files = total_files
)

emit_console("Starting archive run")
emit_console(sprintf("Archive: %s", archive_file))
emit_console("Scanning archive members; this can be slow for large .tar.zst files")

archive_members <- list_archive_flac_members(
  archive_path = archive_file,
  manifest = manifest,
  start_time = start_time,
  heartbeat_seconds = stage_heartbeat_seconds,
  timeout_seconds = stage_timeout_seconds
)
total_files <- length(archive_members)

update_live_progress(
  manifest = manifest,
  current_member = "",
  current_phase = "starting",
  current_detail = sprintf("archive scan complete; discovered %d .flac files", total_files),
  start_time = start_time,
  total_files = total_files
)

emit_console(sprintf("Starting archive run for %d .flac files", total_files))
emit_console(sprintf("Progress summary text: %s", summary_of_summaries_txt))
emit_console(sprintf("Per-file results text: %s", file_results_txt))

for (file_index in seq_along(archive_members)) {
  archive_member <- archive_members[file_index]
  output_paths <- output_paths_for_member(output_root, archive_member)
  file_started_at <- Sys.time()
  phase_prefix <- sprintf("[%s]", progress_label(file_index, total_files))

  emit_console(sprintf("%s [file %.1f%%] starting %s", phase_prefix, stage_progress("starting_file"), archive_member))
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = "starting_file",
    current_detail = sprintf("queued for processing: %s", archive_member),
    start_time = start_time,
    total_files = total_files
  )

  if (file.exists(output_paths$summary_csv)) {
    summary_df <- safe_read_summary_csv(output_paths$summary_csv)
    file_finished_at <- Sys.time()
    processing_seconds <- as.numeric(difftime(file_finished_at, file_started_at, units = "secs"))

    manifest <- rbind(
      manifest,
      data.frame(
        archive_member = archive_member,
        status = "skipped_existing",
        started_at = timestamp_text(file_started_at),
        finished_at = timestamp_text(file_finished_at),
        processing_seconds = processing_seconds,
        processing_time_hms = format_duration(processing_seconds),
        recording_date = if (nrow(summary_df) > 0) substr(summary_df$date_time[1], 1, 10) else "",
        recording_latitude = NA_real_,
        recording_longitude = NA_real_,
        predictions_csv = output_paths$predictions_csv,
        summary_csv = output_paths$summary_csv,
        summary_rows = nrow(summary_df),
        total_species_identified = length(unique(summary_df$scientific_name)),
        species_detected = collapse_species(summary_df),
        error_message = "",
        stringsAsFactors = FALSE
      )
    )

    write.csv(manifest, manifest_csv, row.names = FALSE)
    write_file_results_txt(manifest, file_results_txt)
    update_live_progress(
      manifest = manifest,
      current_member = archive_member,
      current_phase = "skipped_existing",
      current_detail = sprintf("using existing summary for %s", archive_member),
      start_time = start_time,
      total_files = total_files
    )

    emit_console(sprintf("%s Skipping existing results for %s", phase_prefix, archive_member))
    next
  }

  unlink(extract_root, recursive = TRUE, force = TRUE)
  dir.create(extract_root, recursive = TRUE, showWarnings = FALSE)

  result <- tryCatch(
    {
      flac_path <- file.path(extract_root, archive_member)
      dir.create(dirname(flac_path), recursive = TRUE, showWarnings = FALSE)

      run_monitored_command(
        command = "tar",
        args = c("--zstd", "-xvf", archive_file, "-C", extract_root, archive_member),
        phase_prefix = phase_prefix,
        archive_member = archive_member,
        stage = "extracting_flac",
        start_time = start_time,
        total_files = total_files,
        manifest = manifest,
        detail_text = sprintf("downloading/extracting from archive: %s", archive_member),
        heartbeat_seconds = stage_heartbeat_seconds,
        timeout_seconds = stage_timeout_seconds
      )

      if (!file.exists(flac_path)) {
        stop("tar completed without producing expected file: ", flac_path)
      }

      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = "extracted_flac",
        current_detail = sprintf("downloaded/extracted .flac: %s", basename(flac_path)),
        start_time = start_time,
        total_files = total_files
      )

      wav_path <- file.path(
        extract_root,
        sub("\\.flac$", ".wav", archive_member, ignore.case = TRUE)
      )

      run_monitored_command(
        command = "ffmpeg",
        args = c(
          "-hide_banner",
          "-nostats",
          "-progress",
          "pipe:1",
          "-y",
          "-i",
          flac_path,
          wav_path
        ),
        phase_prefix = phase_prefix,
        archive_member = archive_member,
        stage = "converting_wav",
        start_time = start_time,
        total_files = total_files,
        manifest = manifest,
        detail_text = sprintf("converting %s -> %s", basename(flac_path), basename(wav_path)),
        heartbeat_seconds = stage_heartbeat_seconds,
        timeout_seconds = stage_timeout_seconds,
        detail_parser = make_ffmpeg_progress_parser(basename(flac_path), basename(wav_path))
      )

      if (!file.exists(wav_path)) {
        stop("ffmpeg completed without producing expected file: ", wav_path)
      }

      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = "converted_wav",
        current_detail = sprintf("converted .wav ready for analysis: %s", basename(wav_path)),
        start_time = start_time,
        total_files = total_files
      )

      processed <- run_monitored_birdnet_analysis(
        script_dir = script_dir,
        pipeline_settings = pipeline_settings,
        wav_path = wav_path,
        output_dir = dirname(output_paths$summary_csv),
        output_stem = tools::file_path_sans_ext(basename(archive_member)),
        phase_prefix = phase_prefix,
        archive_member = archive_member,
        start_time = start_time,
        total_files = total_files,
        manifest = manifest,
        heartbeat_seconds = stage_heartbeat_seconds,
        timeout_seconds = stage_timeout_seconds
      )

      processed$error_message <- ""
      processed
    },
    error = function(error) {
      list(
        status = "error",
        predictions_csv = output_paths$predictions_csv,
        summary_csv = output_paths$summary_csv,
        summary_rows = NA_integer_,
        total_species_identified = NA_integer_,
        recording_date = "",
        recording_latitude = NA_real_,
        recording_longitude = NA_real_,
        summary_table = empty_summary_table(),
        error_message = conditionMessage(error)
      )
    }
  )

  emit_console(sprintf("%s [file %.1f%%] cleaning temporary files for %s", phase_prefix, stage_progress("cleaning_temp"), archive_member))
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = "cleaning_temp",
    current_detail = sprintf("cleaning temporary .flac/.wav for %s", archive_member),
    start_time = start_time,
    total_files = total_files
  )
  unlink(extract_root, recursive = TRUE, force = TRUE)

  file_finished_at <- Sys.time()
  processing_seconds <- as.numeric(difftime(file_finished_at, file_started_at, units = "secs"))
  species_detected <- collapse_species(result$summary_table)

  manifest <- rbind(
    manifest,
    data.frame(
      archive_member = archive_member,
      status = result$status,
      started_at = timestamp_text(file_started_at),
      finished_at = timestamp_text(file_finished_at),
      processing_seconds = processing_seconds,
      processing_time_hms = format_duration(processing_seconds),
      recording_date = result$recording_date,
      recording_latitude = result$recording_latitude,
      recording_longitude = result$recording_longitude,
      predictions_csv = result$predictions_csv,
      summary_csv = result$summary_csv,
      summary_rows = result$summary_rows,
      total_species_identified = result$total_species_identified,
      species_detected = species_detected,
      error_message = if (!is.null(result$error_message)) result$error_message else "",
      stringsAsFactors = FALSE
    )
  )

  write.csv(manifest, manifest_csv, row.names = FALSE)
  write_file_results_txt(manifest, file_results_txt)
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = if (identical(result$status, "error")) "error" else "completed_file",
    current_detail = if (identical(result$status, "error")) {
      sprintf("error while processing %s: %s", archive_member, result$error_message)
    } else {
      sprintf("completed %s", archive_member)
    },
    start_time = start_time,
    total_files = total_files
  )

  eta_seconds <- estimate_remaining_seconds(start_time, nrow(manifest), total_files)
  emit_console(
    sprintf(
      "%s Finished %s | status=%s | rows=%s | species=%s | file_time=%s | eta=%s",
      phase_prefix,
      archive_member,
      result$status,
      if (is.na(result$summary_rows)) "n/a" else result$summary_rows,
      if (is.na(result$total_species_identified)) "n/a" else result$total_species_identified,
      format_duration(processing_seconds),
      format_duration(eta_seconds)
    )
  )
}

write_summary_of_summaries_txt(
  manifest = manifest,
  output_path = summary_of_summaries_txt,
  archive_file = archive_file,
  current_member = "",
  current_phase = "complete",
  current_detail = "archive processing complete",
  current_file_progress = 100,
  start_time = start_time,
  total_files = total_files
)

emit_console(sprintf("Archive processing complete. Manifest: %s", manifest_csv))
manifest
