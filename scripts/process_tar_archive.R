# user-defined settings ----------------------------------------------------
source_mode <- "archive"  # "archive" or "ecosounds"

archive_file <- "/Volumes/bradshaw/acoustic/GEL_A/GEL_A202508202025_12312025.tar.zst"
ecosounds_workbench_url <- "https://api.ecosounds.org"
ecosounds_project_id <- 1281L
ecosounds_auth_token <- Sys.getenv("ECOSOUNDS_AUTH_TOKEN", unset = "")
ecosounds_user_name <- Sys.getenv("ECOSOUNDS_USERNAME", unset = "")
ecosounds_password <- Sys.getenv("ECOSOUNDS_PASSWORD", unset = "")
# -------------------------------------------------------------------------


get_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_args <- grep("^--file=", command_args, value = TRUE)

  if (length(file_args) > 0) {
    candidate_path <- sub("^--file=", "", file_args[1])

    if (nzchar(candidate_path) && candidate_path != "-" && file.exists(candidate_path)) {
      return(dirname(normalizePath(candidate_path)))
    }
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile) && nzchar(frame$ofile) && file.exists(frame$ofile)) {
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

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required for EcoSounds downloads. Install it with install.packages('jsonlite').")
}


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

if (!source_mode %in% c("archive", "ecosounds")) {
  stop("source_mode must be either 'archive' or 'ecosounds'.")
}

archive_file <- if (identical(source_mode, "archive")) {
  normalizePath(archive_file, mustWork = TRUE)
} else {
  archive_file
}

source_name <- if (identical(source_mode, "archive")) {
  sub("\\.tar\\.zst$", "", basename(archive_file), ignore.case = TRUE)
} else {
  sprintf("ecosounds_project_%s", as.integer(ecosounds_project_id))
}
source_description <- if (identical(source_mode, "archive")) {
  archive_file
} else {
  sprintf("EcoSounds project %s via %s", as.integer(ecosounds_project_id), ecosounds_workbench_url)
}
output_root <- file.path(
  script_dir,
  "..",
  "out",
  paste0(source_name, "_birdnet_output")
)
extract_root <- file.path(tempdir(), source_name, "audio_work")
manifest_csv <- file.path(output_root, paste0(source_name, "_processing_manifest.csv"))
file_results_txt <- file.path(output_root, paste0(source_name, "_file_results.txt"))
summary_of_summaries_txt <- file.path(output_root, paste0(source_name, "_summary_of_summaries.txt"))
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

safe_file_component <- function(text_value) {
  text_value <- gsub("[^-_A-Za-z0-9]", "", text_value)

  if (!nzchar(text_value)) {
    return("unknown")
  }

  text_value
}

canonical_recording_key <- function(path_text) {
  candidate <- basename(as.character(path_text))
  candidate <- sub("_birdnet_species_summary\\.csv$", "", candidate)
  candidate <- sub("_birdnet_predictions\\.csv$", "", candidate)
  candidate <- sub("\\.(wav|flac|mp3|aif|aiff|ogg|m4a|mp4)$", "", candidate, ignore.case = TRUE)
  candidate <- sub("^recording_[0-9]+_", "", candidate)
  candidate
}

ecosounds_archive_member <- function(recording) {
  canonical_name <- basename(as.character(recording[["canonical_file_name"]]))
  file.path(
    sprintf("site_%s", as.integer(recording[["site_id"]])),
    sprintf("recording_%s_%s", as.integer(recording[["id"]]), canonical_name)
  )
}

ecosounds_filter_object <- function(project_id) {
  list(
    filter = list(
      and = list(
        "projects.id" = list(eq = as.integer(project_id))
      )
    ),
    projection = list(
      only = c("id", "recorded_date", "sites.name", "site_id", "canonical_file_name")
    )
  )
}

run_curl_request <- function(url,
                             method = "GET",
                             headers = character(),
                             body_json = NULL,
                             output_file = NULL) {
  response_file <- if (is.null(output_file)) tempfile(pattern = "curl-response-", fileext = ".tmp") else output_file
  args <- c("-sS", "-L", "-X", method)

  for (header in headers) {
    args <- c(args, "-H", header)
  }

  if (!is.null(body_json)) {
    args <- c(args, "--data-binary", body_json)
  }

  args <- c(args, "-o", response_file, "-w", "%{http_code}", url)
  response <- processx::run("curl", args = args, error_on_status = FALSE)

  if (!identical(response$status, 0L)) {
    stop(
      paste(
        c(
          sprintf("curl failed for %s", url),
          trimws(response$stderr)
        ),
        collapse = "\n"
      )
    )
  }

  list(
    status_code = suppressWarnings(as.integer(trimws(response$stdout))),
    response_file = response_file
  )
}

ecosounds_get_auth_token <- function(workbench_url, auth_token = "", user_name = "", password = "") {
  if (nzchar(auth_token)) {
    return(auth_token)
  }

  if (!nzchar(user_name) || !nzchar(password)) {
    stop(
      paste(
        "EcoSounds access requires either ecosounds_auth_token,",
        "or both ecosounds_user_name and ecosounds_password."
      )
    )
  }

  login_payload <- if (grepl("@", user_name, fixed = TRUE)) {
    list(email = user_name, password = password)
  } else {
    list(login = user_name, password = password)
  }

  login_response <- run_curl_request(
    url = sprintf("%s/security", sub("/$", "", workbench_url)),
    method = "POST",
    headers = c("Content-Type: application/json", "Accept: application/json"),
    body_json = jsonlite::toJSON(login_payload, auto_unbox = TRUE)
  )

  if (!identical(login_response$status_code, 200L)) {
    stop(sprintf("EcoSounds login failed with HTTP status %s.", login_response$status_code))
  }

  login_content <- jsonlite::fromJSON(login_response$response_file)
  token <- login_content$data$auth_token

  if (!nzchar(token)) {
    stop("EcoSounds login succeeded but no auth_token was returned.")
  }

  token
}

list_ecosounds_recordings <- function(workbench_url,
                                      auth_token,
                                      project_id,
                                      manifest,
                                      start_time) {
  page <- 1L
  max_page <- Inf
  records <- list()
  base_url <- sub("/$", "", workbench_url)
  json_headers <- c(
    sprintf("Authorization: Token token=\"%s\"", auth_token),
    "Content-Type: application/json",
    "Accept: application/json"
  )
  filter_json <- jsonlite::toJSON(ecosounds_filter_object(project_id), auto_unbox = TRUE)

  while (page <= max_page) {
    page_response <- run_curl_request(
      url = sprintf("%s/audio_recordings/filter?page=%d", base_url, page),
      method = "POST",
      headers = json_headers,
      body_json = filter_json
    )

    if (!identical(page_response$status_code, 200L)) {
      stop(sprintf("EcoSounds recording listing failed on page %d with HTTP status %s.", page, page_response$status_code))
    }

    page_content <- jsonlite::fromJSON(page_response$response_file)
    max_page <- page_content$meta$paging$max_page
    page_records <- page_content$data
    records[[length(records) + 1L]] <- if (is.null(page_records)) {
      data.frame()
    } else {
      as.data.frame(page_records, stringsAsFactors = FALSE)
    }

    recordings_found <- sum(vapply(records, nrow, integer(1)))
    detail_text <- sprintf(
      "listing EcoSounds project %s | page=%d/%s | recordings_found_so_far=%d",
      as.integer(project_id),
      page,
      max_page,
      recordings_found
    )
    emit_console(sprintf("[ecosounds list] %s", detail_text))
    update_live_progress(
      manifest = manifest,
      current_member = "",
      current_phase = "listing_archive",
      current_detail = detail_text,
      start_time = start_time,
      total_files = recordings_found
    )

    page <- page + 1L
  }

  non_empty_records <- Filter(function(x) !is.null(x) && nrow(x) > 0, records)
  if (length(non_empty_records) == 0) {
    return(data.frame(
      id = integer(),
      recorded_date = character(),
      site_id = integer(),
      canonical_file_name = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, non_empty_records)
}

download_ecosounds_recording <- function(recording,
                                         workbench_url,
                                         auth_token,
                                         download_root) {
  relative_item <- ecosounds_archive_member(recording)
  local_path <- file.path(download_root, relative_item)
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)

  download_response <- run_curl_request(
    url = sprintf("%s/audio_recordings/%s/original", sub("/$", "", workbench_url), recording[["id"]]),
    method = "GET",
    headers = sprintf("Authorization: Token token=\"%s\"", auth_token),
    output_file = local_path
  )

  if (!identical(download_response$status_code, 200L)) {
    if (file.exists(local_path)) {
      unlink(local_path, force = TRUE)
    }
    stop(
      sprintf(
        "EcoSounds download failed for recording %s with HTTP status %s.",
        recording[["id"]],
        download_response$status_code
      )
    )
  }

  list(
    archive_member = relative_item,
    local_audio_path = local_path,
    local_workspace = download_root
  )
}

cleanup_local_workspace <- function(workspace_path) {
  if (!is.null(workspace_path) && nzchar(workspace_path) && dir.exists(workspace_path)) {
    unlink(workspace_path, recursive = TRUE, force = TRUE)
  }
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
    downloading_audio = 10,
    downloaded_audio = 20,
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

filter_birdnet_progress_lines <- function(lines) {
  if (length(lines) == 0) {
    return(lines)
  }

  excluded_pattern <- paste0(
    "^WARNING: Attempting to use a delegate that only supports static-sized tensors ",
    "with a graph that has dynamic-sized tensors \\(tensor#187 is a dynamic-sized tensor\\)\\.?$"
  )

  lines[!grepl(excluded_pattern, trimws(lines))]
}

consume_stream_text <- function(buffer, text) {
  if (length(text) == 0 || is.null(text) || !nzchar(text)) {
    return(list(lines = character(), buffer = buffer))
  }

  combined <- paste0(buffer, text)
  pieces <- unlist(strsplit(combined, "\n", fixed = TRUE), use.names = FALSE)

  if (endsWith(combined, "\n")) {
    complete <- pieces
    buffer <- ""
  } else {
    if (length(pieces) == 0) {
      complete <- character()
      buffer <- combined
    } else {
      complete <- pieces[-length(pieces)]
      buffer <- pieces[length(pieces)]
    }
  }

  list(
    lines = sanitize_stream_lines(complete),
    buffer = buffer
  )
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
      output_lines <- c(output_lines, filter_birdnet_progress_lines(c(stdout_lines, stderr_lines)))
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
  output_lines <- c(output_lines, filter_birdnet_progress_lines(c(stdout_lines, stderr_lines)))

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
  listing_buffer <- ""

  on.exit({
    if (process$is_alive()) {
      process$kill_tree()
    }
  }, add = TRUE)

  repeat {
    process$poll_io(1000)

    output_text <- process$read_output()
    parsed_output <- consume_stream_text(listing_buffer, output_text)
    output_lines <- parsed_output$lines
    listing_buffer <- parsed_output$buffer
    if (length(output_lines) > 0) {
      listing_lines <- c(listing_lines, output_lines)
    }

    now <- Sys.time()
    stage_elapsed_seconds <- as.numeric(difftime(now, start_stage_at, units = "secs"))
    member_count <- length(listing_lines)
    flac_count <- sum(grepl("\\.flac$", listing_lines, ignore.case = TRUE))
    latest_member <- if (member_count > 0) tail(listing_lines, 1) else "none"
    detail_text <- sprintf(
      "scanning archive members | members_seen=%d | flac_found_so_far=%d | latest_member=%s",
      member_count,
      flac_count,
      latest_member
    )

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

  parsed_output <- consume_stream_text(listing_buffer, process$read_output())
  listing_lines <- c(listing_lines, parsed_output$lines)
  if (nzchar(parsed_output$buffer)) {
    listing_lines <- c(listing_lines, sanitize_stream_lines(parsed_output$buffer))
  }

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

build_existing_summary_index <- function(out_root) {
  summary_csv_files <- list.files(
    out_root,
    pattern = "_birdnet_species_summary\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  summary_csv_files <- summary_csv_files[!grepl("/analysis/", summary_csv_files)]
  summary_csv_files <- summary_csv_files[file.exists(summary_csv_files)]

  if (length(summary_csv_files) == 0) {
    return(data.frame(
      recording_key = character(),
      summary_csv = character(),
      predictions_csv = character(),
      stringsAsFactors = FALSE
    ))
  }

  summary_index <- data.frame(
    recording_key = vapply(summary_csv_files, canonical_recording_key, character(1)),
    summary_csv = summary_csv_files,
    predictions_csv = sub(
      "_birdnet_species_summary\\.csv$",
      "_birdnet_predictions.csv",
      summary_csv_files
    ),
    stringsAsFactors = FALSE
  )
  summary_index <- summary_index[!duplicated(summary_index$recording_key), , drop = FALSE]
  summary_index
}

find_existing_output_paths <- function(archive_member, output_paths, summary_index) {
  if (file.exists(output_paths$summary_csv)) {
    return(output_paths)
  }

  recording_key <- canonical_recording_key(archive_member)
  matching_rows <- summary_index[summary_index$recording_key == recording_key, , drop = FALSE]

  if (nrow(matching_rows) == 0) {
    return(NULL)
  }

  list(
    predictions_csv = matching_rows$predictions_csv[[1]],
    summary_csv = matching_rows$summary_csv[[1]]
  )
}

add_summary_to_index <- function(summary_index, archive_member, output_paths) {
  if (is.null(output_paths$summary_csv) || !file.exists(output_paths$summary_csv)) {
    return(summary_index)
  }

  recording_key <- canonical_recording_key(archive_member)

  if (recording_key %in% summary_index$recording_key) {
    return(summary_index)
  }

  rbind(
    summary_index,
    data.frame(
      recording_key = recording_key,
      summary_csv = output_paths$summary_csv,
      predictions_csv = output_paths$predictions_csv,
      stringsAsFactors = FALSE
    )
  )
}

write_file_results_txt <- function(manifest, output_path) {
  lines <- c(
    "BirdNET processing results by file",
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
                                           source_description,
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
    "BirdNET processing summary of summaries",
    paste("Source:", source_description),
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
    source_description = source_description,
    current_member = current_member,
    current_phase = current_phase,
    current_detail = current_detail,
    current_file_progress = stage_progress(current_phase),
    start_time = start_time,
    total_files = total_files
  )
}

parse_archive_stream_event <- function(line) {
  fields <- strsplit(line, "\t", fixed = TRUE)[[1]]
  event <- fields[1]

  if (identical(event, "SCAN") && length(fields) >= 4) {
    return(list(
      event = event,
      members_seen = as.integer(fields[2]),
      flac_found_so_far = as.integer(fields[3]),
      latest_member = fields[4]
    ))
  }

  if (identical(event, "FILE") && length(fields) >= 5) {
    return(list(
      event = event,
      members_seen = as.integer(fields[2]),
      flac_found_so_far = as.integer(fields[3]),
      archive_member = fields[4],
      flac_path = fields[5]
    ))
  }

  if (identical(event, "COMPLETE") && length(fields) >= 3) {
    return(list(
      event = event,
      members_seen = as.integer(fields[2]),
      flac_found_so_far = as.integer(fields[3])
    ))
  }

  if (identical(event, "ERROR")) {
    return(list(
      event = event,
      error_message = paste(fields[-1], collapse = "\t")
    ))
  }

  list(event = "RAW", raw = line)
}

append_skipped_existing_manifest <- function(manifest,
                                             archive_member,
                                             output_paths,
                                             file_started_at,
                                             total_files,
                                             start_time) {
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
  existing_summary_index <<- add_summary_to_index(existing_summary_index, archive_member, output_paths)
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = "skipped_existing",
    current_detail = sprintf("using existing summary for %s", archive_member),
    start_time = start_time,
    total_files = total_files
  )

  manifest
}

process_local_audio_item <- function(archive_member,
                                     input_audio_path,
                                     file_index,
                                     total_files,
                                     manifest,
                                     start_time,
                                     ready_phase,
                                     ready_detail,
                                     cleanup_input = TRUE) {
  output_paths <- output_paths_for_member(output_root, archive_member)
  file_started_at <- Sys.time()
  phase_prefix <- sprintf("[%s]", progress_label(file_index, total_files))

  emit_console(sprintf("%s [file %.1f%%] starting %s", phase_prefix, stage_progress("starting_file"), archive_member))
  update_live_progress(
    manifest = manifest,
    current_member = archive_member,
    current_phase = "starting_file",
    current_detail = sprintf("ready for processing: %s", archive_member),
    start_time = start_time,
    total_files = total_files
  )

  existing_output_paths <- find_existing_output_paths(
    archive_member = archive_member,
    output_paths = output_paths,
    summary_index = existing_summary_index
  )

  if (!is.null(existing_output_paths)) {
    manifest <- append_skipped_existing_manifest(
      manifest = manifest,
      archive_member = archive_member,
      output_paths = existing_output_paths,
      file_started_at = file_started_at,
      total_files = total_files,
      start_time = start_time
    )
    emit_console(sprintf("%s Skipping existing results for %s using %s", phase_prefix, archive_member, existing_output_paths$summary_csv))
    if (cleanup_input && file.exists(input_audio_path)) {
      unlink(input_audio_path, force = TRUE)
    }
    return(manifest)
  }

  wav_path <- input_audio_path
  result <- tryCatch(
    {
      update_live_progress(
        manifest = manifest,
        current_member = archive_member,
        current_phase = ready_phase,
        current_detail = ready_detail,
        start_time = start_time,
        total_files = total_files
      )

      input_extension <- tolower(tools::file_ext(input_audio_path))

      if (!identical(input_extension, "wav")) {
        wav_path <- file.path(
          dirname(input_audio_path),
          paste0(tools::file_path_sans_ext(basename(input_audio_path)), ".wav")
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
            input_audio_path,
            wav_path
          ),
          phase_prefix = phase_prefix,
          archive_member = archive_member,
          stage = "converting_wav",
          start_time = start_time,
          total_files = total_files,
          manifest = manifest,
          detail_text = sprintf("converting %s -> %s", basename(input_audio_path), basename(wav_path)),
          heartbeat_seconds = stage_heartbeat_seconds,
          timeout_seconds = stage_timeout_seconds,
          detail_parser = make_ffmpeg_progress_parser(basename(input_audio_path), basename(wav_path))
        )

        if (!file.exists(wav_path)) {
          stop("ffmpeg completed without producing expected file: ", wav_path)
        }
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
    current_detail = sprintf("cleaning temporary audio files for %s", archive_member),
    start_time = start_time,
    total_files = total_files
  )
  if (cleanup_input && file.exists(input_audio_path)) {
    unlink(input_audio_path, force = TRUE)
  }
  if (!identical(normalizePath(wav_path, winslash = "/", mustWork = FALSE), normalizePath(input_audio_path, winslash = "/", mustWork = FALSE)) &&
      file.exists(wav_path)) {
    unlink(wav_path, force = TRUE)
  }

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
  existing_summary_index <<- add_summary_to_index(existing_summary_index, archive_member, result)
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

  manifest
}

process_streamed_flac <- function(archive_member,
                                  flac_path,
                                  file_index,
                                  total_files,
                                  manifest,
                                  start_time) {
  process_local_audio_item(
    archive_member = archive_member,
    input_audio_path = flac_path,
    file_index = file_index,
    total_files = total_files,
    manifest = manifest,
    start_time = start_time,
    ready_phase = "extracted_flac",
    ready_detail = sprintf("stream-extracted audio ready: %s", basename(flac_path)),
    cleanup_input = TRUE
  )
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
existing_summary_index <- build_existing_summary_index(file.path(script_dir, "..", "out"))
start_time <- Sys.time()
total_files <- 0L
members_seen <- 0L
scan_complete <- FALSE

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
  source_description = source_description,
  current_member = "",
  current_phase = "starting",
  current_detail = if (identical(source_mode, "archive")) "initializing archive run" else "initializing EcoSounds run",
  current_file_progress = 0,
  start_time = start_time,
  total_files = total_files
)

emit_console("Starting BirdNET source processing run")
emit_console(sprintf("Source: %s", source_description))
emit_console(sprintf("Progress summary text: %s", summary_of_summaries_txt))
emit_console(sprintf("Per-file results text: %s", file_results_txt))

unlink(extract_root, recursive = TRUE, force = TRUE)
dir.create(extract_root, recursive = TRUE, showWarnings = FALSE)

if (identical(source_mode, "archive")) {
  emit_console("Streaming archive members; processing starts as soon as a .flac is found")

  stream_helper <- processx::process$new(
    command = "python",
    args = c(
      file.path(script_dir, "stream_archive_flacs.py"),
      archive_file,
      extract_root,
      as.character(stage_heartbeat_seconds)
    ),
    stdin = "|",
    stdout = "|",
    stderr = "|",
    cleanup_tree = TRUE
  )

  on.exit({
    if (exists("stream_helper") && stream_helper$is_alive()) {
      try(stream_helper$write_input("STOP\n"), silent = TRUE)
      try(stream_helper$kill_tree(), silent = TRUE)
    }
  }, add = TRUE)

  repeat {
    stream_helper$poll_io(1000)

    helper_stdout <- stream_helper$read_output_lines()
    helper_stderr <- stream_helper$read_error_lines()

    if (length(helper_stderr) > 0) {
      emit_console(sprintf("[archive stream] helper stderr: %s", paste(helper_stderr, collapse = " | ")))
    }

    if (length(helper_stdout) > 0) {
      for (line in helper_stdout) {
        event <- parse_archive_stream_event(line)

        if (identical(event$event, "SCAN")) {
          members_seen <- event$members_seen
          total_files <- max(total_files, event$flac_found_so_far)
          detail_text <- sprintf(
            "streaming archive | members_seen=%d | flac_found_so_far=%d | latest_member=%s",
            event$members_seen,
            event$flac_found_so_far,
            event$latest_member
          )
          emit_console(sprintf("[archive stream] %s", detail_text))
          update_live_progress(
            manifest = manifest,
            current_member = "",
            current_phase = "listing_archive",
            current_detail = detail_text,
            start_time = start_time,
            total_files = total_files
          )
        } else if (identical(event$event, "FILE")) {
          members_seen <- event$members_seen
          total_files <- max(total_files, event$flac_found_so_far)
          manifest <- process_streamed_flac(
            archive_member = event$archive_member,
            flac_path = event$flac_path,
            file_index = event$flac_found_so_far,
            total_files = total_files,
            manifest = manifest,
            start_time = start_time
          )
          stream_helper$write_input("NEXT\n")
        } else if (identical(event$event, "COMPLETE")) {
          members_seen <- event$members_seen
          total_files <- event$flac_found_so_far
          scan_complete <- TRUE
          emit_console(sprintf("Archive stream complete. Members seen: %d, flac files found: %d", members_seen, total_files))
          update_live_progress(
            manifest = manifest,
            current_member = "",
            current_phase = "starting",
            current_detail = sprintf("archive stream complete; discovered %d .flac files", total_files),
            start_time = start_time,
            total_files = total_files
          )
        } else if (identical(event$event, "ERROR")) {
          stop(event$error_message)
        }
      }
    }

    if (!stream_helper$is_alive()) {
      break
    }
  }

  if (!scan_complete) {
    final_status <- stream_helper$get_exit_status()
    if (!identical(final_status, 0L)) {
      helper_tail <- c(stream_helper$read_output_lines(), stream_helper$read_error_lines())
      stop(
        paste(
          c("archive stream helper failed", utils::tail(helper_tail, 20)),
          collapse = "\n"
        )
      )
    }
  }
} else {
  emit_console(sprintf("Listing recordings from EcoSounds project %s", as.integer(ecosounds_project_id)))
  ecosounds_token <- ecosounds_get_auth_token(
    workbench_url = ecosounds_workbench_url,
    auth_token = ecosounds_auth_token,
    user_name = ecosounds_user_name,
    password = ecosounds_password
  )
  recordings <- list_ecosounds_recordings(
    workbench_url = ecosounds_workbench_url,
    auth_token = ecosounds_token,
    project_id = ecosounds_project_id,
    manifest = manifest,
    start_time = start_time
  )

  if (nrow(recordings) == 0) {
    stop(sprintf("No accessible recordings were returned for EcoSounds project %s.", as.integer(ecosounds_project_id)))
  }

  recordings <- recordings[order(recordings$recorded_date, recordings$site_id, recordings$canonical_file_name), , drop = FALSE]
  total_files <- nrow(recordings)
  emit_console(sprintf("EcoSounds listing complete. Recordings found: %d", total_files))

  for (recording_index in seq_len(total_files)) {
    recording <- recordings[recording_index, , drop = FALSE]
    archive_member <- ecosounds_archive_member(recording)
    output_paths <- output_paths_for_member(output_root, archive_member)
    existing_output_paths <- find_existing_output_paths(
      archive_member = archive_member,
      output_paths = output_paths,
      summary_index = existing_summary_index
    )
    phase_prefix <- sprintf("[%s]", progress_label(recording_index, total_files))

    if (!is.null(existing_output_paths)) {
      file_started_at <- Sys.time()
      manifest <- append_skipped_existing_manifest(
        manifest = manifest,
        archive_member = archive_member,
        output_paths = existing_output_paths,
        file_started_at = file_started_at,
        total_files = total_files,
        start_time = start_time
      )
      emit_console(sprintf("%s Skipping existing results for %s using %s", phase_prefix, archive_member, existing_output_paths$summary_csv))
      next
    }

    emit_console(sprintf("%s [file %.1f%%] downloading %s", phase_prefix, stage_progress("downloading_audio"), archive_member))
    update_live_progress(
      manifest = manifest,
      current_member = archive_member,
      current_phase = "downloading_audio",
      current_detail = sprintf("downloading audio from EcoSounds: %s", archive_member),
      start_time = start_time,
      total_files = total_files
    )

    recording_workspace <- file.path(extract_root, sprintf("recording_%s", recording[["id"]]))
    cleanup_local_workspace(recording_workspace)
    dir.create(recording_workspace, recursive = TRUE, showWarnings = FALSE)

    downloaded_recording <- NULL
    manifest <- tryCatch(
      {
        downloaded_recording <- download_ecosounds_recording(
          recording = recording,
          workbench_url = ecosounds_workbench_url,
          auth_token = ecosounds_token,
          download_root = recording_workspace
        )

        updated_manifest <- process_local_audio_item(
          archive_member = downloaded_recording$archive_member,
          input_audio_path = downloaded_recording$local_audio_path,
          file_index = recording_index,
          total_files = total_files,
          manifest = manifest,
          start_time = start_time,
          ready_phase = "downloaded_audio",
          ready_detail = sprintf("downloaded audio ready: %s", basename(downloaded_recording$local_audio_path)),
          cleanup_input = TRUE
        )

        if (file.exists(downloaded_recording$local_audio_path)) {
          stop(sprintf("Downloaded local audio was not removed after processing: %s", downloaded_recording$local_audio_path))
        }

        updated_manifest
      },
      finally = {
        cleanup_local_workspace(recording_workspace)
      }
    )
  }
}

write_summary_of_summaries_txt(
  manifest = manifest,
  output_path = summary_of_summaries_txt,
  source_description = source_description,
  current_member = "",
  current_phase = "complete",
  current_detail = "source processing complete",
  current_file_progress = 100,
  start_time = start_time,
  total_files = total_files
)

emit_console(sprintf("Source processing complete. Manifest: %s", manifest_csv))
manifest
