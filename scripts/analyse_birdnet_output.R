get_current_file_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_args <- grep("^--file=", command_args, value = TRUE)

  if (length(file_args) > 0) {
    return(normalizePath(sub("^--file=", "", file_args[1])))
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(normalizePath(frame$ofile))
    }
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    active_path <- rstudioapi::getActiveDocumentContext()$path

    if (nzchar(active_path)) {
      return(normalizePath(active_path))
    }
  }

  normalizePath(".")
}

find_repo_root <- function(start_path) {
  current_path <- normalizePath(start_path, mustWork = TRUE)

  if (file.info(current_path)$isdir) {
    current_dir <- current_path
  } else {
    current_dir <- dirname(current_path)
  }

  repeat {
    has_scripts_dir <- dir.exists(file.path(current_dir, "scripts"))
    has_readme <- file.exists(file.path(current_dir, "README.md"))

    if (has_scripts_dir && has_readme) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("could not locate the repository root from the current script location")
    }

    current_dir <- parent_dir
  }
}

make_empty_summary_table <- function() {
  data.frame(
    date_time = character(),
    scientific_name = character(),
    common_name = character(),
    confidence = numeric(),
    cumulative_number_of_new_species_detected = integer(),
    total_number_of_species_identified = integer(),
    stringsAsFactors = FALSE
  )
}

read_summary_csv <- function(summary_csv) {
  summary_info <- file.info(summary_csv)

  if (is.na(summary_info$size) || summary_info$size == 0) {
    return(list(
      data = make_empty_summary_table(),
      status = "empty_file",
      message = "summary CSV exists but is empty"
    ))
  }

  summary_df <- tryCatch(
    read.csv(summary_csv, stringsAsFactors = FALSE),
    error = function(error) {
      structure(list(message = conditionMessage(error)), class = "summary_read_error")
    }
  )

  if (inherits(summary_df, "summary_read_error")) {
    return(list(
      data = make_empty_summary_table(),
      status = "read_error",
      message = summary_df$message
    ))
  }

  required_columns <- c(
    "date_time",
    "scientific_name",
    "common_name",
    "confidence",
    "cumulative_number_of_new_species_detected",
    "total_number_of_species_identified"
  )
  missing_columns <- setdiff(required_columns, names(summary_df))

  if (length(missing_columns) > 0) {
    return(list(
      data = make_empty_summary_table(),
      status = "missing_columns",
      message = paste("missing required columns:", paste(missing_columns, collapse = ", "))
    ))
  }

  summary_df <- summary_df[, required_columns, drop = FALSE]
  summary_df$date_time <- trimws(as.character(summary_df$date_time))
  summary_df$scientific_name <- trimws(as.character(summary_df$scientific_name))
  summary_df$common_name <- trimws(as.character(summary_df$common_name))
  summary_df$confidence <- suppressWarnings(as.numeric(summary_df$confidence))

  keep_rows <- !is.na(summary_df$date_time) &
    nzchar(summary_df$date_time) &
    !is.na(summary_df$scientific_name) &
    nzchar(summary_df$scientific_name) &
    !is.na(summary_df$common_name) &
    nzchar(summary_df$common_name) &
    !is.na(summary_df$confidence)

  summary_df <- summary_df[keep_rows, , drop = FALSE]

  list(
    data = summary_df,
    status = "ok",
    message = ""
  )
}

floor_to_bin <- function(date_time, bin_minutes, timezone) {
  if (!inherits(date_time, "POSIXct")) {
    stop("date_time must be POSIXct.")
  }

  bin_seconds <- bin_minutes * 60
  local_time <- as.POSIXlt(date_time, tz = timezone)
  offset_seconds <- unlist(local_time$gmtoff, use.names = FALSE)
  local_epoch_seconds <- as.numeric(date_time) + offset_seconds
  floored_local_epoch_seconds <- floor(local_epoch_seconds / bin_seconds) * bin_seconds
  floored_utc_epoch_seconds <- floored_local_epoch_seconds - offset_seconds

  as.POSIXct(floored_utc_epoch_seconds, origin = "1970-01-01", tz = timezone)
}

build_complete_time_grid <- function(start_time, end_time, bin_minutes, timezone) {
  start_bin <- floor_to_bin(start_time, bin_minutes = bin_minutes, timezone = timezone)
  end_bin <- floor_to_bin(end_time, bin_minutes = bin_minutes, timezone = timezone)

  seq(from = start_bin, to = end_bin, by = sprintf("%d mins", bin_minutes))
}

make_placeholder_plot <- function(title_text, subtitle_text, body_text) {
  ggplot2::ggplot(data.frame(x = 0.5, y = 0.5), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(label = body_text, size = 5, lineheight = 1.1) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(title = title_text, subtitle = subtitle_text, x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11)
    )
}

analysis_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )
}

normalise_common_name <- function(common_name) {
  proper_noun_replacements <- c(
    "australian" = "Australian",
    "australasian" = "Australasian",
    "eurasian" = "Eurasian",
    "european" = "European",
    "horsfield's" = "Horsfield's",
    "lewin's" = "Lewin's",
    "new holland" = "New Holland"
  )

  normalised_name <- tolower(trimws(common_name))

  for (pattern_text in names(proper_noun_replacements)) {
    replacement_text <- proper_noun_replacements[[pattern_text]]
    normalised_name <- gsub(
      paste0("\\b", pattern_text, "\\b"),
      replacement_text,
      normalised_name,
      perl = TRUE
    )
  }

  normalised_name
}

escape_plotmath_text <- function(text_value) {
  text_value <- gsub("\\\\", "\\\\\\\\", text_value)
  gsub("\"", "\\\\\"", text_value)
}

build_species_label_plotmath <- function(common_name, scientific_name) {
  paste0(
    "\"", escape_plotmath_text(common_name), "\"",
    "*\" (\"*",
    "italic(\"", escape_plotmath_text(scientific_name), "\")",
    "*\")\""
  )
}

extract_recorder_id <- function(path_text) {
  path_parts <- strsplit(normalizePath(path_text, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1]]
  recorder_hits <- unique(path_parts[grepl("^GEL_[A-Z]+$", path_parts)])

  if (length(recorder_hits) > 0) {
    return(recorder_hits[1])
  }

  file_hit <- regmatches(basename(path_text), regexpr("GEL_[A-Z]+", basename(path_text)))

  if (length(file_hit) == 1 && !is.na(file_hit) && nzchar(file_hit)) {
    return(file_hit)
  }

  "unknown"
}

calculate_diversity_metrics <- function(detection_counts) {
  detection_counts <- detection_counts[detection_counts > 0]
  total_detections <- sum(detection_counts)

  if (length(detection_counts) == 0 || total_detections <= 0) {
    return(data.frame(
      total_detections = 0,
      species_richness = 0,
      shannon_index = NA_real_,
      simpson_index = NA_real_,
      hill_q1 = NA_real_,
      hill_q2 = NA_real_
    ))
  }

  relative_abundance <- detection_counts / total_detections
  shannon_index <- -sum(relative_abundance * log(relative_abundance))
  simpson_concentration <- sum(relative_abundance^2)

  data.frame(
    total_detections = total_detections,
    species_richness = length(detection_counts),
    shannon_index = shannon_index,
    simpson_index = 1 - simpson_concentration,
    hill_q1 = exp(shannon_index),
    hill_q2 = 1 / simpson_concentration
  )
}

build_monthly_diversity_summary <- function(filtered_detections, timezone) {
  grouped_detections <- split(
    filtered_detections,
    interaction(filtered_detections$recorder_id, filtered_detections$month_start, drop = TRUE)
  )

  monthly_diversity_list <- lapply(grouped_detections, function(group_df) {
    detection_counts <- as.numeric(table(group_df$scientific_name))
    diversity_metrics <- calculate_diversity_metrics(detection_counts)

    data.frame(
      recorder_id = group_df$recorder_id[[1]],
      month_start = as.Date(group_df$month_start[[1]]),
      month_label = format(as.Date(group_df$month_start[[1]]), "%Y-%m"),
      month_of_year = format(as.Date(group_df$month_start[[1]]), "%b"),
      diversity_metrics,
      stringsAsFactors = FALSE
    )
  })

  monthly_diversity_summary <- do.call(rbind, monthly_diversity_list)
  monthly_diversity_summary <- monthly_diversity_summary[
    order(monthly_diversity_summary$recorder_id, monthly_diversity_summary$month_start),
    ,
    drop = FALSE
  ]
  monthly_diversity_summary$month_start <- as.Date(monthly_diversity_summary$month_start)
  monthly_diversity_summary
}

build_monthly_diversity_long <- function(monthly_diversity_summary) {
  do.call(
    rbind,
    list(
      data.frame(
        recorder_id = monthly_diversity_summary$recorder_id,
        month_start = monthly_diversity_summary$month_start,
        month_label = monthly_diversity_summary$month_label,
        metric_name = "Hill number (q = 1)",
        metric_value = monthly_diversity_summary$hill_q1,
        stringsAsFactors = FALSE
      ),
      data.frame(
        recorder_id = monthly_diversity_summary$recorder_id,
        month_start = monthly_diversity_summary$month_start,
        month_label = monthly_diversity_summary$month_label,
        metric_name = "Hill number (q = 2)",
        metric_value = monthly_diversity_summary$hill_q2,
        stringsAsFactors = FALSE
      ),
      data.frame(
        recorder_id = monthly_diversity_summary$recorder_id,
        month_start = monthly_diversity_summary$month_start,
        month_label = monthly_diversity_summary$month_label,
        metric_name = "Shannon index",
        metric_value = monthly_diversity_summary$shannon_index,
        stringsAsFactors = FALSE
      ),
      data.frame(
        recorder_id = monthly_diversity_summary$recorder_id,
        month_start = monthly_diversity_summary$month_start,
        month_label = monthly_diversity_summary$month_label,
        metric_name = "Simpson index",
        metric_value = monthly_diversity_summary$simpson_index,
        stringsAsFactors = FALSE
      )
    )
  )
}

build_time_series_summary_for_subset <- function(detections_subset, bin_minutes, timezone) {
  if (nrow(detections_subset) == 0) {
    return(data.frame(
      time_bin = as.POSIXct(character()),
      identification_count = integer(),
      unique_species_count = integer()
    ))
  }

  detections_subset <- detections_subset[order(detections_subset$date_time, detections_subset$scientific_name), , drop = FALSE]
  detections_subset$time_bin <- floor_to_bin(
    detections_subset$date_time,
    bin_minutes = bin_minutes,
    timezone = timezone
  )
  subset_time_grid <- build_complete_time_grid(
    start_time = min(detections_subset$date_time),
    end_time = max(detections_subset$date_time),
    bin_minutes = bin_minutes,
    timezone = timezone
  )

  detections_by_bin <- aggregate(
    list(identification_count = rep(1L, nrow(detections_subset))),
    by = list(time_bin = detections_subset$time_bin),
    FUN = sum
  )
  species_richness_by_bin <- aggregate(
    list(unique_species_count = detections_subset$scientific_name),
    by = list(time_bin = detections_subset$time_bin),
    FUN = function(x) length(unique(x))
  )

  time_series_summary <- merge(
    data.frame(time_bin = subset_time_grid),
    detections_by_bin,
    by = "time_bin",
    all.x = TRUE
  )
  time_series_summary <- merge(
    time_series_summary,
    species_richness_by_bin,
    by = "time_bin",
    all.x = TRUE
  )
  time_series_summary$identification_count[is.na(time_series_summary$identification_count)] <- 0L
  time_series_summary$unique_species_count[is.na(time_series_summary$unique_species_count)] <- 0L
  time_series_summary[order(time_series_summary$time_bin), , drop = FALSE]
}

build_cumulative_new_species_for_subset <- function(detections_subset, bin_minutes, timezone) {
  if (nrow(detections_subset) == 0) {
    return(data.frame(
      time_bin = as.POSIXct(character()),
      new_species_count = integer(),
      first_detected_species = character(),
      cumulative_new_species = integer(),
      stringsAsFactors = FALSE
    ))
  }

  detections_subset <- detections_subset[order(detections_subset$date_time, detections_subset$scientific_name), , drop = FALSE]
  subset_time_grid <- build_complete_time_grid(
    start_time = min(detections_subset$date_time),
    end_time = max(detections_subset$date_time),
    bin_minutes = bin_minutes,
    timezone = timezone
  )
  first_detections <- detections_subset[!duplicated(detections_subset$scientific_name), , drop = FALSE]
  first_detections$first_detection_bin <- floor_to_bin(
    first_detections$date_time,
    bin_minutes = bin_minutes,
    timezone = timezone
  )

  new_species_by_bin <- aggregate(
    list(new_species_count = rep(1L, nrow(first_detections))),
    by = list(time_bin = first_detections$first_detection_bin),
    FUN = sum
  )
  species_first_detected <- aggregate(
    list(first_detected_species = first_detections$common_name),
    by = list(time_bin = first_detections$first_detection_bin),
    FUN = function(x) paste(unique(x), collapse = "; ")
  )

  cumulative_new_species <- merge(
    data.frame(time_bin = subset_time_grid),
    new_species_by_bin,
    by = "time_bin",
    all.x = TRUE
  )
  cumulative_new_species <- merge(
    cumulative_new_species,
    species_first_detected,
    by = "time_bin",
    all.x = TRUE
  )
  cumulative_new_species$new_species_count[is.na(cumulative_new_species$new_species_count)] <- 0L
  cumulative_new_species$first_detected_species[is.na(cumulative_new_species$first_detected_species)] <- ""
  cumulative_new_species$cumulative_new_species <- cumsum(cumulative_new_species$new_species_count)
  cumulative_new_species
}

build_species_counts_for_subset <- function(detections_subset,
                                            species_levels = NULL,
                                            species_lookup = NULL,
                                            zero_fill = FALSE) {
  if (nrow(detections_subset) == 0 && !zero_fill) {
    return(data.frame(
      scientific_name = character(),
      common_name = character(),
      identification_count = integer(),
      species_label = character(),
      stringsAsFactors = FALSE
    ))
  }

  species_counts <- aggregate(
    list(identification_count = rep(1L, nrow(detections_subset))),
    by = list(
      scientific_name = detections_subset$scientific_name,
      common_name = detections_subset$common_name
    ),
    FUN = sum
  )
  species_counts$species_label <- paste0(species_counts$common_name, " (", species_counts$scientific_name, ")")

  if (zero_fill) {
    species_lookup_subset <- species_lookup[, c("scientific_name", "common_name", "species_label"), drop = FALSE]
    species_lookup_subset$species_label <- as.character(species_lookup_subset$species_label)
    species_counts <- merge(
      species_lookup_subset,
      species_counts[, c("species_label", "identification_count"), drop = FALSE],
      by = "species_label",
      all.x = TRUE
    )
    species_counts$identification_count[is.na(species_counts$identification_count)] <- 0L
  }

  species_counts <- species_counts[order(-species_counts$identification_count, species_counts$species_label), , drop = FALSE]

  if (!is.null(species_levels)) {
    species_counts$species_label <- factor(as.character(species_counts$species_label), levels = species_levels)
  }

  species_counts
}

build_species_counts_by_month_for_subset <- function(detections_subset,
                                                     species_lookup,
                                                     species_levels,
                                                     observed_months) {
  species_counts_by_month <- aggregate(
    list(identification_count = rep(1L, nrow(detections_subset))),
    by = list(
      month_num = detections_subset$month_num,
      month_label = detections_subset$month_label,
      scientific_name = detections_subset$scientific_name,
      common_name = detections_subset$common_name
    ),
    FUN = sum
  )
  species_counts_by_month$species_label <- paste0(
    species_counts_by_month$common_name,
    " (",
    species_counts_by_month$scientific_name,
    ")"
  )

  species_lookup_subset <- species_lookup[, c("scientific_name", "common_name", "species_label"), drop = FALSE]
  species_lookup_subset$species_label <- as.character(species_lookup_subset$species_label)

  month_species_grid <- merge(
    observed_months,
    species_lookup_subset,
    by = NULL
  )

  species_counts_by_month <- merge(
    month_species_grid,
    species_counts_by_month[, c("month_num", "species_label", "identification_count"), drop = FALSE],
    by = c("month_num", "species_label"),
    all.x = TRUE
  )
  species_counts_by_month$identification_count[is.na(species_counts_by_month$identification_count)] <- 0L
  species_counts_by_month$month_label <- factor(species_counts_by_month$month_label, levels = month.abb)
  species_counts_by_month$species_label <- factor(as.character(species_counts_by_month$species_label), levels = species_levels)
  species_counts_by_month$overall_species_order <- match(as.character(species_counts_by_month$species_label), species_levels)
  species_counts_by_month <- species_counts_by_month[
    order(species_counts_by_month$month_num, species_counts_by_month$overall_species_order),
    ,
    drop = FALSE
  ]
  species_counts_by_month$identification_count_plot <- ifelse(
    species_counts_by_month$identification_count > 0,
    species_counts_by_month$identification_count,
    NA_real_
  )
  species_counts_by_month
}

build_periodicity_frames <- function(time_series_summary, bin_minutes, periodicity_max_lag_bins) {
  bin_counts <- time_series_summary$identification_count
  periodicity_frames <- list()

  if (length(bin_counts) >= 2 && length(unique(bin_counts)) > 1) {
    acf_result <- stats::acf(
      bin_counts,
      plot = FALSE,
      lag.max = min(periodicity_max_lag_bins, length(bin_counts) - 1L),
      na.action = stats::na.pass
    )

    periodicity_frames[[length(periodicity_frames) + 1L]] <- data.frame(
      panel = "autocorrelation",
      x_value = as.numeric(acf_result$lag[, , 1]) * (bin_minutes / 60),
      y_value = as.numeric(acf_result$acf[, , 1]),
      stringsAsFactors = FALSE
    )
  }

  if (length(bin_counts) >= 4 && sum((bin_counts - mean(bin_counts))^2) > 0) {
    spectrum_result <- stats::spec.pgram(
      bin_counts,
      taper = 0,
      demean = TRUE,
      detrend = FALSE,
      plot = FALSE,
      fast = FALSE
    )
    positive_frequency <- spectrum_result$freq > 0

    periodicity_frames[[length(periodicity_frames) + 1L]] <- data.frame(
      panel = "spectral density",
      x_value = (bin_minutes / 60) / spectrum_result$freq[positive_frequency],
      y_value = spectrum_result$spec[positive_frequency],
      stringsAsFactors = FALSE
    )
  }

  if (length(periodicity_frames) == 0) {
    return(data.frame(panel = character(), x_value = numeric(), y_value = numeric(), stringsAsFactors = FALSE))
  }

  do.call(rbind, periodicity_frames)
}

write_analysis_summary <- function(summary_txt,
                                   generated_at,
                                   summary_root,
                                   output_dir,
                                   bin_minutes,
                                   min_confidence,
                                   file_status,
                                   filtered_detections) {
  unique_species <- if (nrow(filtered_detections) > 0) {
    length(unique(filtered_detections$scientific_name))
  } else {
    0L
  }

  analysis_lines <- c(
    sprintf("generated at: %s", format(generated_at, "%Y-%m-%d %H:%M:%S %z")),
    sprintf("summary root: %s", summary_root),
    sprintf("output directory: %s", output_dir),
    sprintf("temporal bin size (minutes): %s", bin_minutes),
    sprintf("minimum confidence threshold: %.3f", min_confidence),
    sprintf("summary CSV files discovered: %d", nrow(file_status)),
    sprintf("summary CSV files loaded successfully: %d", sum(file_status$read_status == "ok")),
    sprintf("summary CSV files skipped due to empty/unreadable/incomplete content: %d", sum(file_status$read_status != "ok")),
    sprintf("detections retained after confidence filter: %d", nrow(filtered_detections)),
    sprintf("unique species retained after confidence filter: %d", unique_species)
  )

  if (nrow(filtered_detections) > 0) {
    analysis_lines <- c(
      analysis_lines,
      sprintf(
        "Detection time span: %s to %s",
        format(min(filtered_detections$date_time), "%Y-%m-%d %H:%M:%S %z"),
        format(max(filtered_detections$date_time), "%Y-%m-%d %H:%M:%S %z")
      )
    )
  }

  analysis_lines <- c(
    analysis_lines,
    "",
    "This analysis uses only the summary CSV files currently present under /out/.",
    "You can rerun the script at any time while archive processing is still underway."
  )

  if (any(file_status$read_status != "ok")) {
    problem_rows <- file_status[file_status$read_status != "ok", , drop = FALSE]
    analysis_lines <- c(analysis_lines, "", "skipped or incomplete files:")

    for (row_index in seq_len(nrow(problem_rows))) {
      analysis_lines <- c(
        analysis_lines,
        sprintf(
          "- %s | %s | %s",
          problem_rows$read_status[[row_index]],
          problem_rows$summary_csv[[row_index]],
          problem_rows$message[[row_index]]
        )
      )
    }
  }

  writeLines(analysis_lines, con = summary_txt)
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 package required for analyse_birdnet_output.R; install it with install.packages('ggplot2').")
}

repo_root <- find_repo_root(get_current_file_path())

# User-defined settings ----------------------------------------------------
summary_root <- normalizePath(file.path(repo_root, "out"), mustWork = TRUE)
output_root <- file.path(summary_root, "analysis")
analysis_timezone <- "Australia/Adelaide"
bin_minutes <- 60
min_confidence <- 0.05
periodicity_max_lag_bins <- 48L
show_plots_in_session <- TRUE
# -------------------------------------------------------------------------

if (!is.numeric(bin_minutes) || length(bin_minutes) != 1 || is.na(bin_minutes) || bin_minutes <= 0) {
  stop("bin_minutes must be a single positive number")
}

if (!is.numeric(min_confidence) || length(min_confidence) != 1 || is.na(min_confidence) ||
    min_confidence < 0 || min_confidence > 1) {
  stop("min_confidence must be a single number between 0 and 1")
}

if (!is.numeric(periodicity_max_lag_bins) || length(periodicity_max_lag_bins) != 1 ||
    is.na(periodicity_max_lag_bins) || periodicity_max_lag_bins < 1) {
  stop("periodicity_max_lag_bins must be a single integer greater than or equal to 1")
}

if (!is.logical(show_plots_in_session) || length(show_plots_in_session) != 1 || is.na(show_plots_in_session)) {
  stop("show_plots_in_session must be TRUE or FALSE")
}

bin_minutes <- as.numeric(bin_minutes)
min_confidence <- as.numeric(min_confidence)
periodicity_max_lag_bins <- as.integer(periodicity_max_lag_bins)

analysis_name <- sprintf(
  "confidence_%s_bin_%smin",
  gsub("\\.", "p", format(round(min_confidence, 3), nsmall = 3, trim = TRUE)),
  format(as.integer(round(bin_minutes)), trim = TRUE)
)
output_dir <- file.path(normalizePath(output_root, mustWork = FALSE), analysis_name)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

summary_csv_files <- list.files(
  summary_root,
  pattern = "_birdnet_species_summary\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
summary_csv_files <- summary_csv_files[!grepl("/analysis/", summary_csv_files)]

if (length(summary_csv_files) == 0) {
  stop("no *_birdnet_species_summary.csv files were found under summary_root")
}

summary_results <- lapply(summary_csv_files, read_summary_csv)
file_status <- data.frame(
  summary_csv = summary_csv_files,
  read_status = vapply(summary_results, `[[`, character(1), "status"),
  rows_loaded = vapply(summary_results, function(result) nrow(result$data), integer(1)),
  message = vapply(summary_results, `[[`, character(1), "message"),
  stringsAsFactors = FALSE
)

loaded_tables <- lapply(seq_along(summary_results), function(index) {
  summary_table <- summary_results[[index]]$data

  if (nrow(summary_table) == 0) {
    return(NULL)
  }

  summary_table$source_summary_csv <- summary_csv_files[[index]]
  summary_table
})
loaded_tables <- Filter(Negate(is.null), loaded_tables)

if (length(loaded_tables) == 0) {
  stop("summary CSV files found, but none currently contain usable detections")
}

combined_detections <- do.call(rbind, loaded_tables)
combined_detections$date_time <- as.POSIXct(
  combined_detections$date_time,
  format = "%Y-%m-%d %H:%M:%S %z",
  tz = analysis_timezone
)

if (anyNA(combined_detections$date_time)) {
  bad_rows <- combined_detections[is.na(combined_detections$date_time), "source_summary_csv", drop = TRUE]
  stop(
    paste(
      "failed to parse date_time values from:",
      paste(unique(bad_rows), collapse = ", ")
    )
  )
}

combined_detections <- combined_detections[order(combined_detections$date_time, combined_detections$scientific_name), , drop = FALSE]
combined_detections$common_name <- vapply(combined_detections$common_name, normalise_common_name, character(1))
combined_detections$recorder_id <- vapply(combined_detections$source_summary_csv, extract_recorder_id, character(1))
filtered_detections <- combined_detections[combined_detections$confidence >= min_confidence, , drop = FALSE]

if (nrow(filtered_detections) == 0) {
  stop("No detections remain after applying min_confidence.")
}

filtered_detections$month_num <- as.integer(format(filtered_detections$date_time, "%m"))
filtered_detections$month_label <- factor(month.abb[filtered_detections$month_num], levels = month.abb)
filtered_detections$month_start <- as.Date(strftime(filtered_detections$date_time, "%Y-%m-01", tz = analysis_timezone))
filtered_detections$time_bin <- floor_to_bin(
  filtered_detections$date_time,
  bin_minutes = bin_minutes,
  timezone = analysis_timezone
)

time_grid <- build_complete_time_grid(
  start_time = min(filtered_detections$date_time),
  end_time = max(filtered_detections$date_time),
  bin_minutes = bin_minutes,
  timezone = analysis_timezone
)

detections_by_bin <- aggregate(
  list(identification_count = rep(1L, nrow(filtered_detections))),
  by = list(time_bin = filtered_detections$time_bin),
  FUN = sum
)

species_richness_by_bin <- aggregate(
  list(unique_species_count = filtered_detections$scientific_name),
  by = list(time_bin = filtered_detections$time_bin),
  FUN = function(x) length(unique(x))
)

time_series_summary <- merge(
  data.frame(time_bin = time_grid),
  detections_by_bin,
  by = "time_bin",
  all.x = TRUE
)
time_series_summary <- merge(
  time_series_summary,
  species_richness_by_bin,
  by = "time_bin",
  all.x = TRUE
)
time_series_summary$identification_count[is.na(time_series_summary$identification_count)] <- 0L
time_series_summary$unique_species_count[is.na(time_series_summary$unique_species_count)] <- 0L
time_series_summary <- time_series_summary[order(time_series_summary$time_bin), , drop = FALSE]

first_detections <- filtered_detections[!duplicated(filtered_detections$scientific_name), , drop = FALSE]
first_detections <- first_detections[order(first_detections$date_time, first_detections$scientific_name), , drop = FALSE]
first_detections$first_detection_bin <- floor_to_bin(
  first_detections$date_time,
  bin_minutes = bin_minutes,
  timezone = analysis_timezone
)

new_species_by_bin <- aggregate(
  list(new_species_count = rep(1L, nrow(first_detections))),
  by = list(time_bin = first_detections$first_detection_bin),
  FUN = sum
)

species_first_detected <- aggregate(
  list(first_detected_species = first_detections$common_name),
  by = list(time_bin = first_detections$first_detection_bin),
  FUN = function(x) paste(unique(x), collapse = "; ")
)

cumulative_new_species <- merge(
  data.frame(time_bin = time_grid),
  new_species_by_bin,
  by = "time_bin",
  all.x = TRUE
)
cumulative_new_species <- merge(
  cumulative_new_species,
  species_first_detected,
  by = "time_bin",
  all.x = TRUE
)
cumulative_new_species$new_species_count[is.na(cumulative_new_species$new_species_count)] <- 0L
cumulative_new_species$first_detected_species[is.na(cumulative_new_species$first_detected_species)] <- ""
cumulative_new_species$cumulative_new_species <- cumsum(cumulative_new_species$new_species_count)

species_counts <- aggregate(
  list(identification_count = rep(1L, nrow(filtered_detections))),
  by = list(
    scientific_name = filtered_detections$scientific_name,
    common_name = filtered_detections$common_name
  ),
  FUN = sum
)
species_counts <- species_counts[species_counts$identification_count >= 1, , drop = FALSE]
species_counts$species_label <- paste0(species_counts$common_name, " (", species_counts$scientific_name, ")")
species_counts$species_label_plotmath <- vapply(
  seq_len(nrow(species_counts)),
  function(index) {
    build_species_label_plotmath(
      species_counts$common_name[[index]],
      species_counts$scientific_name[[index]]
    )
  },
  character(1)
)
species_counts <- species_counts[order(-species_counts$identification_count, species_counts$species_label), , drop = FALSE]
species_counts$species_label <- factor(species_counts$species_label, levels = rev(species_counts$species_label))
global_species_levels <- levels(species_counts$species_label)
species_label_plotmath_lookup <- setNames(species_counts$species_label_plotmath, species_counts$species_label)
observed_months <- unique(filtered_detections[, c("month_num", "month_label")])
observed_months <- observed_months[order(observed_months$month_num), , drop = FALSE]

species_counts_by_month <- aggregate(
  list(identification_count = rep(1L, nrow(filtered_detections))),
  by = list(
    month_num = filtered_detections$month_num,
    month_label = filtered_detections$month_label,
    scientific_name = filtered_detections$scientific_name,
    common_name = filtered_detections$common_name
  ),
  FUN = sum
)
species_counts_by_month <- species_counts_by_month[species_counts_by_month$identification_count >= 1, , drop = FALSE]
species_counts_by_month$species_label <- paste0(
  species_counts_by_month$common_name,
  " (",
  species_counts_by_month$scientific_name,
  ")"
)
species_counts_by_month$species_label <- factor(
  species_counts_by_month$species_label,
  levels = global_species_levels
)
species_counts_by_month$overall_species_order <- match(
  as.character(species_counts_by_month$species_label),
  global_species_levels
)
species_lookup <- species_counts[, c("scientific_name", "common_name", "species_label")]
species_lookup$species_label <- as.character(species_lookup$species_label)
month_species_grid <- merge(
  observed_months,
  species_lookup,
  by = NULL
)
species_counts_by_month <- merge(
  month_species_grid,
  species_counts_by_month[, c("month_num", "species_label", "identification_count", "overall_species_order")],
  by = c("month_num", "species_label"),
  all.x = TRUE
)
species_counts_by_month$identification_count[is.na(species_counts_by_month$identification_count)] <- 0
species_counts_by_month$month_label <- factor(species_counts_by_month$month_label, levels = month.abb)
species_counts_by_month$species_label <- factor(
  species_counts_by_month$species_label,
  levels = global_species_levels
)
species_counts_by_month$overall_species_order <- match(
  as.character(species_counts_by_month$species_label),
  global_species_levels
)
species_counts_by_month <- species_counts_by_month[
  order(
    species_counts_by_month$month_num,
    species_counts_by_month$overall_species_order
  ),
  ,
  drop = FALSE
]
species_counts_by_month$identification_count_plot <- ifelse(
  species_counts_by_month$identification_count > 0,
  species_counts_by_month$identification_count,
  NA_real_
)
species_counts_by_month_positive <- species_counts_by_month[
  !is.na(species_counts_by_month$identification_count_plot),
  ,
  drop = FALSE
]

monthly_diversity_summary <- build_monthly_diversity_summary(filtered_detections, analysis_timezone)
monthly_diversity_long <- build_monthly_diversity_long(monthly_diversity_summary)
monthly_diversity_long$metric_name <- factor(
  monthly_diversity_long$metric_name,
  levels = c("Hill number (q = 1)", "Hill number (q = 2)", "Shannon index", "Simpson index")
)

overall_monthly_diversity_summary <- build_monthly_diversity_summary(
  transform(filtered_detections, recorder_id = "ALL_RECORDERS"),
  analysis_timezone
)
overall_monthly_diversity_long <- build_monthly_diversity_long(overall_monthly_diversity_summary)
overall_monthly_diversity_long$metric_name <- factor(
  overall_monthly_diversity_long$metric_name,
  levels = levels(monthly_diversity_long$metric_name)
)

recorder_ids <- sort(unique(filtered_detections$recorder_id))
recorder_output_root <- file.path(output_dir, "recorders")
dir.create(recorder_output_root, recursive = TRUE, showWarnings = FALSE)

time_series_by_recorder <- do.call(
  rbind,
  lapply(recorder_ids, function(recorder_id) {
    subset_detections <- filtered_detections[filtered_detections$recorder_id == recorder_id, , drop = FALSE]
    subset_time_series <- build_time_series_summary_for_subset(subset_detections, bin_minutes, analysis_timezone)
    subset_time_series$recorder_id <- recorder_id
    subset_time_series
  })
)

cumulative_new_species_by_recorder <- do.call(
  rbind,
  lapply(recorder_ids, function(recorder_id) {
    subset_detections <- filtered_detections[filtered_detections$recorder_id == recorder_id, , drop = FALSE]
    subset_cumulative <- build_cumulative_new_species_for_subset(subset_detections, bin_minutes, analysis_timezone)
    subset_cumulative$recorder_id <- recorder_id
    subset_cumulative
  })
)

species_counts_by_recorder <- do.call(
  rbind,
  lapply(recorder_ids, function(recorder_id) {
    subset_detections <- filtered_detections[filtered_detections$recorder_id == recorder_id, , drop = FALSE]
    subset_species_counts <- build_species_counts_for_subset(
      subset_detections,
      species_levels = global_species_levels,
      species_lookup = species_counts,
      zero_fill = TRUE
    )
    subset_species_counts$recorder_id <- recorder_id
    subset_species_counts$identification_count_plot <- ifelse(
      subset_species_counts$identification_count > 0,
      subset_species_counts$identification_count,
      NA_real_
    )
    subset_species_counts
  })
)
species_counts_by_recorder_positive <- species_counts_by_recorder[
  !is.na(species_counts_by_recorder$identification_count_plot),
  ,
  drop = FALSE
]

species_counts_by_month_by_recorder <- do.call(
  rbind,
  lapply(recorder_ids, function(recorder_id) {
    subset_detections <- filtered_detections[filtered_detections$recorder_id == recorder_id, , drop = FALSE]
    subset_observed_months <- unique(subset_detections[, c("month_num", "month_label")])
    subset_observed_months <- subset_observed_months[order(subset_observed_months$month_num), , drop = FALSE]
    subset_species_lookup <- species_counts_by_recorder_positive[
      species_counts_by_recorder_positive$recorder_id == recorder_id,
      c("scientific_name", "common_name", "species_label"),
      drop = FALSE
    ]
    subset_species_levels <- global_species_levels[global_species_levels %in% as.character(subset_species_lookup$species_label)]
    subset_species_lookup$species_label <- as.character(subset_species_lookup$species_label)

    subset_monthly_counts <- build_species_counts_by_month_for_subset(
      subset_detections,
      species_lookup = subset_species_lookup,
      species_levels = subset_species_levels,
      observed_months = subset_observed_months
    )
    subset_monthly_counts$recorder_id <- recorder_id
    subset_monthly_counts
  })
)
species_counts_by_month_by_recorder_positive <- species_counts_by_month_by_recorder[
  !is.na(species_counts_by_month_by_recorder$identification_count_plot),
  ,
  drop = FALSE
]

periodicity_by_recorder <- do.call(
  rbind,
  lapply(recorder_ids, function(recorder_id) {
    subset_time_series <- time_series_by_recorder[time_series_by_recorder$recorder_id == recorder_id, , drop = FALSE]
    subset_periodicity <- build_periodicity_frames(subset_time_series, bin_minutes, periodicity_max_lag_bins)
    if (nrow(subset_periodicity) == 0) {
      return(NULL)
    }
    subset_periodicity$recorder_id <- recorder_id
    subset_periodicity
  })
)
if (is.null(periodicity_by_recorder)) {
  periodicity_by_recorder <- data.frame(
    panel = character(),
    x_value = numeric(),
    y_value = numeric(),
    recorder_id = character(),
    stringsAsFactors = FALSE
  )
}

bin_counts <- time_series_summary$identification_count
acf_table <- data.frame(lag_bin = numeric(), lag_hours = numeric(), autocorrelation = numeric())
spectrum_table <- data.frame(frequency_cycles_per_bin = numeric(), period_bins = numeric(), period_hours = numeric(), spectral_density = numeric())

if (length(bin_counts) >= 2 && length(unique(bin_counts)) > 1) {
  acf_result <- stats::acf(
    bin_counts,
    plot = FALSE,
    lag.max = min(periodicity_max_lag_bins, length(bin_counts) - 1L),
    na.action = stats::na.pass
  )

  acf_table <- data.frame(
    lag_bin = as.numeric(acf_result$lag[, , 1]),
    lag_hours = as.numeric(acf_result$lag[, , 1]) * (bin_minutes / 60),
    autocorrelation = as.numeric(acf_result$acf[, , 1])
  )
}

if (length(bin_counts) >= 4 && sum((bin_counts - mean(bin_counts))^2) > 0) {
  spectrum_result <- stats::spec.pgram(
    bin_counts,
    taper = 0,
    demean = TRUE,
    detrend = FALSE,
    plot = FALSE,
    fast = FALSE
  )

  positive_frequency <- spectrum_result$freq > 0
  spectrum_table <- data.frame(
    frequency_cycles_per_bin = spectrum_result$freq[positive_frequency],
    period_bins = 1 / spectrum_result$freq[positive_frequency],
    period_hours = (bin_minutes / 60) / spectrum_result$freq[positive_frequency],
    spectral_density = spectrum_result$spec[positive_frequency]
  )
  spectrum_table <- spectrum_table[order(spectrum_table$period_hours), , drop = FALSE]
}

analysis_summary_txt <- file.path(output_dir, "birdnet_analysis_summary.txt")
input_files_csv <- file.path(output_dir, "birdnet_analysis_input_files.csv")
filtered_detections_csv <- file.path(output_dir, "birdnet_analysis_filtered_detections.csv")
time_series_csv <- file.path(output_dir, "birdnet_identifications_by_time_bin.csv")
time_series_by_recorder_csv <- file.path(output_dir, "birdnet_identifications_by_time_bin_by_recorder.csv")
cumulative_species_csv <- file.path(output_dir, "birdnet_cumulative_new_species_by_time_bin.csv")
cumulative_species_by_recorder_csv <- file.path(output_dir, "birdnet_cumulative_new_species_by_time_bin_by_recorder.csv")
species_counts_csv <- file.path(output_dir, "birdnet_identifications_by_species.csv")
species_counts_by_recorder_csv <- file.path(output_dir, "birdnet_identifications_by_species_by_recorder.csv")
species_counts_by_month_csv <- file.path(output_dir, "birdnet_identifications_by_species_by_month.csv")
species_counts_by_month_by_recorder_csv <- file.path(output_dir, "birdnet_identifications_by_species_by_month_by_recorder.csv")
monthly_diversity_csv <- file.path(output_dir, "birdnet_monthly_diversity_metrics.csv")
overall_monthly_diversity_csv <- file.path(output_dir, "birdnet_monthly_diversity_metrics_overall.csv")
acf_csv <- file.path(output_dir, "birdnet_identification_acf.csv")
spectrum_csv <- file.path(output_dir, "birdnet_identification_spectrum.csv")
periodicity_by_recorder_csv <- file.path(output_dir, "birdnet_identification_periodicity_by_recorder.csv")

write.csv(file_status, input_files_csv, row.names = FALSE)
write.csv(filtered_detections, filtered_detections_csv, row.names = FALSE)
write.csv(time_series_summary, time_series_csv, row.names = FALSE)
write.csv(time_series_by_recorder, time_series_by_recorder_csv, row.names = FALSE)
write.csv(cumulative_new_species, cumulative_species_csv, row.names = FALSE)
write.csv(cumulative_new_species_by_recorder, cumulative_species_by_recorder_csv, row.names = FALSE)
write.csv(species_counts, species_counts_csv, row.names = FALSE)
write.csv(species_counts_by_recorder, species_counts_by_recorder_csv, row.names = FALSE)
write.csv(species_counts_by_month, species_counts_by_month_csv, row.names = FALSE)
write.csv(species_counts_by_month_by_recorder, species_counts_by_month_by_recorder_csv, row.names = FALSE)
write.csv(monthly_diversity_summary, monthly_diversity_csv, row.names = FALSE)
write.csv(overall_monthly_diversity_summary, overall_monthly_diversity_csv, row.names = FALSE)
write.csv(acf_table, acf_csv, row.names = FALSE)
write.csv(spectrum_table, spectrum_csv, row.names = FALSE)
write.csv(periodicity_by_recorder, periodicity_by_recorder_csv, row.names = FALSE)

generated_at <- Sys.time()
write_analysis_summary(
  summary_txt = analysis_summary_txt,
  generated_at = generated_at,
  summary_root = summary_root,
  output_dir = output_dir,
  bin_minutes = bin_minutes,
  min_confidence = min_confidence,
  file_status = file_status,
  filtered_detections = filtered_detections
)

plot_subtitle <- sprintf(
  "bin size: %s min | minimum confidence: %.3f",
  as.integer(round(bin_minutes)),
  min_confidence
)

time_series_plot <- ggplot2::ggplot(
  time_series_summary,
  ggplot2::aes(x = time_bin, y = identification_count)
) +
  ggplot2::geom_col(fill = "steelblue", width = bin_minutes * 60 * 0.9) +
  ggplot2::labs(
    title = "BirdNET identifications over time",
    subtitle = plot_subtitle,
    x = "time bin",
    y = "identifications per bin"
  ) +
  analysis_plot_theme()

cumulative_species_plot <- ggplot2::ggplot(
  cumulative_new_species,
  ggplot2::aes(x = time_bin, y = cumulative_new_species)
) +
  ggplot2::geom_step(linewidth = 1.1, colour = "darkgreen") +
  ggplot2::labs(
    title = "cumulative new species detected over time",
    subtitle = plot_subtitle,
    x = "time bin",
    y = "cumulative number of new species"
  ) +
  analysis_plot_theme()

species_counts_plot <- ggplot2::ggplot(
  species_counts,
  ggplot2::aes(x = species_label, y = identification_count)
) +
  ggplot2::geom_col(fill = "tan3") +
  ggplot2::coord_flip() +
  ggplot2::scale_x_discrete(
    labels = function(x) {
      parse(text = unname(species_label_plotmath_lookup[as.character(x)]))
    }
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "identifications per species",
    subtitle = sprintf("minimum confidence: %.3f", min_confidence),
    x = "species",
    y = "number of identifications (log10 scale)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 11),
    axis.text.y = ggplot2::element_text(size = 8.4),
    panel.grid.minor = ggplot2::element_blank()
  )

species_counts_by_month_plot <- ggplot2::ggplot(
  species_counts_by_month,
  ggplot2::aes(x = species_label, y = identification_count_plot)
) +
  ggplot2::geom_col(
    data = species_counts_by_month_positive,
    fill = "tan3"
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_grid(. ~ month_label) +
  ggplot2::scale_x_discrete(
    drop = FALSE,
    labels = function(x) {
      parse(text = unname(species_label_plotmath_lookup[as.character(x)]))
    }
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "identifications per species by month",
    subtitle = sprintf("minimum confidence: %.3f", min_confidence),
    x = "species",
    y = "number of identifications (log10 scale)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 11),
    axis.text.y = ggplot2::element_text(size = 8.4),
    panel.grid.minor = ggplot2::element_blank()
  )

monthly_diversity_plot <- ggplot2::ggplot(
  overall_monthly_diversity_long,
  ggplot2::aes(x = month_start, y = metric_value, group = recorder_id)
) +
  ggplot2::geom_line(linewidth = 0.9, colour = "steelblue4") +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~metric_name, scales = "free_y", ncol = 2) +
  ggplot2::labs(
    title = "monthly diversity metrics across all recorders",
    subtitle = "detections treated as relative abundance for Shannon, Simpson, and Hill numbers",
    x = "month",
    y = "metric value"
  ) +
  ggplot2::scale_x_date(date_labels = "%Y-%m") +
  analysis_plot_theme()

time_series_by_recorder_plot <- ggplot2::ggplot(
  time_series_by_recorder,
  ggplot2::aes(x = time_bin, y = identification_count)
) +
  ggplot2::geom_col(fill = "steelblue", width = bin_minutes * 60 * 0.9) +
  ggplot2::facet_grid(recorder_id ~ ., scales = "free_y") +
  ggplot2::labs(
    title = "BirdNET identifications over time by recorder",
    subtitle = plot_subtitle,
    x = "time bin",
    y = "identifications per bin"
  ) +
  analysis_plot_theme()

cumulative_species_by_recorder_plot <- ggplot2::ggplot(
  cumulative_new_species_by_recorder,
  ggplot2::aes(x = time_bin, y = cumulative_new_species)
) +
  ggplot2::geom_step(linewidth = 1.1, colour = "darkgreen") +
  ggplot2::facet_grid(recorder_id ~ ., scales = "free_y") +
  ggplot2::labs(
    title = "cumulative new species detected over time by recorder",
    subtitle = plot_subtitle,
    x = "time bin",
    y = "cumulative number of new species"
  ) +
  analysis_plot_theme()

species_counts_by_recorder_plot <- ggplot2::ggplot(
  species_counts_by_recorder,
  ggplot2::aes(x = species_label, y = identification_count_plot)
) +
  ggplot2::geom_col(
    data = species_counts_by_recorder_positive,
    fill = "tan3"
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_grid(recorder_id ~ ., scales = "free_y", space = "free_y") +
  ggplot2::scale_x_discrete(
    drop = FALSE,
    labels = function(x) {
      parse(text = unname(species_label_plotmath_lookup[as.character(x)]))
    }
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "identifications per species by recorder",
    subtitle = sprintf("minimum confidence: %.3f", min_confidence),
    x = "species",
    y = "number of identifications (log10 scale)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 11),
    axis.text.y = ggplot2::element_text(size = 8.4),
    panel.grid.minor = ggplot2::element_blank()
  )

species_counts_by_month_by_recorder_plot <- ggplot2::ggplot(
  species_counts_by_month_by_recorder,
  ggplot2::aes(x = species_label, y = identification_count_plot)
) +
  ggplot2::geom_col(
    data = species_counts_by_month_by_recorder_positive,
    fill = "tan3"
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_grid(recorder_id ~ month_label) +
  ggplot2::scale_x_discrete(
    drop = FALSE,
    labels = function(x) {
      parse(text = unname(species_label_plotmath_lookup[as.character(x)]))
    }
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "identifications per species by recorder and month",
    subtitle = sprintf("minimum confidence: %.3f", min_confidence),
    x = "species",
    y = "number of identifications (log10 scale)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 11),
    axis.text.y = ggplot2::element_text(size = 8.4),
    panel.grid.minor = ggplot2::element_blank()
  )

monthly_diversity_by_recorder_plot <- ggplot2::ggplot(
  monthly_diversity_long,
  ggplot2::aes(x = month_start, y = metric_value, group = 1)
) +
  ggplot2::geom_line(linewidth = 0.9, colour = "steelblue4") +
  ggplot2::geom_point(size = 1.8, colour = "steelblue4") +
  ggplot2::facet_grid(recorder_id ~ metric_name, scales = "free_y") +
  ggplot2::labs(
    title = "monthly diversity metrics by recorder",
    subtitle = "detections treated as relative abundance for Shannon, Simpson, and Hill numbers",
    x = "month",
    y = "metric value"
  ) +
  ggplot2::scale_x_date(date_labels = "%Y-%m") +
  analysis_plot_theme()

periodicity_frames <- list()

if (nrow(acf_table) > 0) {
  periodicity_frames[[length(periodicity_frames) + 1L]] <- data.frame(
    panel = "autocorrelation",
    x_value = acf_table$lag_hours,
    y_value = acf_table$autocorrelation,
    stringsAsFactors = FALSE
  )
}

if (nrow(spectrum_table) > 0) {
  periodicity_frames[[length(periodicity_frames) + 1L]] <- data.frame(
    panel = "spectral density",
    x_value = spectrum_table$period_hours,
    y_value = spectrum_table$spectral_density,
    stringsAsFactors = FALSE
  )
}

if (length(periodicity_frames) > 0) {
  periodicity_data <- do.call(rbind, periodicity_frames)
  periodicity_plot <- ggplot2::ggplot(
    periodicity_data,
    ggplot2::aes(x = x_value, y = y_value)
  ) +
    ggplot2::geom_line(linewidth = 0.9, colour = "firebrick3") +
    ggplot2::facet_wrap(~panel, scales = "free", ncol = 1) +
    ggplot2::labs(
      title = "temporal periodicity of identification rates",
      subtitle = plot_subtitle,
      x = "lag / period (hours)",
      y = "metric value"
    ) +
    analysis_plot_theme()

  if (nrow(acf_table) > 0) {
    periodicity_plot <- periodicity_plot +
      ggplot2::geom_hline(
        data = data.frame(panel = "autocorrelation", yintercept = 0),
        ggplot2::aes(yintercept = yintercept),
        inherit.aes = FALSE,
        linetype = "dashed",
        colour = "grey40"
      )
  }
} else {
  periodicity_plot <- make_placeholder_plot(
    title_text = "temporal periodicity of identification rates",
    subtitle_text = plot_subtitle,
    body_text = "not enough variation or time bins are currently available\nfor autocorrelation or spectral periodicity analysis."
  )
}

if (nrow(periodicity_by_recorder) > 0) {
  periodicity_by_recorder_plot <- ggplot2::ggplot(
    periodicity_by_recorder,
    ggplot2::aes(x = x_value, y = y_value)
  ) +
    ggplot2::geom_line(linewidth = 0.9, colour = "firebrick3") +
    ggplot2::facet_grid(recorder_id ~ panel, scales = "free") +
    ggplot2::labs(
      title = "temporal periodicity of identification rates by recorder",
      subtitle = plot_subtitle,
      x = "lag / period (hours)",
      y = "metric value"
    ) +
    analysis_plot_theme()

  if (any(periodicity_by_recorder$panel == "autocorrelation")) {
    periodicity_by_recorder_plot <- periodicity_by_recorder_plot +
      ggplot2::geom_hline(
        data = unique(periodicity_by_recorder[periodicity_by_recorder$panel == "autocorrelation", "recorder_id", drop = FALSE]),
        ggplot2::aes(yintercept = 0),
        inherit.aes = FALSE,
        linetype = "dashed",
        colour = "grey40"
      )
  }
} else {
  periodicity_by_recorder_plot <- make_placeholder_plot(
    title_text = "temporal periodicity of identification rates by recorder",
    subtitle_text = plot_subtitle,
    body_text = "not enough variation or time bins are currently available\nfor recorder-specific autocorrelation or spectral analysis."
  )
}

if (isTRUE(show_plots_in_session)) {
  print(time_series_plot)
  print(cumulative_species_plot)
  print(species_counts_plot)
  print(species_counts_by_month_plot)
  print(monthly_diversity_plot)
  print(time_series_by_recorder_plot)
  print(cumulative_species_by_recorder_plot)
  print(species_counts_by_recorder_plot)
  print(species_counts_by_month_by_recorder_plot)
  print(monthly_diversity_by_recorder_plot)
  print(periodicity_plot)
  print(periodicity_by_recorder_plot)
}

ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_identifications_over_time.png"),
  plot = time_series_plot,
  width = 12,
  height = 7,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_cumulative_new_species.png"),
  plot = cumulative_species_plot,
  width = 12,
  height = 7,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_identifications_by_species.png"),
  plot = species_counts_plot,
  width = 13,
  height = 10,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_identifications_by_species_by_month.png"),
  plot = species_counts_by_month_plot,
  width = 16,
  height = 12,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_monthly_diversity_metrics.png"),
  plot = monthly_diversity_plot,
  width = 14,
  height = 10,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_identifications_over_time_by_recorder.png"),
  plot = time_series_by_recorder_plot,
  width = 14,
  height = max(7, 3 * length(recorder_ids)),
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_cumulative_new_species_by_recorder.png"),
  plot = cumulative_species_by_recorder_plot,
  width = 14,
  height = max(7, 3 * length(recorder_ids)),
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_identifications_by_species_by_recorder.png"),
  plot = species_counts_by_recorder_plot,
  width = 14,
  height = max(10, 3 * length(recorder_ids)),
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_identifications_by_species_by_month_by_recorder.png"),
  plot = species_counts_by_month_by_recorder_plot,
  width = max(16, 3 * length(unique(species_counts_by_month_by_recorder$month_label))),
  height = max(12, 3 * length(recorder_ids)),
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_monthly_diversity_metrics_by_recorder.png"),
  plot = monthly_diversity_by_recorder_plot,
  width = 16,
  height = max(8, 2.8 * length(recorder_ids)),
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_periodicity.png"),
  plot = periodicity_plot,
  width = 12,
  height = 9,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_periodicity_by_recorder.png"),
  plot = periodicity_by_recorder_plot,
  width = 14,
  height = max(8, 3 * length(recorder_ids)),
  dpi = 150
)

for (recorder_id in recorder_ids) {
  recorder_dir <- file.path(recorder_output_root, recorder_id)
  dir.create(recorder_dir, recursive = TRUE, showWarnings = FALSE)

  recorder_time_series <- time_series_by_recorder[time_series_by_recorder$recorder_id == recorder_id, , drop = FALSE]
  recorder_cumulative <- cumulative_new_species_by_recorder[
    cumulative_new_species_by_recorder$recorder_id == recorder_id,
    ,
    drop = FALSE
  ]
  recorder_species_counts <- species_counts_by_recorder_positive[
    species_counts_by_recorder_positive$recorder_id == recorder_id,
    ,
    drop = FALSE
  ]
  recorder_species_levels <- global_species_levels[global_species_levels %in% as.character(recorder_species_counts$species_label)]
  recorder_species_lookup <- species_label_plotmath_lookup[recorder_species_levels]
  recorder_species_counts$species_label <- factor(as.character(recorder_species_counts$species_label), levels = recorder_species_levels)

  recorder_species_by_month <- species_counts_by_month_by_recorder[
    species_counts_by_month_by_recorder$recorder_id == recorder_id,
    ,
    drop = FALSE
  ]
  recorder_species_by_month_positive <- recorder_species_by_month[!is.na(recorder_species_by_month$identification_count_plot), , drop = FALSE]
  recorder_species_by_month$species_label <- factor(as.character(recorder_species_by_month$species_label), levels = recorder_species_levels)
  recorder_species_by_month_positive$species_label <- factor(as.character(recorder_species_by_month_positive$species_label), levels = recorder_species_levels)

  recorder_diversity_long <- monthly_diversity_long[monthly_diversity_long$recorder_id == recorder_id, , drop = FALSE]
  recorder_periodicity <- periodicity_by_recorder[periodicity_by_recorder$recorder_id == recorder_id, , drop = FALSE]

  recorder_time_series_plot <- ggplot2::ggplot(
    recorder_time_series,
    ggplot2::aes(x = time_bin, y = identification_count)
  ) +
    ggplot2::geom_col(fill = "steelblue", width = bin_minutes * 60 * 0.9) +
    ggplot2::labs(
      title = sprintf("BirdNET identifications over time: %s", recorder_id),
      subtitle = plot_subtitle,
      x = "time bin",
      y = "identifications per bin"
    ) +
    analysis_plot_theme()

  recorder_cumulative_plot <- ggplot2::ggplot(
    recorder_cumulative,
    ggplot2::aes(x = time_bin, y = cumulative_new_species)
  ) +
    ggplot2::geom_step(linewidth = 1.1, colour = "darkgreen") +
    ggplot2::labs(
      title = sprintf("cumulative new species detected over time: %s", recorder_id),
      subtitle = plot_subtitle,
      x = "time bin",
      y = "cumulative number of new species"
    ) +
    analysis_plot_theme()

  recorder_species_plot <- ggplot2::ggplot(
    recorder_species_counts,
    ggplot2::aes(x = species_label, y = identification_count)
  ) +
    ggplot2::geom_col(fill = "tan3") +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(
      labels = function(x) {
        parse(text = unname(recorder_species_lookup[as.character(x)]))
      }
    ) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = sprintf("identifications per species: %s", recorder_id),
      subtitle = sprintf("minimum confidence: %.3f", min_confidence),
      x = "species",
      y = "number of identifications (log10 scale)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(size = 8.4),
      panel.grid.minor = ggplot2::element_blank()
    )

  recorder_species_by_month_plot <- ggplot2::ggplot(
    recorder_species_by_month,
    ggplot2::aes(x = species_label, y = identification_count_plot)
  ) +
    ggplot2::geom_col(
      data = recorder_species_by_month_positive,
      fill = "tan3"
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(. ~ month_label) +
    ggplot2::scale_x_discrete(
      drop = FALSE,
      labels = function(x) {
        parse(text = unname(recorder_species_lookup[as.character(x)]))
      }
    ) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = sprintf("identifications per species by month: %s", recorder_id),
      subtitle = sprintf("minimum confidence: %.3f", min_confidence),
      x = "species",
      y = "number of identifications (log10 scale)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(size = 8.4),
      panel.grid.minor = ggplot2::element_blank()
    )

  recorder_diversity_plot <- ggplot2::ggplot(
    recorder_diversity_long,
    ggplot2::aes(x = month_start, y = metric_value, group = 1)
  ) +
    ggplot2::geom_line(linewidth = 0.9, colour = "steelblue4") +
    ggplot2::geom_point(size = 2, colour = "steelblue4") +
    ggplot2::facet_wrap(~metric_name, scales = "free_y", ncol = 2) +
    ggplot2::labs(
      title = sprintf("monthly diversity metrics: %s", recorder_id),
      subtitle = "detections treated as relative abundance for Shannon, Simpson, and Hill numbers",
      x = "month",
      y = "metric value"
    ) +
    ggplot2::scale_x_date(date_labels = "%Y-%m") +
    analysis_plot_theme()

  if (nrow(recorder_periodicity) > 0) {
    recorder_periodicity_plot <- ggplot2::ggplot(
      recorder_periodicity,
      ggplot2::aes(x = x_value, y = y_value)
    ) +
      ggplot2::geom_line(linewidth = 0.9, colour = "firebrick3") +
      ggplot2::facet_wrap(~panel, scales = "free", ncol = 1) +
      ggplot2::labs(
        title = sprintf("temporal periodicity of identification rates: %s", recorder_id),
        subtitle = plot_subtitle,
        x = "lag / period (hours)",
        y = "metric value"
      ) +
      analysis_plot_theme()

    if (any(recorder_periodicity$panel == "autocorrelation")) {
      recorder_periodicity_plot <- recorder_periodicity_plot +
        ggplot2::geom_hline(
          data = data.frame(panel = "autocorrelation", yintercept = 0),
          ggplot2::aes(yintercept = yintercept),
          inherit.aes = FALSE,
          linetype = "dashed",
          colour = "grey40"
        )
    }
  } else {
    recorder_periodicity_plot <- make_placeholder_plot(
      title_text = sprintf("temporal periodicity of identification rates: %s", recorder_id),
      subtitle_text = plot_subtitle,
      body_text = "not enough variation or time bins are currently available\nfor recorder-specific autocorrelation or spectral analysis."
    )
  }

  ggplot2::ggsave(file.path(recorder_dir, "birdnet_identifications_over_time.png"), recorder_time_series_plot, width = 12, height = 7, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_cumulative_new_species.png"), recorder_cumulative_plot, width = 12, height = 7, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_identifications_by_species.png"), recorder_species_plot, width = 13, height = 10, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_identifications_by_species_by_month.png"), recorder_species_by_month_plot, width = 16, height = 12, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_monthly_diversity_metrics.png"), recorder_diversity_plot, width = 14, height = 10, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_periodicity.png"), recorder_periodicity_plot, width = 12, height = 9, dpi = 150)
}

message(sprintf("Analysis complete. Outputs written to: %s", output_dir))
