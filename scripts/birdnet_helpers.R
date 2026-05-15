library(birdnetR)

get_current_script_path <- function() {
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

  normalizePath(".")
}

build_filter_from_csv <- function(csv_path) {
  species_df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)

  if (ncol(species_df) < 2) {
    stop("species_csv must contain at least two columns for scientific and common names.")
  }

  if (all(c("scientific_name", "common_name") %in% names(species_df))) {
    scientific_name <- species_df$scientific_name
    common_name <- species_df$common_name
  } else {
    scientific_name <- species_df[[1]]
    common_name <- species_df[[2]]
  }

  scientific_name <- trimws(scientific_name)
  common_name <- trimws(common_name)
  keep_rows <- !is.na(scientific_name) & !is.na(common_name) &
    nzchar(scientific_name) & nzchar(common_name)
  species_labels <- paste(scientific_name[keep_rows], common_name[keep_rows], sep = "_")

  unique(species_labels)
}

extract_recording_coordinates <- function(audio_path,
                                          fallback_latitude = NULL,
                                          fallback_longitude = NULL) {
  matches <- regexec(
    "(-?[0-9]{1,2}\\.[0-9]+)([+-][0-9]{1,3}\\.[0-9]+)",
    basename(audio_path),
    perl = TRUE
  )
  parts <- regmatches(basename(audio_path), matches)[[1]]

  if (length(parts) == 3) {
    return(list(
      latitude = as.numeric(parts[2]),
      longitude = as.numeric(parts[3])
    ))
  }

  if (!is.null(fallback_latitude) && !is.null(fallback_longitude)) {
    return(list(
      latitude = as.numeric(fallback_latitude),
      longitude = as.numeric(fallback_longitude)
    ))
  }

  stop("Could not determine latitude/longitude from the filename and no fallback coordinates were supplied.")
}

extract_recording_date <- function(audio_path, fallback_date = NULL) {
  timestamp_text <- regmatches(
    basename(audio_path),
    regexpr("[0-9]{8}T[0-9]{6}[+-][0-9]{4}", basename(audio_path))
  )

  if (length(timestamp_text) == 1 && !is.na(timestamp_text)) {
    return(as.Date(substr(timestamp_text, 1, 8), format = "%Y%m%d"))
  }

  date_text <- regmatches(
    basename(audio_path),
    regexpr("[0-9]{8}", basename(audio_path))
  )

  if (length(date_text) == 1 && !is.na(date_text)) {
    return(as.Date(date_text, format = "%Y%m%d"))
  }

  if (!is.null(fallback_date)) {
    return(as.Date(fallback_date))
  }

  stop("Could not determine recording date from the filename and no fallback date was supplied.")
}

extract_recording_start_time <- function(audio_path,
                                         timezone = "Australia/Adelaide",
                                         fallback_date = NULL) {
  timestamp_text <- regmatches(
    basename(audio_path),
    regexpr("[0-9]{8}T[0-9]{6}[+-][0-9]{4}", basename(audio_path))
  )

  if (length(timestamp_text) == 1 && !is.na(timestamp_text)) {
    return(as.POSIXct(timestamp_text, format = "%Y%m%dT%H%M%S%z", tz = timezone))
  }

  if (!is.null(fallback_date)) {
    return(as.POSIXct(as.Date(fallback_date), tz = timezone))
  }

  stop("Could not determine recording start time from the filename and no fallback date was supplied.")
}

recording_week_from_date <- function(recording_date) {
  week <- as.integer(strftime(as.Date(recording_date), format = "%V"))
  max(1L, min(48L, week))
}

empty_summary_table <- function() {
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

initialize_birdnet_pipeline <- function(species_csv,
                                        version = "v2.4",
                                        language = "en_us",
                                        timezone = "Australia/Adelaide",
                                        fallback_latitude = NULL,
                                        fallback_longitude = NULL,
                                        fallback_date = NULL,
                                        prediction_min_confidence = 0.05,
                                        summary_confidence_threshold = 0.05,
                                        range_min_confidence = 0.03,
                                        use_arrow = TRUE) {
  list(
    species_csv = normalizePath(species_csv, mustWork = TRUE),
    version = version,
    language = language,
    timezone = timezone,
    fallback_latitude = fallback_latitude,
    fallback_longitude = fallback_longitude,
    fallback_date = fallback_date,
    prediction_min_confidence = prediction_min_confidence,
    summary_confidence_threshold = summary_confidence_threshold,
    range_min_confidence = range_min_confidence,
    use_arrow = use_arrow,
    csv_species_filter = build_filter_from_csv(species_csv),
    audio_model = birdnet_model_tflite(version = version, language = language),
    meta_model = birdnet_model_meta(version = version, language = language)
  )
}

process_audio_file <- function(pipeline,
                               audio_file,
                               output_dir = dirname(audio_file),
                               output_stem = tools::file_path_sans_ext(basename(audio_file)),
                               allow_empty_summary = TRUE,
                               progress_callback = NULL) {
  if (!is.null(progress_callback) && !is.function(progress_callback)) {
    stop("progress_callback must be NULL or a function.")
  }

  audio_file <- normalizePath(audio_file, mustWork = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  recording_coords <- extract_recording_coordinates(
    audio_file,
    fallback_latitude = pipeline$fallback_latitude,
    fallback_longitude = pipeline$fallback_longitude
  )
  recording_date <- extract_recording_date(audio_file, fallback_date = pipeline$fallback_date)
  recording_start_time <- extract_recording_start_time(
    audio_file,
    timezone = pipeline$timezone,
    fallback_date = recording_date
  )
  recording_week <- recording_week_from_date(recording_date)

  if (!is.null(progress_callback)) {
    progress_callback("building_range_filter", audio_file)
  }

  range_predictions <- predict_species_at_location_and_time(
    pipeline$meta_model,
    latitude = recording_coords$latitude,
    longitude = recording_coords$longitude,
    week = recording_week,
    min_confidence = pipeline$range_min_confidence
  )

  range_species_filter <- unique(stats::na.omit(range_predictions$label))
  species_filter <- intersect(pipeline$csv_species_filter, range_species_filter)

  if (length(species_filter) == 0) {
    stop("species_filter is empty after combining the CSV filter with the BirdNET range model.")
  }

  if (!is.null(progress_callback)) {
    progress_callback("running_birdnet", audio_file)
  }

  predictions <- predict_species_from_audio_file(
    pipeline$audio_model,
    audio_file = audio_file,
    min_confidence = pipeline$prediction_min_confidence,
    filter_species = species_filter,
    use_arrow = pipeline$use_arrow
  )
  predictions <- as.data.frame(predictions)

  prediction_columns <- c("start", "scientific_name", "common_name", "confidence")
  predictions_clean <- predictions[
    complete.cases(predictions[, prediction_columns]),
    prediction_columns,
    drop = FALSE
  ]
  predictions_clean <- predictions_clean[order(predictions_clean$start), , drop = FALSE]

  summary_predictions <- predictions_clean[
    predictions_clean$confidence >= pipeline$summary_confidence_threshold,
    ,
    drop = FALSE
  ]

  predictions_csv <- file.path(output_dir, paste0(output_stem, "_birdnet_predictions.csv"))
  summary_csv <- file.path(output_dir, paste0(output_stem, "_birdnet_species_summary.csv"))

  if (nrow(summary_predictions) == 0) {
    if (!is.null(progress_callback)) {
      progress_callback("writing_empty_summary", audio_file)
    }

    summary_table <- empty_summary_table()
    write.csv(summary_predictions, predictions_csv, row.names = FALSE)
    write.csv(summary_table, summary_csv, row.names = FALSE)

    if (!allow_empty_summary) {
      stop(
        paste0(
          "No detections met summary_confidence_threshold = ",
          pipeline$summary_confidence_threshold,
          ". Lower this value or reduce the filtering constraints."
        )
      )
    }

      return(list(
      status = if (nrow(predictions_clean) == 0) "no_usable_detections" else "no_summary_detections",
      predictions_csv = predictions_csv,
      summary_csv = summary_csv,
      summary_rows = 0L,
      total_species_identified = 0L,
      recording_date = as.character(recording_date),
      recording_latitude = recording_coords$latitude,
      recording_longitude = recording_coords$longitude,
      summary_table = summary_table
    ))
  }

  total_species_identified <- length(unique(summary_predictions$scientific_name))

  summary_table <- data.frame(
    date_time = recording_start_time + summary_predictions$start,
    scientific_name = summary_predictions$scientific_name,
    common_name = summary_predictions$common_name,
    confidence = summary_predictions$confidence,
    stringsAsFactors = FALSE
  )

  summary_table$new_species_detected <- !duplicated(summary_table$scientific_name)
  summary_table$cumulative_number_of_new_species_detected <- cumsum(summary_table$new_species_detected)
  summary_table$total_number_of_species_identified <- total_species_identified
  summary_table$date_time <- format(summary_table$date_time, "%Y-%m-%d %H:%M:%S %z")
  summary_table$new_species_detected <- NULL

  if (!is.null(progress_callback)) {
    progress_callback("writing_summary", audio_file)
  }

  write.csv(summary_predictions, predictions_csv, row.names = FALSE)
  write.csv(summary_table, summary_csv, row.names = FALSE)

  list(
    status = "ok",
    predictions_csv = predictions_csv,
    summary_csv = summary_csv,
    summary_rows = nrow(summary_table),
    total_species_identified = total_species_identified,
    recording_date = as.character(recording_date),
    recording_latitude = recording_coords$latitude,
    recording_longitude = recording_coords$longitude,
    summary_table = summary_table
  )
}
