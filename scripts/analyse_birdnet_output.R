# user-defined settings ----------------------------------------------------
analysis_timezone <- "Australia/Adelaide"
bin_minutes <- 60
top_species_time_bin_minutes <- 4 * 7 * 24 * 60
rolling_mean_window_days <- 30
min_confidence <- 0.1
periodicity_max_lag_bins <- 48L
show_plots_in_session <- TRUE
# -------------------------------------------------------------------------

get_current_file_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_args <- grep("^--file=", command_args, value = TRUE)

  if (length(file_args) > 0) {
    candidate_path <- sub("^--file=", "", file_args[1])

    if (nzchar(candidate_path) && candidate_path != "-" && file.exists(candidate_path)) {
      return(normalizePath(candidate_path))
    }
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile) && nzchar(frame$ofile) && file.exists(frame$ofile)) {
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

  if (bin_minutes %% (24 * 60) == 0) {
    day_step <- bin_minutes / (24 * 60)
    local_days <- seq(
      from = as.Date(start_bin, tz = timezone),
      to = as.Date(end_bin, tz = timezone),
      by = sprintf("%d days", day_step)
    )
    return(as.POSIXct(format(local_days, "%Y-%m-%d"), tz = timezone))
  }

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
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )
}

top_species_plot_theme <- function() {
  analysis_plot_theme() +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.height = grid::unit(0.35, "cm"),
      legend.key.width = grid::unit(0.65, "cm"),
      legend.box.spacing = grid::unit(0.1, "cm"),
      legend.spacing.x = grid::unit(0.1, "cm")
    )
}

build_species_label_parser <- function(plotmath_lookup) {
  function(x) {
    parse(text = unname(plotmath_lookup[as.character(x)]))
  }
}

top_species_style_values <- function(species_levels) {
  species_levels <- unique(as.character(species_levels))

  if (length(species_levels) == 0L) {
    return(list(
      colours = setNames(character(0), character(0)),
      linetypes = setNames(character(0), character(0)),
      linewidths = setNames(numeric(0), character(0)),
      shapes = setNames(numeric(0), character(0)),
      point_sizes = setNames(numeric(0), character(0))
    ))
  }

  line_colours <- grDevices::hcl.colors(length(species_levels), palette = "Dark 3")
  line_types <- rep(
    c("solid", "longdash", "dashed", "dotdash", "twodash", "dotted"),
    length.out = length(species_levels)
  )
  line_widths <- rep(c(0.8, 1.0, 1.2, 1.4), length.out = length(species_levels))
  point_shapes <- rep(c(16, 17, 15, 18, 3, 7, 8, 0, 1, 2), length.out = length(species_levels))
  point_sizes <- rep(c(1.8, 2.1, 2.4, 2.7, 2.0, 2.3, 2.6, 2.2, 2.5, 2.8), length.out = length(species_levels))

  list(
    colours = stats::setNames(line_colours, species_levels),
    linetypes = stats::setNames(line_types, species_levels),
    linewidths = stats::setNames(line_widths, species_levels),
    shapes = stats::setNames(point_shapes, species_levels),
    point_sizes = stats::setNames(point_sizes, species_levels)
  )
}

top_species_scale_layers <- function(style_values, label_parser, legend_rows = 2L) {
  species_levels <- names(style_values$colours)

  list(
    ggplot2::scale_colour_manual(
      values = style_values$colours,
      breaks = species_levels,
      labels = label_parser,
      guide = ggplot2::guide_legend(
        nrow = legend_rows,
        byrow = TRUE,
        override.aes = list(
          linetype = unname(style_values$linetypes[species_levels]),
          linewidth = unname(style_values$linewidths[species_levels]),
          shape = unname(style_values$shapes[species_levels]),
          size = unname(style_values$point_sizes[species_levels])
        )
      )
    ),
    ggplot2::scale_linetype_manual(
      values = style_values$linetypes,
      breaks = species_levels,
      labels = label_parser,
      guide = "none"
    ),
    ggplot2::scale_linewidth_manual(
      values = style_values$linewidths,
      breaks = species_levels,
      labels = label_parser,
      guide = "none"
    ),
    ggplot2::scale_shape_manual(
      values = style_values$shapes,
      breaks = species_levels,
      labels = label_parser,
      guide = "none"
    ),
    ggplot2::scale_size_manual(
      values = style_values$point_sizes,
      breaks = species_levels,
      labels = label_parser,
      guide = "none"
    )
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

na_posixct <- function(timezone) {
  structure(NA_real_, class = c("POSIXct", "POSIXt"), tzone = timezone)
}

extract_recording_coordinates_from_path <- function(path_text) {
  matches <- regexec(
    "(-?[0-9]{1,2}\\.[0-9]+)([+-][0-9]{1,3}\\.[0-9]+)",
    basename(path_text),
    perl = TRUE
  )
  parts <- regmatches(basename(path_text), matches)[[1]]

  if (length(parts) == 3) {
    return(c(
      latitude = as.numeric(parts[2]),
      longitude = as.numeric(parts[3])
    ))
  }

  c(latitude = NA_real_, longitude = NA_real_)
}

extract_recording_start_time_from_path <- function(path_text, timezone) {
  timestamp_text <- regmatches(
    basename(path_text),
    regexpr("[0-9]{8}T[0-9]{6}[+-][0-9]{4}", basename(path_text))
  )

  if (length(timestamp_text) == 1 && !is.na(timestamp_text) && nzchar(timestamp_text)) {
    return(as.POSIXct(timestamp_text, format = "%Y%m%dT%H%M%S%z", tz = timezone))
  }

  na_posixct(timezone)
}

build_summary_file_metadata <- function(summary_csv_files, timezone) {
  metadata_list <- lapply(summary_csv_files, function(summary_csv) {
    coordinates <- extract_recording_coordinates_from_path(summary_csv)
    recording_start_time <- extract_recording_start_time_from_path(summary_csv, timezone = timezone)
    local_date <- if (is.na(recording_start_time)) {
      as.Date(NA)
    } else {
      as.Date(strftime(recording_start_time, "%Y-%m-%d", tz = timezone))
    }

    data.frame(
      summary_csv = summary_csv,
      recorder_id = extract_recorder_id(summary_csv),
      recording_start_time = recording_start_time,
      local_date = local_date,
      recording_latitude = unname(coordinates[["latitude"]]),
      recording_longitude = unname(coordinates[["longitude"]]),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, metadata_list)
}

deg2rad <- function(x) {
  x * pi / 180
}

sun_calc_to_julian <- function(date_time) {
  as.numeric(date_time) / 86400 - 0.5 + 2440588
}

sun_calc_from_julian <- function(julian_day, timezone) {
  as.POSIXct((julian_day + 0.5 - 2440588) * 86400, origin = "1970-01-01", tz = timezone)
}

sun_calc_to_days <- function(date_time) {
  sun_calc_to_julian(date_time) - 2451545
}

sun_calc_julian_cycle <- function(days_since_j2000, longitude_west_radians) {
  round(days_since_j2000 - 0.0009 - longitude_west_radians / (2 * pi))
}

sun_calc_approx_transit <- function(hour_angle_radians, longitude_west_radians, julian_cycle) {
  0.0009 + (hour_angle_radians + longitude_west_radians) / (2 * pi) + julian_cycle
}

sun_calc_solar_mean_anomaly <- function(approx_transit_days) {
  deg2rad(357.5291 + 0.98560028 * approx_transit_days)
}

sun_calc_ecliptic_longitude <- function(mean_anomaly_radians) {
  mean_anomaly_radians +
    deg2rad(1.9148) * sin(mean_anomaly_radians) +
    deg2rad(0.02) * sin(2 * mean_anomaly_radians) +
    deg2rad(0.0003) * sin(3 * mean_anomaly_radians) +
    deg2rad(102.9372) +
    pi
}

sun_calc_declination <- function(ecliptic_longitude_radians, latitude_radians = 0) {
  earth_obliquity <- deg2rad(23.4397)
  asin(
    sin(latitude_radians) * cos(earth_obliquity) +
      cos(latitude_radians) * sin(earth_obliquity) * sin(ecliptic_longitude_radians)
  )
}

sun_calc_solar_transit <- function(approx_transit_days, mean_anomaly_radians, ecliptic_longitude_radians) {
  2451545 +
    approx_transit_days +
    0.0053 * sin(mean_anomaly_radians) -
    0.0069 * sin(2 * ecliptic_longitude_radians)
}

sun_calc_hour_angle <- function(solar_altitude_radians, latitude_radians, declination_radians) {
  cos_hour_angle <- (
    sin(solar_altitude_radians) -
      sin(latitude_radians) * sin(declination_radians)
  ) / (cos(latitude_radians) * cos(declination_radians))

  if (is.na(cos_hour_angle) || cos_hour_angle < -1 || cos_hour_angle > 1) {
    return(NA_real_)
  }

  acos(cos_hour_angle)
}

sun_calc_set_julian <- function(solar_altitude_radians,
                                longitude_west_radians,
                                latitude_radians,
                                declination_radians,
                                julian_cycle,
                                mean_anomaly_radians,
                                ecliptic_longitude_radians) {
  hour_angle <- sun_calc_hour_angle(
    solar_altitude_radians = solar_altitude_radians,
    latitude_radians = latitude_radians,
    declination_radians = declination_radians
  )

  if (is.na(hour_angle)) {
    return(NA_real_)
  }

  sun_calc_solar_transit(
    sun_calc_approx_transit(hour_angle, longitude_west_radians, julian_cycle),
    mean_anomaly_radians,
    ecliptic_longitude_radians
  )
}

calculate_solar_times <- function(local_date, latitude, longitude, timezone) {
  date_noon <- as.POSIXct(
    sprintf("%s 12:00:00", format(as.Date(local_date), "%Y-%m-%d")),
    tz = timezone
  )
  longitude_west_radians <- deg2rad(-longitude)
  latitude_radians <- deg2rad(latitude)
  days_since_j2000 <- sun_calc_to_days(date_noon)
  julian_cycle <- sun_calc_julian_cycle(days_since_j2000, longitude_west_radians)
  approx_transit_days <- sun_calc_approx_transit(0, longitude_west_radians, julian_cycle)
  mean_anomaly_radians <- sun_calc_solar_mean_anomaly(approx_transit_days)
  ecliptic_longitude_radians <- sun_calc_ecliptic_longitude(mean_anomaly_radians)
  declination_radians <- sun_calc_declination(ecliptic_longitude_radians)
  solar_noon_julian <- sun_calc_solar_transit(
    approx_transit_days,
    mean_anomaly_radians,
    ecliptic_longitude_radians
  )
  sunrise_set_julian <- sun_calc_set_julian(
    solar_altitude_radians = deg2rad(-0.833),
    longitude_west_radians = longitude_west_radians,
    latitude_radians = latitude_radians,
    declination_radians = declination_radians,
    julian_cycle = julian_cycle,
    mean_anomaly_radians = mean_anomaly_radians,
    ecliptic_longitude_radians = ecliptic_longitude_radians
  )
  civil_dusk_julian <- sun_calc_set_julian(
    solar_altitude_radians = deg2rad(-6),
    longitude_west_radians = longitude_west_radians,
    latitude_radians = latitude_radians,
    declination_radians = declination_radians,
    julian_cycle = julian_cycle,
    mean_anomaly_radians = mean_anomaly_radians,
    ecliptic_longitude_radians = ecliptic_longitude_radians
  )

  if (is.na(sunrise_set_julian) || is.na(civil_dusk_julian)) {
    solar_noon_altitude <- asin(
      sin(latitude_radians) * sin(declination_radians) +
        cos(latitude_radians) * cos(declination_radians)
    )
    is_continuous_daylight <- solar_noon_altitude > deg2rad(-0.833)

    return(data.frame(
      local_date = as.Date(local_date),
      latitude = latitude,
      longitude = longitude,
      civil_dawn = na_posixct(timezone),
      sunrise = na_posixct(timezone),
      solar_noon = sun_calc_from_julian(solar_noon_julian, timezone = timezone),
      sunset = na_posixct(timezone),
      civil_dusk = na_posixct(timezone),
      fallback_phase = if (isTRUE(is_continuous_daylight)) "daylight" else "night",
      stringsAsFactors = FALSE
    ))
  }

  sunrise_julian <- solar_noon_julian - (sunrise_set_julian - solar_noon_julian)
  civil_dawn_julian <- solar_noon_julian - (civil_dusk_julian - solar_noon_julian)

  data.frame(
    local_date = as.Date(local_date),
    latitude = latitude,
    longitude = longitude,
    civil_dawn = sun_calc_from_julian(civil_dawn_julian, timezone = timezone),
    sunrise = sun_calc_from_julian(sunrise_julian, timezone = timezone),
    solar_noon = sun_calc_from_julian(solar_noon_julian, timezone = timezone),
    sunset = sun_calc_from_julian(sunrise_set_julian, timezone = timezone),
    civil_dusk = sun_calc_from_julian(civil_dusk_julian, timezone = timezone),
    fallback_phase = NA_character_,
    stringsAsFactors = FALSE
  )
}

build_light_phase_schedule <- function(local_date, latitude, longitude, timezone) {
  solar_times <- calculate_solar_times(
    local_date = local_date,
    latitude = latitude,
    longitude = longitude,
    timezone = timezone
  )
  day_start <- as.POSIXct(sprintf("%s 00:00:00", format(as.Date(local_date), "%Y-%m-%d")), tz = timezone)
  next_day <- day_start + 24 * 3600

  if (!is.na(solar_times$fallback_phase[[1]])) {
    interval_df <- data.frame(
      light_phase = solar_times$fallback_phase[[1]],
      plot_phase = solar_times$fallback_phase[[1]],
      plot_phase_key = solar_times$fallback_phase[[1]],
      interval_start = day_start,
      interval_end = next_day,
      stringsAsFactors = FALSE
    )
  } else {
    interval_df <- data.frame(
      light_phase = c("night", "twilight", "daylight", "twilight", "night"),
      plot_phase = c("night", "morning_twilight", "daylight", "evening_twilight", "night"),
      plot_phase_key = c("night_pre_dawn", "morning_twilight", "daylight", "evening_twilight", "night_post_dusk"),
      interval_start = c(day_start, solar_times$civil_dawn[[1]], solar_times$sunrise[[1]], solar_times$sunset[[1]], solar_times$civil_dusk[[1]]),
      interval_end = c(solar_times$civil_dawn[[1]], solar_times$sunrise[[1]], solar_times$sunset[[1]], solar_times$civil_dusk[[1]], next_day),
      stringsAsFactors = FALSE
    )
    interval_df <- interval_df[interval_df$interval_end > interval_df$interval_start, , drop = FALSE]
  }

  interval_df$local_date <- as.Date(local_date)
  interval_df$latitude <- latitude
  interval_df$longitude <- longitude
  interval_df
}

build_light_phase_lookup <- function(local_dates, latitudes, longitudes, timezone) {
  unique_keys <- unique(data.frame(
    local_date = as.Date(local_dates),
    latitude = as.numeric(latitudes),
    longitude = as.numeric(longitudes),
    stringsAsFactors = FALSE
  ))
  unique_keys <- unique_keys[!is.na(unique_keys$local_date) & !is.na(unique_keys$latitude) & !is.na(unique_keys$longitude), , drop = FALSE]

  if (nrow(unique_keys) == 0) {
    return(list(
      solar_times = data.frame(
        local_date = as.Date(character()),
        latitude = numeric(),
        longitude = numeric(),
        civil_dawn = as.POSIXct(character()),
        sunrise = as.POSIXct(character()),
        solar_noon = as.POSIXct(character()),
        sunset = as.POSIXct(character()),
        civil_dusk = as.POSIXct(character()),
        fallback_phase = character(),
        stringsAsFactors = FALSE
      ),
      schedules = data.frame(
        light_phase = character(),
        plot_phase = character(),
        interval_start = as.POSIXct(character()),
        interval_end = as.POSIXct(character()),
        local_date = as.Date(character()),
        latitude = numeric(),
        longitude = numeric(),
        stringsAsFactors = FALSE
      )
    ))
  }

  solar_times <- do.call(
    rbind,
    lapply(seq_len(nrow(unique_keys)), function(index) {
      calculate_solar_times(
        local_date = unique_keys$local_date[[index]],
        latitude = unique_keys$latitude[[index]],
        longitude = unique_keys$longitude[[index]],
        timezone = timezone
      )
    })
  )
  schedules <- do.call(
    rbind,
    lapply(seq_len(nrow(unique_keys)), function(index) {
      build_light_phase_schedule(
        local_date = unique_keys$local_date[[index]],
        latitude = unique_keys$latitude[[index]],
        longitude = unique_keys$longitude[[index]],
        timezone = timezone
      )
    })
  )

  list(solar_times = solar_times, schedules = schedules)
}

phase_band_palette <- c(
  morning_twilight = "deeppink2",
  daylight = "gold",
  evening_twilight = "deeppink2",
  night = "midnightblue"
)

build_recorder_reference_locations <- function(summary_metadata) {
  valid_metadata <- summary_metadata[
    !is.na(summary_metadata$recording_latitude) &
      !is.na(summary_metadata$recording_longitude),
    c("recorder_id", "recording_latitude", "recording_longitude"),
    drop = FALSE
  ]

  if (nrow(valid_metadata) == 0) {
    return(data.frame(
      recorder_id = character(),
      recording_latitude = numeric(),
      recording_longitude = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(
    rbind,
    lapply(split(valid_metadata, valid_metadata$recorder_id), function(group_df) {
      data.frame(
        recorder_id = as.character(group_df$recorder_id[[1]]),
        recording_latitude = stats::median(group_df$recording_latitude, na.rm = TRUE),
        recording_longitude = stats::median(group_df$recording_longitude, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
  )
}

build_plot_light_phase_bands <- function(reference_locations,
                                         local_dates,
                                         timezone,
                                         aggregate_across_locations = FALSE) {
  reference_locations <- reference_locations[
    !is.na(reference_locations$recording_latitude) &
      !is.na(reference_locations$recording_longitude),
    ,
    drop = FALSE
  ]
  local_dates <- sort(unique(as.Date(local_dates)))
  local_dates <- local_dates[!is.na(local_dates)]

  if (nrow(reference_locations) == 0 || length(local_dates) == 0) {
    return(data.frame(
      recorder_id = character(),
      plot_phase = character(),
      interval_start = as.POSIXct(character(), tz = timezone),
      interval_end = as.POSIXct(character(), tz = timezone),
      stringsAsFactors = FALSE
    ))
  }

  band_rows <- do.call(
    rbind,
    lapply(seq_len(nrow(reference_locations)), function(index) {
      do.call(
        rbind,
        lapply(local_dates, function(local_date) {
          schedule_df <- build_light_phase_schedule(
            local_date = local_date,
            latitude = reference_locations$recording_latitude[[index]],
            longitude = reference_locations$recording_longitude[[index]],
            timezone = timezone
          )
          schedule_df$recorder_id <- reference_locations$recorder_id[[index]]
          schedule_df
        })
      )
    })
  )

  band_rows <- unique(band_rows[, c("recorder_id", "plot_phase", "plot_phase_key", "interval_start", "interval_end", "local_date"), drop = FALSE])
  band_rows$plot_phase <- factor(
    band_rows$plot_phase,
    levels = c("morning_twilight", "daylight", "evening_twilight", "night")
  )

  if (!isTRUE(aggregate_across_locations)) {
    return(band_rows)
  }

  aggregate(
    cbind(
      interval_start_num = as.numeric(band_rows$interval_start),
      interval_end_num = as.numeric(band_rows$interval_end)
    ),
    by = list(
      local_date = band_rows$local_date,
      plot_phase = band_rows$plot_phase,
      plot_phase_key = band_rows$plot_phase_key
    ),
    FUN = median
  ) |>
    transform(
      interval_start = as.POSIXct(interval_start_num, origin = "1970-01-01", tz = timezone),
      interval_end = as.POSIXct(interval_end_num, origin = "1970-01-01", tz = timezone)
    ) |>
    subset(select = c("plot_phase", "interval_start", "interval_end"))
}

light_phase_band_layers <- function(band_df, alpha = 0.14, ymin = -Inf, ymax = Inf) {
  list(
    ggplot2::geom_rect(
      data = band_df,
      ggplot2::aes(xmin = interval_start, xmax = interval_end, fill = plot_phase),
      inherit.aes = FALSE,
      alpha = alpha,
      colour = NA,
      ymin = ymin,
      ymax = ymax
    ),
    ggplot2::scale_fill_manual(
      values = phase_band_palette,
      guide = "none",
      drop = FALSE
    )
  )
}

classify_light_phase <- function(date_time, solar_time_row) {
  if (nrow(solar_time_row) != 1 || is.na(date_time)) {
    return(NA_character_)
  }

  if (!is.na(solar_time_row$fallback_phase[[1]])) {
    return(solar_time_row$fallback_phase[[1]])
  }

  if (date_time >= solar_time_row$sunrise[[1]] && date_time < solar_time_row$sunset[[1]]) {
    return("daylight")
  }

  if (date_time >= solar_time_row$civil_dawn[[1]] && date_time < solar_time_row$sunrise[[1]]) {
    return("twilight")
  }

  if (date_time >= solar_time_row$sunset[[1]] && date_time < solar_time_row$civil_dusk[[1]]) {
    return("twilight")
  }

  "night"
}

estimate_recording_duration_seconds <- function(summary_metadata, default_hours = 1) {
  recorder_ids <- unique(summary_metadata$recorder_id)
  duration_table <- lapply(recorder_ids, function(recorder_id) {
    recorder_starts <- sort(unique(summary_metadata$recording_start_time[summary_metadata$recorder_id == recorder_id]))
    recorder_starts <- recorder_starts[!is.na(recorder_starts)]
    positive_diffs <- as.numeric(diff(recorder_starts), units = "secs")
    positive_diffs <- positive_diffs[is.finite(positive_diffs) & positive_diffs > 0 & positive_diffs <= 6 * 3600]

    data.frame(
      recorder_id = recorder_id,
      estimated_duration_seconds = if (length(positive_diffs) > 0) stats::median(positive_diffs) else default_hours * 3600,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, duration_table)
}

interval_overlap_seconds <- function(start_a, end_a, start_b, end_b) {
  overlap_start <- max(as.numeric(start_a), as.numeric(start_b))
  overlap_end <- min(as.numeric(end_a), as.numeric(end_b))
  max(0, overlap_end - overlap_start)
}

calculate_recording_phase_effort <- function(recording_start_time,
                                             recording_end_time,
                                             latitude,
                                             longitude,
                                             timezone) {
  covered_dates <- seq(
    from = as.Date(strftime(recording_start_time, "%Y-%m-%d", tz = timezone)),
    to = as.Date(strftime(recording_end_time - 1, "%Y-%m-%d", tz = timezone)),
    by = "day"
  )

  effort_rows <- lapply(covered_dates, function(local_date) {
    phase_schedule <- build_light_phase_schedule(
      local_date = local_date,
      latitude = latitude,
      longitude = longitude,
      timezone = timezone
    )

    overlaps <- vapply(
      seq_len(nrow(phase_schedule)),
      function(index) {
        interval_overlap_seconds(
          recording_start_time,
          recording_end_time,
          phase_schedule$interval_start[[index]],
          phase_schedule$interval_end[[index]]
        )
      },
      numeric(1)
    )

    data.frame(
      local_date = as.Date(local_date),
      light_phase = phase_schedule$light_phase,
      sampled_hours = overlaps / 3600,
      stringsAsFactors = FALSE
    )
  })

  effort_df <- do.call(rbind, effort_rows)
  aggregate(
    list(sampled_hours = effort_df$sampled_hours),
    by = list(
      local_date = effort_df$local_date,
      light_phase = effort_df$light_phase
    ),
    FUN = sum
  )
}

build_recording_phase_effort <- function(summary_metadata, timezone) {
  valid_metadata <- summary_metadata[
    !is.na(summary_metadata$recording_start_time) &
      !is.na(summary_metadata$recording_latitude) &
      !is.na(summary_metadata$recording_longitude),
    ,
    drop = FALSE
  ]

  if (nrow(valid_metadata) == 0) {
    return(data.frame(
      summary_csv = character(),
      recorder_id = character(),
      local_date = as.Date(character()),
      light_phase = character(),
      sampled_hours = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  duration_lookup <- estimate_recording_duration_seconds(valid_metadata)
  valid_metadata <- merge(valid_metadata, duration_lookup, by = "recorder_id", all.x = TRUE)
  valid_metadata$recording_end_time <- valid_metadata$recording_start_time + valid_metadata$estimated_duration_seconds

  effort_list <- lapply(seq_len(nrow(valid_metadata)), function(index) {
    effort_df <- calculate_recording_phase_effort(
      recording_start_time = valid_metadata$recording_start_time[[index]],
      recording_end_time = valid_metadata$recording_end_time[[index]],
      latitude = valid_metadata$recording_latitude[[index]],
      longitude = valid_metadata$recording_longitude[[index]],
      timezone = timezone
    )
    effort_df$summary_csv <- valid_metadata$summary_csv[[index]]
    effort_df$recorder_id <- valid_metadata$recorder_id[[index]]
    effort_df
  })

  do.call(rbind, effort_list)
}

build_diel_species_summary <- function(detections_subset, effort_summary, include_recorder = FALSE) {
  light_phase_levels <- c("daylight", "twilight", "night")
  empty_summary <- data.frame(
    scientific_name = character(),
    common_name = character(),
    species_label = character(),
    total_detections = integer(),
    daylight_detections = integer(),
    twilight_detections = integer(),
    night_detections = integer(),
    daylight_effort_hours = numeric(),
    twilight_effort_hours = numeric(),
    night_effort_hours = numeric(),
    daylight_detections_per_hour = numeric(),
    twilight_detections_per_hour = numeric(),
    night_detections_per_hour = numeric(),
    daylight_detection_share = numeric(),
    twilight_detection_share = numeric(),
    night_detection_share = numeric(),
    dominant_light_phase = character(),
    log2_night_day_rate_ratio = numeric(),
    stringsAsFactors = FALSE
  )

  if (isTRUE(include_recorder)) {
    empty_summary <- cbind(data.frame(recorder_id = character(), stringsAsFactors = FALSE), empty_summary)
  }

  if (nrow(detections_subset) == 0) {
    return(empty_summary)
  }

  grouping_columns <- c(if (isTRUE(include_recorder)) "recorder_id", "scientific_name", "common_name", "species_label")
  count_groups <- c(grouping_columns, "light_phase")
  counts_long <- aggregate(
    list(detection_count = rep(1L, nrow(detections_subset))),
    by = as.list(detections_subset[, count_groups, drop = FALSE]),
    FUN = sum
  )

  species_lookup <- unique(detections_subset[, grouping_columns, drop = FALSE])
  phase_grid <- data.frame(light_phase = light_phase_levels, stringsAsFactors = FALSE)
  full_grid <- merge(species_lookup, phase_grid, by = NULL)
  counts_long <- merge(full_grid, counts_long, by = count_groups, all.x = TRUE)
  counts_long$detection_count[is.na(counts_long$detection_count)] <- 0L

  effort_key_columns <- c(if (isTRUE(include_recorder)) "recorder_id", "light_phase")
  counts_long <- merge(counts_long, effort_summary, by = effort_key_columns, all.x = TRUE)
  counts_long$sampled_hours[is.na(counts_long$sampled_hours)] <- 0
  counts_long$detections_per_hour <- ifelse(
    counts_long$sampled_hours > 0,
    counts_long$detection_count / counts_long$sampled_hours,
    NA_real_
  )

  summary_list <- lapply(split(counts_long, interaction(counts_long[, grouping_columns, drop = FALSE], drop = TRUE)), function(group_df) {
    group_df <- group_df[match(light_phase_levels, group_df$light_phase), , drop = FALSE]
    total_detections <- sum(group_df$detection_count)
    shares <- if (total_detections > 0) {
      group_df$detection_count / total_detections
    } else {
      rep(0, length(light_phase_levels))
    }
    phase_rates <- group_df$detections_per_hour
    dominant_phase <- light_phase_levels[[which.max(replace(phase_rates, is.na(phase_rates), -Inf))]]
    night_rate <- (group_df$detection_count[group_df$light_phase == "night"] + 0.5) /
      (group_df$sampled_hours[group_df$light_phase == "night"] + 0.5)
    daylight_rate <- (group_df$detection_count[group_df$light_phase == "daylight"] + 0.5) /
      (group_df$sampled_hours[group_df$light_phase == "daylight"] + 0.5)

    summary_row <- data.frame(
      scientific_name = group_df$scientific_name[[1]],
      common_name = group_df$common_name[[1]],
      species_label = group_df$species_label[[1]],
      total_detections = total_detections,
      daylight_detections = group_df$detection_count[group_df$light_phase == "daylight"],
      twilight_detections = group_df$detection_count[group_df$light_phase == "twilight"],
      night_detections = group_df$detection_count[group_df$light_phase == "night"],
      daylight_effort_hours = group_df$sampled_hours[group_df$light_phase == "daylight"],
      twilight_effort_hours = group_df$sampled_hours[group_df$light_phase == "twilight"],
      night_effort_hours = group_df$sampled_hours[group_df$light_phase == "night"],
      daylight_detections_per_hour = group_df$detections_per_hour[group_df$light_phase == "daylight"],
      twilight_detections_per_hour = group_df$detections_per_hour[group_df$light_phase == "twilight"],
      night_detections_per_hour = group_df$detections_per_hour[group_df$light_phase == "night"],
      daylight_detection_share = shares[group_df$light_phase == "daylight"],
      twilight_detection_share = shares[group_df$light_phase == "twilight"],
      night_detection_share = shares[group_df$light_phase == "night"],
      dominant_light_phase = dominant_phase,
      log2_night_day_rate_ratio = log2(night_rate / daylight_rate),
      stringsAsFactors = FALSE
    )

    if (isTRUE(include_recorder)) {
      summary_row <- cbind(
        data.frame(recorder_id = as.character(group_df$recorder_id[[1]]), stringsAsFactors = FALSE),
        summary_row
      )
    }

    summary_row
  })

  summary_df <- do.call(rbind, summary_list)
  summary_df[order(-summary_df$total_detections, summary_df$species_label), , drop = FALSE]
}

build_diel_species_long <- function(diel_species_summary, include_recorder = FALSE) {
  id_columns <- c(if (isTRUE(include_recorder)) "recorder_id", "scientific_name", "common_name", "species_label", "total_detections", "dominant_light_phase", "log2_night_day_rate_ratio")
  do.call(
    rbind,
    list(
      data.frame(
        diel_species_summary[, id_columns, drop = FALSE],
        light_phase = "daylight",
        detection_count = diel_species_summary$daylight_detections,
        sampled_hours = diel_species_summary$daylight_effort_hours,
        detections_per_hour = diel_species_summary$daylight_detections_per_hour,
        detection_share = diel_species_summary$daylight_detection_share,
        stringsAsFactors = FALSE
      ),
      data.frame(
        diel_species_summary[, id_columns, drop = FALSE],
        light_phase = "twilight",
        detection_count = diel_species_summary$twilight_detections,
        sampled_hours = diel_species_summary$twilight_effort_hours,
        detections_per_hour = diel_species_summary$twilight_detections_per_hour,
        detection_share = diel_species_summary$twilight_detection_share,
        stringsAsFactors = FALSE
      ),
      data.frame(
        diel_species_summary[, id_columns, drop = FALSE],
        light_phase = "night",
        detection_count = diel_species_summary$night_detections,
        sampled_hours = diel_species_summary$night_effort_hours,
        detections_per_hour = diel_species_summary$night_detections_per_hour,
        detection_share = diel_species_summary$night_detection_share,
        stringsAsFactors = FALSE
      )
    )
  )
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

add_running_mean_to_time_series <- function(time_series_summary, bin_minutes, running_days = 7) {
  if (nrow(time_series_summary) == 0) {
    time_series_summary$identification_count_running_mean <- numeric()
    time_series_summary$identification_count_plot <- numeric()
    time_series_summary$identification_count_running_mean_plot <- numeric()
    return(time_series_summary)
  }

  window_bins <- max(1L, as.integer(round((running_days * 24 * 60) / bin_minutes)))
  counts <- as.numeric(time_series_summary$identification_count)
  cumulative_counts <- c(0, cumsum(counts))
  running_mean <- vapply(
    seq_along(counts),
    function(index) {
      start_index <- max(1L, index - window_bins + 1L)
      total_count <- cumulative_counts[index + 1L] - cumulative_counts[start_index]
      total_count / (index - start_index + 1L)
    },
    numeric(1)
  )

  time_series_summary$identification_count_running_mean <- running_mean
  time_series_summary$identification_count_plot <- ifelse(
    time_series_summary$identification_count > 0,
    time_series_summary$identification_count,
    NA_real_
  )
  time_series_summary$identification_count_running_mean_plot <- ifelse(
    time_series_summary$identification_count_running_mean > 0,
    time_series_summary$identification_count_running_mean,
    NA_real_
  )
  time_series_summary
}

build_time_series_summary_for_subset <- function(detections_subset, bin_minutes, timezone, rolling_mean_window_days = 7) {
  if (nrow(detections_subset) == 0) {
    return(data.frame(
      time_bin = as.POSIXct(character()),
      identification_count = integer(),
      unique_species_count = integer(),
      identification_count_running_mean = numeric(),
      identification_count_plot = numeric(),
      identification_count_running_mean_plot = numeric()
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
  time_series_summary <- time_series_summary[order(time_series_summary$time_bin), , drop = FALSE]
  add_running_mean_to_time_series(
    time_series_summary,
    bin_minutes = bin_minutes,
    running_days = rolling_mean_window_days
  )
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

empty_temporal_diagnostics_df <- function() {
  data.frame(
    metric_name = character(),
    panel = character(),
    x_value = numeric(),
    y_value = numeric(),
    lag_bin = numeric(),
    lag_hours = numeric(),
    frequency_cycles_per_bin = numeric(),
    period_bins = numeric(),
    period_hours = numeric(),
    significance_limit = numeric(),
    stringsAsFactors = FALSE
  )
}

empty_temporal_tests_df <- function() {
  data.frame(
    metric_name = character(),
    test_name = character(),
    lag_bin = numeric(),
    lag_hours = numeric(),
    statistic = numeric(),
    p_value = numeric(),
    significant = logical(),
    stringsAsFactors = FALSE
  )
}

empty_temporal_peaks_df <- function() {
  data.frame(
    metric_name = character(),
    peak_rank = integer(),
    frequency_cycles_per_bin = numeric(),
    period_bins = numeric(),
    period_hours = numeric(),
    spectral_density = numeric(),
    relative_power = numeric(),
    stringsAsFactors = FALSE
  )
}

bind_data_frames <- function(data_frames, empty_template) {
  valid_frames <- Filter(function(x) !is.null(x) && nrow(x) > 0, data_frames)

  if (length(valid_frames) == 0) {
    return(empty_template)
  }

  do.call(rbind, valid_frames)
}

identify_spectral_peaks <- function(frequency_cycles_per_bin, spectral_density, metric_name, bin_minutes, max_peaks = 5L) {
  if (length(spectral_density) == 0) {
    return(empty_temporal_peaks_df())
  }

  if (length(spectral_density) < 3) {
    local_peak_indices <- order(spectral_density, decreasing = TRUE)
  } else {
    local_peak_indices <- which(
      c(
        FALSE,
        spectral_density[2:(length(spectral_density) - 1L)] > spectral_density[1:(length(spectral_density) - 2L)] &
          spectral_density[2:(length(spectral_density) - 1L)] >= spectral_density[3:length(spectral_density)],
        FALSE
      )
    )

    if (length(local_peak_indices) == 0) {
      local_peak_indices <- order(spectral_density, decreasing = TRUE)
    } else {
      local_peak_indices <- local_peak_indices[order(spectral_density[local_peak_indices], decreasing = TRUE)]
    }
  }

  local_peak_indices <- unique(local_peak_indices)[seq_len(min(max_peaks, length(local_peak_indices)))]

  data.frame(
    metric_name = metric_name,
    peak_rank = seq_along(local_peak_indices),
    frequency_cycles_per_bin = frequency_cycles_per_bin[local_peak_indices],
    period_bins = 1 / frequency_cycles_per_bin[local_peak_indices],
    period_hours = (bin_minutes / 60) / frequency_cycles_per_bin[local_peak_indices],
    spectral_density = spectral_density[local_peak_indices],
    relative_power = spectral_density[local_peak_indices] / sum(spectral_density),
    stringsAsFactors = FALSE
  )
}

build_temporal_diagnostics_for_metric <- function(time_series_summary,
                                                  value_column,
                                                  metric_name,
                                                  bin_minutes,
                                                  periodicity_max_lag_bins) {
  values <- as.numeric(time_series_summary[[value_column]])
  diagnostics <- empty_temporal_diagnostics_df()
  tests <- empty_temporal_tests_df()
  peaks <- empty_temporal_peaks_df()

  if (length(values) < 2 || length(unique(values)) <= 1) {
    return(list(diagnostics = diagnostics, tests = tests, peaks = peaks))
  }

  max_lag <- min(periodicity_max_lag_bins, length(values) - 1L)
  significance_limit <- 1.96 / sqrt(length(values))

  acf_result <- stats::acf(
    values,
    plot = FALSE,
    lag.max = max_lag,
    na.action = stats::na.pass
  )
  diagnostics <- rbind(
    diagnostics,
    data.frame(
      metric_name = metric_name,
      panel = "autocorrelation (ACF)",
      x_value = as.numeric(acf_result$lag[, , 1]) * (bin_minutes / 60),
      y_value = as.numeric(acf_result$acf[, , 1]),
      lag_bin = as.numeric(acf_result$lag[, , 1]),
      lag_hours = as.numeric(acf_result$lag[, , 1]) * (bin_minutes / 60),
      frequency_cycles_per_bin = NA_real_,
      period_bins = NA_real_,
      period_hours = NA_real_,
      significance_limit = significance_limit,
      stringsAsFactors = FALSE
    )
  )

  if (max_lag >= 1) {
    pacf_result <- stats::pacf(
      values,
      plot = FALSE,
      lag.max = max_lag,
      na.action = stats::na.pass
    )
    diagnostics <- rbind(
      diagnostics,
      data.frame(
        metric_name = metric_name,
        panel = "partial autocorrelation (PACF)",
        x_value = as.numeric(pacf_result$lag) * (bin_minutes / 60),
        y_value = as.numeric(pacf_result$acf),
        lag_bin = as.numeric(pacf_result$lag),
        lag_hours = as.numeric(pacf_result$lag) * (bin_minutes / 60),
        frequency_cycles_per_bin = NA_real_,
        period_bins = NA_real_,
        period_hours = NA_real_,
        significance_limit = significance_limit,
        stringsAsFactors = FALSE
      )
    )
  }

  candidate_lags <- unique(pmin(
    c(
      max(1L, as.integer(round((24 * 60) / bin_minutes))),
      max(1L, as.integer(round((7 * 24 * 60) / bin_minutes))),
      max_lag
    ),
    max_lag
  ))
  candidate_lags <- candidate_lags[candidate_lags >= 1]

  if (length(candidate_lags) > 0) {
    tests <- do.call(
      rbind,
      lapply(candidate_lags, function(test_lag) {
        lb_result <- stats::Box.test(values, lag = test_lag, type = "Ljung-Box")
        data.frame(
          metric_name = metric_name,
          test_name = "Ljung-Box test",
          lag_bin = test_lag,
          lag_hours = test_lag * (bin_minutes / 60),
          statistic = as.numeric(lb_result$statistic),
          p_value = as.numeric(lb_result$p.value),
          significant = as.numeric(lb_result$p.value) < 0.05,
          stringsAsFactors = FALSE
        )
      })
    )
  }

  if (length(values) >= 4 && sum((values - mean(values))^2) > 0) {
    spectrum_result <- stats::spec.pgram(
      values,
      taper = 0,
      demean = TRUE,
      detrend = FALSE,
      plot = FALSE,
      fast = FALSE
    )
    positive_frequency <- spectrum_result$freq > 0
    spectral_frequency <- spectrum_result$freq[positive_frequency]
    spectral_density <- spectrum_result$spec[positive_frequency]

    diagnostics <- rbind(
      diagnostics,
      data.frame(
        metric_name = metric_name,
        panel = "spectral density",
        x_value = (bin_minutes / 60) / spectral_frequency,
        y_value = spectral_density,
        lag_bin = NA_real_,
        lag_hours = NA_real_,
        frequency_cycles_per_bin = spectral_frequency,
        period_bins = 1 / spectral_frequency,
        period_hours = (bin_minutes / 60) / spectral_frequency,
        significance_limit = NA_real_,
        stringsAsFactors = FALSE
      )
    )

    peaks <- identify_spectral_peaks(
      frequency_cycles_per_bin = spectral_frequency,
      spectral_density = spectral_density,
      metric_name = metric_name,
      bin_minutes = bin_minutes
    )
  }

  list(diagnostics = diagnostics, tests = tests, peaks = peaks)
}

build_temporal_diagnostics_bundle <- function(time_series_summary, bin_minutes, periodicity_max_lag_bins) {
  metric_specs <- data.frame(
    value_column = c("identification_count", "unique_species_count"),
    metric_name = c("number of detections per bin", "unique species identified per bin"),
    stringsAsFactors = FALSE
  )

  diagnostic_results <- lapply(seq_len(nrow(metric_specs)), function(metric_index) {
    build_temporal_diagnostics_for_metric(
      time_series_summary = time_series_summary,
      value_column = metric_specs$value_column[[metric_index]],
      metric_name = metric_specs$metric_name[[metric_index]],
      bin_minutes = bin_minutes,
      periodicity_max_lag_bins = periodicity_max_lag_bins
    )
  })

  list(
    diagnostics = bind_data_frames(
      lapply(diagnostic_results, function(result) result$diagnostics),
      empty_temporal_diagnostics_df()
    ),
    tests = bind_data_frames(
      lapply(diagnostic_results, function(result) result$tests),
      empty_temporal_tests_df()
    ),
    peaks = bind_data_frames(
      lapply(diagnostic_results, function(result) result$peaks),
      empty_temporal_peaks_df()
    )
  )
}

build_temporal_diagnostics_plot <- function(diagnostics_df,
                                            peaks_df,
                                            title_text,
                                            subtitle_text,
                                            facet_column = "facet_label",
                                            ncol = 2,
                                            placeholder_text) {
  if (nrow(diagnostics_df) == 0) {
    return(make_placeholder_plot(
      title_text = title_text,
      subtitle_text = subtitle_text,
      body_text = placeholder_text
    ))
  }

  facet_formula <- stats::as.formula(paste("~", facet_column))
  plot_object <- ggplot2::ggplot(
    diagnostics_df,
    ggplot2::aes(x = x_value, y = y_value)
  ) +
    ggplot2::geom_line(linewidth = 0.9, colour = "firebrick3") +
    ggplot2::facet_wrap(facet_formula, scales = "free", ncol = ncol) +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "lag / period (hours)",
      y = "metric value"
    ) +
    analysis_plot_theme()

  significance_lines <- unique(
    diagnostics_df[
      diagnostics_df$panel %in% c("autocorrelation (ACF)", "partial autocorrelation (PACF)") &
        !is.na(diagnostics_df$significance_limit),
      c(facet_column, "significance_limit"),
      drop = FALSE
    ]
  )

  if (nrow(significance_lines) > 0) {
    zero_lines <- significance_lines
    zero_lines$yintercept <- 0
    upper_lines <- significance_lines
    upper_lines$yintercept <- upper_lines$significance_limit
    lower_lines <- significance_lines
    lower_lines$yintercept <- -lower_lines$significance_limit

    plot_object <- plot_object +
      ggplot2::geom_hline(
        data = zero_lines,
        ggplot2::aes(yintercept = yintercept),
        inherit.aes = FALSE,
        linetype = "dashed",
        colour = "grey40"
      ) +
      ggplot2::geom_hline(
        data = upper_lines,
        ggplot2::aes(yintercept = yintercept),
        inherit.aes = FALSE,
        linetype = "dotted",
        colour = "grey55"
      ) +
      ggplot2::geom_hline(
        data = lower_lines,
        ggplot2::aes(yintercept = yintercept),
        inherit.aes = FALSE,
        linetype = "dotted",
        colour = "grey55"
      )
  }

  if (nrow(peaks_df) > 0) {
    peak_points <- peaks_df
    peak_points$x_value <- peak_points$period_hours
    peak_points$y_value <- peak_points$spectral_density
    peak_points$peak_label <- sprintf(
      "peak %d\n%.1f h",
      peak_points$peak_rank,
      peak_points$period_hours
    )

    plot_object <- plot_object +
      ggplot2::geom_point(
        data = peak_points,
        ggplot2::aes(x = x_value, y = y_value),
        inherit.aes = FALSE,
        colour = "goldenrod3",
        size = 2.2
      ) +
      ggplot2::geom_text(
        data = peak_points[peak_points$peak_rank <= 3, , drop = FALSE],
        ggplot2::aes(x = x_value, y = y_value, label = peak_label),
        inherit.aes = FALSE,
        colour = "goldenrod4",
        size = 3,
        nudge_y = 0.03 * max(diagnostics_df$y_value, na.rm = TRUE)
      )
  }

  plot_object
}

build_top_species_time_series <- function(detections_subset, bin_minutes, top_n = 10L, timezone) {
  if (nrow(detections_subset) == 0) {
    return(data.frame(
      time_bin = as.POSIXct(character()),
      scientific_name = character(),
      common_name = character(),
      species_label = character(),
      identification_count = integer(),
      stringsAsFactors = FALSE
    ))
  }

  species_totals <- aggregate(
    list(total_detections = rep(1L, nrow(detections_subset))),
    by = list(
      scientific_name = detections_subset$scientific_name,
      common_name = detections_subset$common_name
    ),
    FUN = sum
  )
  species_totals$species_label <- paste0(species_totals$common_name, " (", species_totals$scientific_name, ")")
  species_totals <- species_totals[order(-species_totals$total_detections, species_totals$species_label), , drop = FALSE]
  species_totals <- species_totals[seq_len(min(top_n, nrow(species_totals))), , drop = FALSE]

  top_species_labels <- species_totals$species_label
  detections_subset$species_label <- paste0(detections_subset$common_name, " (", detections_subset$scientific_name, ")")
  detections_subset <- detections_subset[detections_subset$species_label %in% top_species_labels, , drop = FALSE]
  detections_subset$time_bin <- floor_to_bin(
    detections_subset$date_time,
    bin_minutes = bin_minutes,
    timezone = timezone
  )

  time_grid <- build_complete_time_grid(
    start_time = min(detections_subset$date_time),
    end_time = max(detections_subset$date_time),
    bin_minutes = bin_minutes,
    timezone = timezone
  )

  species_time_series <- aggregate(
    list(identification_count = rep(1L, nrow(detections_subset))),
    by = list(
      time_bin = detections_subset$time_bin,
      scientific_name = detections_subset$scientific_name,
      common_name = detections_subset$common_name,
      species_label = detections_subset$species_label
    ),
    FUN = sum
  )

  species_lookup <- species_totals[, c("scientific_name", "common_name", "species_label"), drop = FALSE]
  species_time_grid <- merge(
    data.frame(time_bin = time_grid),
    species_lookup,
    by = NULL
  )
  species_time_series <- merge(
    species_time_grid,
    species_time_series,
    by = c("time_bin", "scientific_name", "common_name", "species_label"),
    all.x = TRUE
  )
  species_time_series$identification_count[is.na(species_time_series$identification_count)] <- 0L
  species_time_series$identification_count_plot <- ifelse(
    species_time_series$identification_count > 0,
    species_time_series$identification_count,
    NA_real_
  )
  species_time_series$species_label <- factor(species_time_series$species_label, levels = species_totals$species_label)
  species_time_series
}

write_analysis_summary <- function(summary_txt,
                                   generated_at,
                                   summary_root,
                                   output_dir,
                                   bin_minutes,
                                   rolling_mean_window_days,
                                   min_confidence,
                                   file_status,
                                   filtered_detections,
                                   light_phase_sampling_effort,
                                   diel_species_summary) {
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
    sprintf("rolling mean window (days): %s", rolling_mean_window_days),
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

  if (nrow(light_phase_sampling_effort) > 0) {
    effort_lookup <- stats::setNames(light_phase_sampling_effort$sampled_hours, light_phase_sampling_effort$light_phase)
    analysis_lines <- c(
      analysis_lines,
      sprintf("Sampled daylight hours used in diel analysis: %.2f", unname(ifelse(is.na(effort_lookup[["daylight"]]), 0, effort_lookup[["daylight"]]))),
      sprintf("Sampled twilight hours used in diel analysis: %.2f", unname(ifelse(is.na(effort_lookup[["twilight"]]), 0, effort_lookup[["twilight"]]))),
      sprintf("Sampled night hours used in diel analysis: %.2f", unname(ifelse(is.na(effort_lookup[["night"]]), 0, effort_lookup[["night"]])))
    )
  }

  if (nrow(diel_species_summary) > 0) {
    day_species <- diel_species_summary[which.min(diel_species_summary$log2_night_day_rate_ratio), , drop = FALSE]
    night_species <- diel_species_summary[which.max(diel_species_summary$log2_night_day_rate_ratio), , drop = FALSE]
    analysis_lines <- c(
      analysis_lines,
      sprintf(
        "strongest daylight-biased species by normalised rate: %s (log2 night/day rate ratio = %.2f)",
        day_species$species_label[[1]],
        day_species$log2_night_day_rate_ratio[[1]]
      ),
      sprintf(
        "strongest night-biased species by normalised rate: %s (log2 night/day rate ratio = %.2f)",
        night_species$species_label[[1]],
        night_species$log2_night_day_rate_ratio[[1]]
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

# other user-defined settings ----------------------------------------------------
summary_root <- normalizePath(file.path(repo_root, "out"), mustWork = TRUE)
output_root <- file.path(summary_root, "analysis")
# -------------------------------------------------------------------------

if (!is.numeric(bin_minutes) || length(bin_minutes) != 1 || is.na(bin_minutes) || bin_minutes <= 0) {
  stop("bin_minutes must be a single positive number")
}

if (!is.numeric(top_species_time_bin_minutes) || length(top_species_time_bin_minutes) != 1 ||
    is.na(top_species_time_bin_minutes) || top_species_time_bin_minutes <= 0) {
  stop("top_species_time_bin_minutes must be a single positive number")
}

if (!is.numeric(rolling_mean_window_days) || length(rolling_mean_window_days) != 1 ||
    is.na(rolling_mean_window_days) || rolling_mean_window_days <= 0) {
  stop("rolling_mean_window_days must be a single positive number")
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
top_species_time_bin_minutes <- as.numeric(top_species_time_bin_minutes)
rolling_mean_window_days <- as.numeric(rolling_mean_window_days)
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
summary_file_metadata <- build_summary_file_metadata(summary_csv_files, timezone = analysis_timezone)

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
file_status$recorder_id <- summary_file_metadata$recorder_id
file_status$recording_start_time <- summary_file_metadata$recording_start_time
file_status$local_date <- summary_file_metadata$local_date
file_status$recording_latitude <- summary_file_metadata$recording_latitude
file_status$recording_longitude <- summary_file_metadata$recording_longitude

loaded_tables <- lapply(seq_along(summary_results), function(index) {
  summary_table <- summary_results[[index]]$data

  if (nrow(summary_table) == 0) {
    return(NULL)
  }

  summary_table$source_summary_csv <- summary_csv_files[[index]]
   summary_table$recording_start_time <- summary_file_metadata$recording_start_time[[index]]
   summary_table$recording_latitude <- summary_file_metadata$recording_latitude[[index]]
   summary_table$recording_longitude <- summary_file_metadata$recording_longitude[[index]]
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
combined_detections$species_label <- paste0(combined_detections$common_name, " (", combined_detections$scientific_name, ")")
combined_detections$recorder_id <- vapply(combined_detections$source_summary_csv, extract_recorder_id, character(1))
filtered_detections <- combined_detections[combined_detections$confidence >= min_confidence, , drop = FALSE]

if (nrow(filtered_detections) == 0) {
  stop("No detections remain after applying min_confidence.")
}

filtered_detections$month_num <- as.integer(format(filtered_detections$date_time, "%m"))
filtered_detections$month_label <- factor(month.abb[filtered_detections$month_num], levels = month.abb)
filtered_detections$month_start <- as.Date(strftime(filtered_detections$date_time, "%Y-%m-01", tz = analysis_timezone))
filtered_detections$local_date <- as.Date(strftime(filtered_detections$date_time, "%Y-%m-%d", tz = analysis_timezone))
filtered_detections$local_hour <- as.numeric(strftime(filtered_detections$date_time, "%H", tz = analysis_timezone)) +
  as.numeric(strftime(filtered_detections$date_time, "%M", tz = analysis_timezone)) / 60 +
  as.numeric(strftime(filtered_detections$date_time, "%S", tz = analysis_timezone)) / 3600
filtered_detections$time_bin <- floor_to_bin(
  filtered_detections$date_time,
  bin_minutes = bin_minutes,
  timezone = analysis_timezone
)
light_phase_bundle <- build_light_phase_lookup(
  local_dates = filtered_detections$local_date,
  latitudes = filtered_detections$recording_latitude,
  longitudes = filtered_detections$recording_longitude,
  timezone = analysis_timezone
)

if (nrow(light_phase_bundle$solar_times) > 0) {
  light_phase_keys <- paste(
    light_phase_bundle$solar_times$local_date,
    signif(light_phase_bundle$solar_times$latitude, 8),
    signif(light_phase_bundle$solar_times$longitude, 8),
    sep = "|"
  )
  detection_keys <- paste(
    filtered_detections$local_date,
    signif(filtered_detections$recording_latitude, 8),
    signif(filtered_detections$recording_longitude, 8),
    sep = "|"
  )
  light_phase_match <- match(detection_keys, light_phase_keys)
  filtered_detections$civil_dawn <- light_phase_bundle$solar_times$civil_dawn[light_phase_match]
  filtered_detections$sunrise <- light_phase_bundle$solar_times$sunrise[light_phase_match]
  filtered_detections$sunset <- light_phase_bundle$solar_times$sunset[light_phase_match]
  filtered_detections$civil_dusk <- light_phase_bundle$solar_times$civil_dusk[light_phase_match]
  filtered_detections$light_phase <- vapply(
    seq_len(nrow(filtered_detections)),
    function(index) {
      if (is.na(light_phase_match[[index]])) {
        return(NA_character_)
      }
      classify_light_phase(
        date_time = filtered_detections$date_time[[index]],
        solar_time_row = light_phase_bundle$solar_times[light_phase_match[[index]], , drop = FALSE]
      )
    },
    character(1)
  )
} else {
  filtered_detections$civil_dawn <- na_posixct(analysis_timezone)
  filtered_detections$sunrise <- na_posixct(analysis_timezone)
  filtered_detections$sunset <- na_posixct(analysis_timezone)
  filtered_detections$civil_dusk <- na_posixct(analysis_timezone)
  filtered_detections$light_phase <- NA_character_
}

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
time_series_summary <- add_running_mean_to_time_series(
  time_series_summary,
  bin_minutes = bin_minutes,
  running_days = rolling_mean_window_days
)

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

top_species_time_series <- build_top_species_time_series(
  filtered_detections,
  bin_minutes = top_species_time_bin_minutes,
  top_n = 10L,
  timezone = analysis_timezone
)
top_species_time_series_positive <- top_species_time_series[
  !is.na(top_species_time_series$identification_count_plot),
  ,
  drop = FALSE
]
top_species_label_parser <- build_species_label_parser(species_label_plotmath_lookup)
top_species_style <- top_species_style_values(levels(top_species_time_series$species_label))

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
    subset_time_series <- build_time_series_summary_for_subset(
      subset_detections,
      bin_minutes,
      analysis_timezone,
      rolling_mean_window_days = rolling_mean_window_days
    )
    subset_time_series$recorder_id <- recorder_id
    subset_time_series
  })
)

top_species_time_series_by_recorder <- do.call(
  rbind,
  lapply(recorder_ids, function(recorder_id) {
    subset_detections <- filtered_detections[filtered_detections$recorder_id == recorder_id, , drop = FALSE]
    subset_top_species <- build_top_species_time_series(
      subset_detections,
      bin_minutes = top_species_time_bin_minutes,
      top_n = 10L,
      timezone = analysis_timezone
    )
    subset_top_species$recorder_id <- recorder_id
    subset_top_species
  })
)
top_species_time_series_by_recorder_positive <- top_species_time_series_by_recorder[
  !is.na(top_species_time_series_by_recorder$identification_count_plot),
  ,
  drop = FALSE
]
top_species_by_recorder_style <- top_species_style_values(unique(as.character(top_species_time_series_by_recorder$species_label)))

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

overall_temporal_bundle <- build_temporal_diagnostics_bundle(
  time_series_summary = time_series_summary,
  bin_minutes = bin_minutes,
  periodicity_max_lag_bins = periodicity_max_lag_bins
)
temporal_diagnostics <- overall_temporal_bundle$diagnostics
temporal_tests <- overall_temporal_bundle$tests
temporal_peaks <- overall_temporal_bundle$peaks

temporal_diagnostics_by_recorder <- bind_data_frames(
  lapply(recorder_ids, function(recorder_id) {
    subset_time_series <- time_series_by_recorder[time_series_by_recorder$recorder_id == recorder_id, , drop = FALSE]
    subset_bundle <- build_temporal_diagnostics_bundle(subset_time_series, bin_minutes, periodicity_max_lag_bins)
    if (nrow(subset_bundle$diagnostics) == 0) {
      return(NULL)
    }
    subset_bundle$diagnostics$recorder_id <- recorder_id
    subset_bundle$diagnostics
  }),
  data.frame(empty_temporal_diagnostics_df(), recorder_id = character(), stringsAsFactors = FALSE)
)

temporal_tests_by_recorder <- bind_data_frames(
  lapply(recorder_ids, function(recorder_id) {
    subset_time_series <- time_series_by_recorder[time_series_by_recorder$recorder_id == recorder_id, , drop = FALSE]
    subset_bundle <- build_temporal_diagnostics_bundle(subset_time_series, bin_minutes, periodicity_max_lag_bins)
    if (nrow(subset_bundle$tests) == 0) {
      return(NULL)
    }
    subset_bundle$tests$recorder_id <- recorder_id
    subset_bundle$tests
  }),
  data.frame(empty_temporal_tests_df(), recorder_id = character(), stringsAsFactors = FALSE)
)

temporal_peaks_by_recorder <- bind_data_frames(
  lapply(recorder_ids, function(recorder_id) {
    subset_time_series <- time_series_by_recorder[time_series_by_recorder$recorder_id == recorder_id, , drop = FALSE]
    subset_bundle <- build_temporal_diagnostics_bundle(subset_time_series, bin_minutes, periodicity_max_lag_bins)
    if (nrow(subset_bundle$peaks) == 0) {
      return(NULL)
    }
    subset_bundle$peaks$recorder_id <- recorder_id
    subset_bundle$peaks
  }),
  data.frame(empty_temporal_peaks_df(), recorder_id = character(), stringsAsFactors = FALSE)
)

detection_metric_name <- "number of detections per bin"

acf_table <- temporal_diagnostics[
  temporal_diagnostics$metric_name == detection_metric_name &
    temporal_diagnostics$panel == "autocorrelation (ACF)",
  c("lag_bin", "lag_hours", "y_value", "significance_limit"),
  drop = FALSE
]
names(acf_table)[names(acf_table) == "y_value"] <- "autocorrelation"

spectrum_table <- temporal_diagnostics[
  temporal_diagnostics$metric_name == detection_metric_name &
    temporal_diagnostics$panel == "spectral density",
  c("frequency_cycles_per_bin", "period_bins", "period_hours", "y_value"),
  drop = FALSE
]
names(spectrum_table)[names(spectrum_table) == "y_value"] <- "spectral_density"

periodicity_by_recorder <- temporal_diagnostics_by_recorder[
  temporal_diagnostics_by_recorder$metric_name == detection_metric_name,
  c("metric_name", "panel", "x_value", "y_value", "significance_limit", "recorder_id"),
  drop = FALSE
]

if (nrow(light_phase_bundle$schedules) > 0) {
  light_phase_calendar_long <- aggregate(
    list(
      phase_hours = as.numeric(
        difftime(
          light_phase_bundle$schedules$interval_end,
          light_phase_bundle$schedules$interval_start,
          units = "hours"
        )
      )
    ),
    by = list(
      local_date = light_phase_bundle$schedules$local_date,
      latitude = light_phase_bundle$schedules$latitude,
      longitude = light_phase_bundle$schedules$longitude,
      light_phase = light_phase_bundle$schedules$light_phase
    ),
    FUN = sum
  )
  light_phase_calendar <- reshape(
    light_phase_calendar_long,
    idvar = c("local_date", "latitude", "longitude"),
    timevar = "light_phase",
    direction = "wide"
  )
  names(light_phase_calendar) <- sub("^phase_hours\\.", "", names(light_phase_calendar))
  names(light_phase_calendar)[names(light_phase_calendar) %in% c("daylight", "twilight", "night")] <- paste0(
    names(light_phase_calendar)[names(light_phase_calendar) %in% c("daylight", "twilight", "night")],
    "_hours"
  )
  light_phase_calendar <- merge(
    light_phase_bundle$solar_times,
    light_phase_calendar,
    by = c("local_date", "latitude", "longitude"),
    all.x = TRUE
  )

  for (phase_name in c("daylight_hours", "twilight_hours", "night_hours")) {
    if (!phase_name %in% names(light_phase_calendar)) {
      light_phase_calendar[[phase_name]] <- 0
    }
    light_phase_calendar[[phase_name]][is.na(light_phase_calendar[[phase_name]])] <- 0
  }
} else {
  light_phase_calendar_long <- data.frame(
    local_date = as.Date(character()),
    latitude = numeric(),
    longitude = numeric(),
    light_phase = character(),
    phase_hours = numeric(),
    stringsAsFactors = FALSE
  )
  light_phase_calendar <- data.frame(
    local_date = as.Date(character()),
    latitude = numeric(),
    longitude = numeric(),
    civil_dawn = as.POSIXct(character()),
    sunrise = as.POSIXct(character()),
    solar_noon = as.POSIXct(character()),
    sunset = as.POSIXct(character()),
    civil_dusk = as.POSIXct(character()),
    fallback_phase = character(),
    daylight_hours = numeric(),
    twilight_hours = numeric(),
    night_hours = numeric(),
    stringsAsFactors = FALSE
  )
}

recording_phase_effort <- build_recording_phase_effort(summary_file_metadata, timezone = analysis_timezone)
if (nrow(recording_phase_effort) > 0) {
  light_phase_sampling_effort <- aggregate(
    list(sampled_hours = recording_phase_effort$sampled_hours),
    by = list(light_phase = recording_phase_effort$light_phase),
    FUN = sum
  )
  light_phase_sampling_effort_by_recorder <- aggregate(
    list(sampled_hours = recording_phase_effort$sampled_hours),
    by = list(recorder_id = recording_phase_effort$recorder_id, light_phase = recording_phase_effort$light_phase),
    FUN = sum
  )
} else {
  light_phase_sampling_effort <- data.frame(
    light_phase = character(),
    sampled_hours = numeric(),
    stringsAsFactors = FALSE
  )
  light_phase_sampling_effort_by_recorder <- data.frame(
    recorder_id = character(),
    light_phase = character(),
    sampled_hours = numeric(),
    stringsAsFactors = FALSE
  )
}

diel_detections <- filtered_detections[!is.na(filtered_detections$light_phase), , drop = FALSE]
diel_species_summary <- build_diel_species_summary(
  detections_subset = diel_detections,
  effort_summary = light_phase_sampling_effort,
  include_recorder = FALSE
)
diel_species_summary_by_recorder <- build_diel_species_summary(
  detections_subset = diel_detections,
  effort_summary = light_phase_sampling_effort_by_recorder,
  include_recorder = TRUE
)
diel_species_long <- build_diel_species_long(diel_species_summary, include_recorder = FALSE)

analysis_summary_txt <- file.path(output_dir, "birdnet_analysis_summary.txt")
input_files_csv <- file.path(output_dir, "birdnet_analysis_input_files.csv")
filtered_detections_csv <- file.path(output_dir, "birdnet_analysis_filtered_detections.csv")
light_phase_calendar_csv <- file.path(output_dir, "birdnet_light_phase_calendar.csv")
light_phase_calendar_long_csv <- file.path(output_dir, "birdnet_light_phase_calendar_long.csv")
light_phase_sampling_effort_csv <- file.path(output_dir, "birdnet_light_phase_sampling_effort.csv")
light_phase_sampling_effort_by_recorder_csv <- file.path(output_dir, "birdnet_light_phase_sampling_effort_by_recorder.csv")
diel_species_csv <- file.path(output_dir, "birdnet_diel_activity_by_species.csv")
diel_species_by_recorder_csv <- file.path(output_dir, "birdnet_diel_activity_by_species_by_recorder.csv")
time_series_csv <- file.path(output_dir, "birdnet_identifications_by_time_bin.csv")
time_series_by_recorder_csv <- file.path(output_dir, "birdnet_identifications_by_time_bin_by_recorder.csv")
top_species_time_series_csv <- file.path(output_dir, "birdnet_top_10_species_detections_through_time.csv")
top_species_time_series_by_recorder_csv <- file.path(output_dir, "birdnet_top_10_species_detections_through_time_by_recorder.csv")
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
temporal_diagnostics_csv <- file.path(output_dir, "birdnet_temporal_diagnostics.csv")
temporal_tests_csv <- file.path(output_dir, "birdnet_temporal_periodicity_tests.csv")
temporal_peaks_csv <- file.path(output_dir, "birdnet_temporal_spectral_peaks.csv")
periodicity_by_recorder_csv <- file.path(output_dir, "birdnet_identification_periodicity_by_recorder.csv")
temporal_diagnostics_by_recorder_csv <- file.path(output_dir, "birdnet_temporal_diagnostics_by_recorder.csv")
temporal_tests_by_recorder_csv <- file.path(output_dir, "birdnet_temporal_periodicity_tests_by_recorder.csv")
temporal_peaks_by_recorder_csv <- file.path(output_dir, "birdnet_temporal_spectral_peaks_by_recorder.csv")

write.csv(file_status, input_files_csv, row.names = FALSE)
write.csv(filtered_detections, filtered_detections_csv, row.names = FALSE)
write.csv(light_phase_calendar, light_phase_calendar_csv, row.names = FALSE)
write.csv(light_phase_calendar_long, light_phase_calendar_long_csv, row.names = FALSE)
write.csv(light_phase_sampling_effort, light_phase_sampling_effort_csv, row.names = FALSE)
write.csv(light_phase_sampling_effort_by_recorder, light_phase_sampling_effort_by_recorder_csv, row.names = FALSE)
write.csv(diel_species_summary, diel_species_csv, row.names = FALSE)
write.csv(diel_species_summary_by_recorder, diel_species_by_recorder_csv, row.names = FALSE)
write.csv(time_series_summary, time_series_csv, row.names = FALSE)
write.csv(time_series_by_recorder, time_series_by_recorder_csv, row.names = FALSE)
write.csv(top_species_time_series, top_species_time_series_csv, row.names = FALSE)
write.csv(top_species_time_series_by_recorder, top_species_time_series_by_recorder_csv, row.names = FALSE)
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
write.csv(temporal_diagnostics, temporal_diagnostics_csv, row.names = FALSE)
write.csv(temporal_tests, temporal_tests_csv, row.names = FALSE)
write.csv(temporal_peaks, temporal_peaks_csv, row.names = FALSE)
write.csv(periodicity_by_recorder, periodicity_by_recorder_csv, row.names = FALSE)
write.csv(temporal_diagnostics_by_recorder, temporal_diagnostics_by_recorder_csv, row.names = FALSE)
write.csv(temporal_tests_by_recorder, temporal_tests_by_recorder_csv, row.names = FALSE)
write.csv(temporal_peaks_by_recorder, temporal_peaks_by_recorder_csv, row.names = FALSE)

generated_at <- Sys.time()
write_analysis_summary(
  summary_txt = analysis_summary_txt,
  generated_at = generated_at,
  summary_root = summary_root,
  output_dir = output_dir,
  bin_minutes = bin_minutes,
  rolling_mean_window_days = rolling_mean_window_days,
  min_confidence = min_confidence,
  file_status = file_status,
  filtered_detections = filtered_detections,
  light_phase_sampling_effort = light_phase_sampling_effort,
  diel_species_summary = diel_species_summary
)

plot_subtitle <- sprintf(
  "bin size: %s min | minimum confidence: %.3f",
  as.integer(round(bin_minutes)),
  min_confidence
)
time_series_plot_subtitle <- paste0(
  plot_subtitle,
  sprintf(" | red line = %.3g-day running mean", rolling_mean_window_days)
)
time_series_plot_linear_subtitle <- plot_subtitle
recorder_reference_locations <- build_recorder_reference_locations(summary_file_metadata)
overall_light_phase_bands <- build_plot_light_phase_bands(
  reference_locations = recorder_reference_locations,
  local_dates = as.Date(time_series_summary$time_bin, tz = analysis_timezone),
  timezone = analysis_timezone,
  aggregate_across_locations = TRUE
)
by_recorder_light_phase_bands <- build_plot_light_phase_bands(
  reference_locations = recorder_reference_locations,
  local_dates = as.Date(time_series_summary$time_bin, tz = analysis_timezone),
  timezone = analysis_timezone,
  aggregate_across_locations = FALSE
)

time_series_plot <- ggplot2::ggplot(
  time_series_summary,
  ggplot2::aes(x = time_bin, y = identification_count_plot)
) +
  ggplot2::geom_col(fill = "steelblue", alpha = 0.5, width = bin_minutes * 60 * 0.9, na.rm = TRUE) +
  ggplot2::geom_line(
    ggplot2::aes(y = identification_count_running_mean_plot),
    colour = "firebrick2",
    linewidth = 1.2,
    linetype = "dashed",
    na.rm = TRUE
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "BirdNET identifications over time",
    subtitle = time_series_plot_subtitle,
    x = "time bin",
    y = expression("identifications per bin (" * log[10] * " scale)")
  ) +
  analysis_plot_theme()

time_series_plot_linear <- ggplot2::ggplot(
  time_series_summary,
  ggplot2::aes(x = time_bin, y = identification_count)
) +
  light_phase_band_layers(overall_light_phase_bands) +
  ggplot2::geom_col(fill = "black", alpha = 0.5, width = bin_minutes * 60 * 0.9) +
  ggplot2::labs(
    title = "BirdNET identifications over time",
    subtitle = time_series_plot_linear_subtitle,
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
    y = expression("number of identifications (" * log[10] * " scale)")
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
    y = expression("number of identifications (" * log[10] * " scale)")
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

top_species_plot <- ggplot2::ggplot(
  top_species_time_series,
  ggplot2::aes(x = time_bin, y = identification_count_plot, colour = species_label, group = species_label)
) +
  ggplot2::geom_line(
    data = top_species_time_series_positive,
    ggplot2::aes(linetype = species_label, linewidth = species_label)
  ) +
  ggplot2::geom_point(
    data = top_species_time_series_positive,
    ggplot2::aes(shape = species_label, size = species_label),
    stroke = 0.7
  ) +
  top_species_scale_layers(
    style_values = top_species_style,
    label_parser = top_species_label_parser
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "detections through time for the 10 most detected species",
    subtitle = sprintf("bin size: %s hours | minimum confidence: %.3f", round(top_species_time_bin_minutes / 60, 2), min_confidence),
    x = "time bin",
    y = expression("number of detections (" * log[10] * " scale)"),
    colour = "species"
  ) +
  top_species_plot_theme()

time_series_by_recorder_plot <- ggplot2::ggplot(
  time_series_by_recorder,
  ggplot2::aes(x = time_bin, y = identification_count_plot)
) +
  ggplot2::geom_col(fill = "steelblue", alpha = 0.5, width = bin_minutes * 60 * 0.9, na.rm = TRUE) +
  ggplot2::geom_line(
    ggplot2::aes(y = identification_count_running_mean_plot),
    colour = "firebrick2",
    linewidth = 1.2,
    linetype = "dashed",
    na.rm = TRUE
  ) +
  ggplot2::facet_grid(recorder_id ~ ., scales = "free_y") +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "BirdNET identifications over time by recorder",
    subtitle = time_series_plot_subtitle,
    x = "time bin",
    y = expression("identifications per bin (" * log[10] * " scale)")
  ) +
  analysis_plot_theme()

time_series_by_recorder_plot_linear <- ggplot2::ggplot(
  time_series_by_recorder,
  ggplot2::aes(x = time_bin, y = identification_count)
) +
  light_phase_band_layers(by_recorder_light_phase_bands) +
  ggplot2::geom_col(fill = "black", alpha = 0.5, width = bin_minutes * 60 * 0.9) +
  ggplot2::facet_grid(recorder_id ~ ., scales = "free_y") +
  ggplot2::labs(
    title = "BirdNET identifications over time by recorder",
    subtitle = time_series_plot_linear_subtitle,
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
    y = expression("number of identifications (" * log[10] * " scale)")
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
    y = expression("number of identifications (" * log[10] * " scale)")
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
  ggplot2::facet_wrap(
    ~paste(recorder_id, metric_name, sep = "\n"),
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::labs(
    title = "monthly diversity metrics by recorder",
    subtitle = "detections treated as relative abundance for Shannon, Simpson, and Hill numbers",
    x = "month",
    y = "metric value"
  ) +
  ggplot2::scale_x_date(date_labels = "%Y-%m") +
  analysis_plot_theme()

top_species_by_recorder_plot <- ggplot2::ggplot(
  top_species_time_series_by_recorder,
  ggplot2::aes(x = time_bin, y = identification_count_plot, colour = species_label, group = species_label)
) +
  ggplot2::geom_line(
    data = top_species_time_series_by_recorder_positive,
    ggplot2::aes(linetype = species_label, linewidth = species_label)
  ) +
  ggplot2::geom_point(
    data = top_species_time_series_by_recorder_positive,
    ggplot2::aes(shape = species_label, size = species_label),
    stroke = 0.7
  ) +
  ggplot2::facet_grid(recorder_id ~ ., scales = "free_y") +
  top_species_scale_layers(
    style_values = top_species_by_recorder_style,
    label_parser = top_species_label_parser
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "detections through time for the 10 most detected species by recorder",
    subtitle = sprintf("bin size: %s hours | minimum confidence: %.3f", round(top_species_time_bin_minutes / 60, 2), min_confidence),
    x = "time bin",
    y = expression("number of detections (" * log[10] * " scale)"),
    colour = "species"
  ) +
  top_species_plot_theme()

diel_plot_subtitle <- paste(
  "normalised by sampled hours within local daylight, civil twilight, and darkness",
  sprintf("| minimum confidence: %.3f", min_confidence)
)

if (nrow(diel_species_summary) > 0 && any(is.finite(diel_species_summary$log2_night_day_rate_ratio))) {
  diel_top_species <- diel_species_summary[seq_len(min(20L, nrow(diel_species_summary))), , drop = FALSE]
  diel_top_species_long <- diel_species_long[diel_species_long$species_label %in% diel_top_species$species_label, , drop = FALSE]
  heatmap_species_levels <- rev(diel_top_species$species_label)
  diel_top_species_long$species_label <- factor(as.character(diel_top_species_long$species_label), levels = heatmap_species_levels)
  diel_top_species_long$detections_per_hour_plot <- ifelse(
    diel_top_species_long$detections_per_hour > 0,
    diel_top_species_long$detections_per_hour,
    NA_real_
  )

  diel_bias_species <- diel_top_species[order(diel_top_species$log2_night_day_rate_ratio, diel_top_species$species_label), , drop = FALSE]
  diel_bias_species$species_label <- factor(as.character(diel_bias_species$species_label), levels = diel_bias_species$species_label)

  diel_activity_heatmap_plot <- ggplot2::ggplot(
    diel_top_species_long,
    ggplot2::aes(x = light_phase, y = species_label, fill = detections_per_hour_plot)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_x_discrete(
      labels = c(daylight = "daylight", twilight = "civil twilight", night = "darkness"),
      drop = FALSE
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        parse(text = unname(species_label_plotmath_lookup[as.character(x)]))
      }
    ) +
    ggplot2::scale_fill_gradient(
      low = "grey95",
      high = "midnightblue",
      trans = scales::log2_trans(),
      na.value = "grey90"
    ) +
    ggplot2::labs(
      title = "local light-phase calling activity by species",
      subtitle = diel_plot_subtitle,
      x = "local light phase",
      y = "species",
      fill = expression(log[2] * "(detections per hour)")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(size = 8.4),
      panel.grid = ggplot2::element_blank()
    )

  diel_preference_plot <- ggplot2::ggplot(
    diel_bias_species,
    ggplot2::aes(x = log2_night_day_rate_ratio, y = species_label, fill = dominant_light_phase)
  ) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5, linetype = "dashed") +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        parse(text = unname(species_label_plotmath_lookup[as.character(x)]))
      }
    ) +
    ggplot2::scale_fill_manual(
      values = c(daylight = "goldenrod2", twilight = "darkorchid3", night = "midnightblue"),
      labels = c(daylight = "daylight", twilight = "civil twilight", night = "darkness"),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "normalised night-versus-day calling bias by species",
      subtitle = diel_plot_subtitle,
      x = expression(log[2] * "(night/day detections per hour)"),
      y = "species",
      fill = "highest\nrate phase"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(size = 8.4),
      panel.grid.minor = ggplot2::element_blank()
    )
} else {
  diel_activity_heatmap_plot <- make_placeholder_plot(
    title_text = "local light-phase calling activity by species",
    subtitle_text = diel_plot_subtitle,
    body_text = paste(
      "not enough detections with recoverable timestamps and coordinates",
      "are currently available to summarize daylight, twilight, and darkness activity.",
      sep = "\n"
    )
  )
  diel_preference_plot <- make_placeholder_plot(
    title_text = "normalised night-versus-day calling bias by species",
    subtitle_text = diel_plot_subtitle,
    body_text = paste(
      "not enough detections with recoverable timestamps and coordinates",
      "are currently available to compare normalised night and daylight calling rates.",
      sep = "\n"
    )
  )
}

temporal_diagnostics$facet_label <- paste(temporal_diagnostics$metric_name, temporal_diagnostics$panel, sep = "\n")
temporal_peaks$facet_label <- paste(temporal_peaks$metric_name, "spectral density", sep = "\n")
periodicity_plot <- build_temporal_diagnostics_plot(
  diagnostics_df = temporal_diagnostics,
  peaks_df = temporal_peaks,
  title_text = "temporal periodicity diagnostics for detections and species richness",
  subtitle_text = plot_subtitle,
  facet_column = "facet_label",
  ncol = 2,
  placeholder_text = paste(
    "not enough variation or time bins are currently available",
    "for autocorrelation, partial-autocorrelation, or spectral periodicity analysis.",
    sep = "\n"
  )
)

temporal_diagnostics_by_recorder$facet_label <- paste(
  temporal_diagnostics_by_recorder$recorder_id,
  temporal_diagnostics_by_recorder$metric_name,
  temporal_diagnostics_by_recorder$panel,
  sep = "\n"
)
temporal_peaks_by_recorder$facet_label <- paste(
  temporal_peaks_by_recorder$recorder_id,
  temporal_peaks_by_recorder$metric_name,
  "spectral density",
  sep = "\n"
)
periodicity_by_recorder_plot <- build_temporal_diagnostics_plot(
  diagnostics_df = temporal_diagnostics_by_recorder,
  peaks_df = temporal_peaks_by_recorder,
  title_text = "temporal periodicity diagnostics by recorder",
  subtitle_text = plot_subtitle,
  facet_column = "facet_label",
  ncol = 2,
  placeholder_text = paste(
    "not enough variation or time bins are currently available",
    "for recorder-specific autocorrelation, PACF, or spectral analysis.",
    sep = "\n"
  )
)

if (isTRUE(show_plots_in_session) && interactive()) {
  while (grDevices::dev.cur() > 1) {
    grDevices::dev.off()
  }

  print(time_series_plot)
  print(time_series_plot_linear)
  print(cumulative_species_plot)
  print(species_counts_plot)
  print(species_counts_by_month_plot)
  print(monthly_diversity_plot)
  print(top_species_plot)
  print(diel_activity_heatmap_plot)
  print(diel_preference_plot)
  print(time_series_by_recorder_plot)
  print(time_series_by_recorder_plot_linear)
  print(top_species_by_recorder_plot)
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
  filename = file.path(output_dir, "birdnet_identifications_over_time_linear.png"),
  plot = time_series_plot_linear,
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
  filename = file.path(output_dir, "birdnet_top_10_species_detections_through_time.png"),
  plot = top_species_plot,
  width = 14,
  height = 8,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_diel_activity_by_species.png"),
  plot = diel_activity_heatmap_plot,
  width = 13,
  height = 9,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_day_night_calling_bias_by_species.png"),
  plot = diel_preference_plot,
  width = 13,
  height = 9,
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
  filename = file.path(output_dir, "birdnet_identifications_over_time_by_recorder_linear.png"),
  plot = time_series_by_recorder_plot_linear,
  width = 14,
  height = max(7, 3 * length(recorder_ids)),
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_top_10_species_detections_through_time_by_recorder.png"),
  plot = top_species_by_recorder_plot,
  width = 15,
  height = max(8, 3 * length(recorder_ids)),
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
  width = 15,
  height = 11,
  dpi = 150
)
ggplot2::ggsave(
  filename = file.path(output_dir, "birdnet_periodicity_by_recorder.png"),
  plot = periodicity_by_recorder_plot,
  width = 16,
  height = max(10, 4.2 * length(recorder_ids)),
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
  recorder_temporal_diagnostics <- temporal_diagnostics_by_recorder[
    temporal_diagnostics_by_recorder$recorder_id == recorder_id,
    ,
    drop = FALSE
  ]
  recorder_temporal_peaks <- temporal_peaks_by_recorder[
    temporal_peaks_by_recorder$recorder_id == recorder_id,
    ,
    drop = FALSE
  ]
  recorder_top_species <- top_species_time_series_by_recorder[
    top_species_time_series_by_recorder$recorder_id == recorder_id,
    ,
    drop = FALSE
  ]
  recorder_top_species_levels <- unique(as.character(recorder_top_species$species_label))
  recorder_top_species_lookup <- species_label_plotmath_lookup[recorder_top_species_levels]
  recorder_top_species_label_parser <- build_species_label_parser(recorder_top_species_lookup)
  recorder_top_species_style <- top_species_style_values(recorder_top_species_levels)
  recorder_top_species$species_label <- factor(as.character(recorder_top_species$species_label), levels = recorder_top_species_levels)
  recorder_light_phase_bands <- by_recorder_light_phase_bands[
    by_recorder_light_phase_bands$recorder_id == recorder_id,
    ,
    drop = FALSE
  ]
  if (nrow(recorder_light_phase_bands) == 0) {
    recorder_reference_location <- recorder_reference_locations[
      recorder_reference_locations$recorder_id == recorder_id,
      ,
      drop = FALSE
    ]
    recorder_light_phase_bands <- build_plot_light_phase_bands(
      reference_locations = recorder_reference_location,
      local_dates = as.Date(recorder_time_series$time_bin, tz = analysis_timezone),
      timezone = analysis_timezone,
      aggregate_across_locations = FALSE
    )
  }

  recorder_time_series_plot <- ggplot2::ggplot(
    recorder_time_series,
    ggplot2::aes(x = time_bin, y = identification_count_plot)
  ) +
    ggplot2::geom_col(fill = "black", width = bin_minutes * 60 * 0.9, na.rm = TRUE) +
    ggplot2::geom_line(
      ggplot2::aes(y = identification_count_running_mean_plot),
      colour = "firebrick2",
      linewidth = 1.2,
      linetype = "dashed",
      na.rm = TRUE
    ) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = sprintf("BirdNET identifications over time: %s", recorder_id),
      subtitle = time_series_plot_subtitle,
      x = "time bin",
      y = expression("identifications per bin (" * log[10] * " scale)")
    ) +
    analysis_plot_theme()

  recorder_time_series_plot_linear <- ggplot2::ggplot(
    recorder_time_series,
    ggplot2::aes(x = time_bin, y = identification_count)
  ) +
    light_phase_band_layers(recorder_light_phase_bands) +
    ggplot2::geom_col(fill = "black", width = bin_minutes * 60 * 0.9) +
    ggplot2::labs(
      title = sprintf("BirdNET identifications over time: %s", recorder_id),
      subtitle = time_series_plot_linear_subtitle,
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
      y = expression("number of identifications (" * log[10] * " scale)")
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
      y = expression("number of identifications (" * log[10] * " scale)")
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

  recorder_top_species_plot <- ggplot2::ggplot(
    recorder_top_species,
    ggplot2::aes(x = time_bin, y = identification_count_plot, colour = species_label, group = species_label)
  ) +
    ggplot2::geom_line(
      data = recorder_top_species[!is.na(recorder_top_species$identification_count_plot), , drop = FALSE],
      ggplot2::aes(linetype = species_label, linewidth = species_label)
    ) +
    ggplot2::geom_point(
      data = recorder_top_species[!is.na(recorder_top_species$identification_count_plot), , drop = FALSE],
      ggplot2::aes(shape = species_label, size = species_label),
      stroke = 0.7
    ) +
    top_species_scale_layers(
      style_values = recorder_top_species_style,
      label_parser = recorder_top_species_label_parser
    ) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = sprintf("detections through time for the 10 most detected species: %s", recorder_id),
      subtitle = sprintf("bin size: %s hours | minimum confidence: %.3f", round(top_species_time_bin_minutes / 60, 2), min_confidence),
      x = "time bin",
      y = expression("number of detections (" * log[10] * " scale)"),
      colour = "species"
    ) +
    top_species_plot_theme()

  recorder_temporal_diagnostics$facet_label <- paste(
    recorder_temporal_diagnostics$metric_name,
    recorder_temporal_diagnostics$panel,
    sep = "\n"
  )
  recorder_temporal_peaks$facet_label <- paste(
    recorder_temporal_peaks$metric_name,
    "spectral density",
    sep = "\n"
  )
  recorder_periodicity_plot <- build_temporal_diagnostics_plot(
    diagnostics_df = recorder_temporal_diagnostics,
    peaks_df = recorder_temporal_peaks,
    title_text = sprintf("temporal periodicity diagnostics: %s", recorder_id),
    subtitle_text = plot_subtitle,
    facet_column = "facet_label",
    ncol = 2,
    placeholder_text = paste(
      "not enough variation or time bins are currently available",
      "for recorder-specific autocorrelation, PACF, or spectral analysis.",
      sep = "\n"
    )
  )

  ggplot2::ggsave(file.path(recorder_dir, "birdnet_identifications_over_time.png"), recorder_time_series_plot, width = 12, height = 7, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_identifications_over_time_linear.png"), recorder_time_series_plot_linear, width = 12, height = 7, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_cumulative_new_species.png"), recorder_cumulative_plot, width = 12, height = 7, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_identifications_by_species.png"), recorder_species_plot, width = 13, height = 10, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_identifications_by_species_by_month.png"), recorder_species_by_month_plot, width = 16, height = 12, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_monthly_diversity_metrics.png"), recorder_diversity_plot, width = 14, height = 10, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_top_10_species_detections_through_time.png"), recorder_top_species_plot, width = 14, height = 8, dpi = 150)
  ggplot2::ggsave(file.path(recorder_dir, "birdnet_periodicity.png"), recorder_periodicity_plot, width = 15, height = 11, dpi = 150)
}

message(sprintf("Analysis complete. Outputs written to: %s", output_dir))
