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

audio_file <- normalizePath(
  "~/Documents/Biology/Birds/acoustics/GEL_A/20251123_AAO_GEL_A-35.52235+139.10576/20251123T080000+0930_REC_-35.52235+139.10576.wav",
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

pipeline <- initialize_birdnet_pipeline(
  species_csv = species_csv,
  timezone = "Australia/Adelaide",
  fallback_latitude = -35.52235,
  fallback_longitude = 139.10576,
  prediction_min_confidence = 0.05,
  summary_confidence_threshold = 0.05,
  use_arrow = TRUE
)

result <- process_audio_file(
  pipeline = pipeline,
  audio_file = audio_file,
  output_dir = dirname(audio_file),
  allow_empty_summary = FALSE
)

summary_table <- result$summary_table
summary_table
