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

if (!exists("script_dir", inherits = FALSE)) {
  script_dir <- get_script_dir()
}

if (!exists("source_mode", inherits = TRUE)) {
  source(file.path(script_dir, "downloading_user_options.R"), local = globalenv())
}

if (!identical(source_mode, "ecosounds")) {
  stop("process_ecosounds.R requires source_mode <- 'ecosounds' in downloading_user_options.R.")
}

source(file.path(script_dir, "process_download_common.R"), local = globalenv())
