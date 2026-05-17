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
source(file.path(script_dir, "downloading_user_options.R"), local = globalenv())

if (!source_mode %in% c("archive", "ecosounds")) {
  stop("source_mode in downloading_user_options.R must be either 'archive' or 'ecosounds'.")
}

entrypoint_path <- if (identical(source_mode, "archive")) {
  file.path(script_dir, "process_tar_archive.R")
} else {
  file.path(script_dir, "process_ecosounds.R")
}

source(entrypoint_path, local = globalenv())
