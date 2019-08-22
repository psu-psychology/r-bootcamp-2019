# copy_rmd_2_box.R
# Extracts and copies site-wide Rmd files to target directory

copy_rmd_2_box <- function(source_dir = ".", target_dir = "~/Box Sync/r-bootcamp-2019-files") {
  rmd_src <- list.files(path = source_dir, "\\.Rmd$", recursive = TRUE, full.names = TRUE)
  n_files <- file.copy(rmd_src, to = target_dir, recursive = TRUE)
  message("Copied ", sum(n_files), " files to ", target_dir)
}

