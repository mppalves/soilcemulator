#' @title run_batch_rmse
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @description run_analysis for cross valiation batches
#' @param modelid  model id output from \link{write_run_info}
#' @param source_folder madrat source folder
#' @param targetdir output directory
#' @export

run_move_files <- function(source_folder, modelid, targetdir) {
  hash <- substr(modelid, 1, 6)
  output_dir <- grep(modelid, list.dirs(targetdir, recursive = F), value = T)
  setwd(output_dir)
  files <- grep(paste0("^", hash, "|mean_",hash, "|stddevs_",hash), list.files(), value = T, perl = T)
  dpath <- file.path(source_folder, hash)
  dir.create(dpath, showWarnings = F)
  file.copy(files, to = dpath, overwrite = T)
}
