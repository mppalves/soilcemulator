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
  files1 <- grep(paste0("^", hash), list.files(), value = T, perl = T)
  dpath <- file.path(source_folder, hash)
  dir.create(dpath, showWarnings = F)
  file.copy(files1, to = dpath, overwrite = T)
  dirs <- list.dirs()
  last_dir <- dirs[length(dirs)]
  if (grepl("[0-9]{4}", last_dir)) {
    setwd(last_dir)
    files2 <- grep(paste0("mean+|stddevs+"), list.files(), value = T, perl = T)
    file.copy(files2, to = dpath, overwrite = T)
  } else {
    stop("Means and stds could not be copied")
  }
  print(paste0("Files moved to ", source_folder))
  print(paste(files1))
  print(paste(files2))
}
