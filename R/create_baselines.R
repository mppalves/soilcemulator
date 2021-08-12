#' @title create_baselines
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @description run_analysis for cross valiation batches
#' @param modelid  model id output from \link{write_run_info}
#' @param targetdir output directory
#' @param pprosdf dataset
#' @import magclass
#' @export

create_baselines <- function(modelid, targetdir, pprosdf) {

  output_dir <- grep(modelid, list.dirs(targetdir, recursive = F), value = T)
  setwd(output_dir)
  files <- grep("output", list.files(), value = T, perl = T)
  y_hat <- readRDS(grep(".rds", files, value = T, ignore.case = FALSE))

  files <- grep("stddevs_lab", list.files(), value = T, perl = T)
  std <- readRDS(grep(".rds", files, value = T, ignore.case = FALSE))

  files <- grep("mean_lab", list.files(), value = T, perl = T)
  mean <- readRDS(grep(".rds", files, value = T, ignore.case = FALSE))

  y_hat <- y_hat * std + mean

  lsus_name <- grep("lsu", colnames(pprosdf), value = T)
  cells <- as.numeric(unique(pprosdf[["Cell"]]))
  years <- unique(pprosdf[["Year"]])
  lsus <- unique(pprosdf[[lsus_name]])
  y_array <- array(y_hat, dim = c(59199, length(lsus), length(years)), dimnames = list(cells,lsus, years))
  y_mag <- as.magpie(y_array, spatial = 1)
  write.magpie(y_mag, paste0("baselines_",modelid, ".mz"))
  return(y_mag)
}
