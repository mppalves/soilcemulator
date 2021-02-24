#' @title run_batch_analysis
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @description run_analysis for cross valiation batches
#' @param modelid  model id output from \link{write_run_info}
#' @param targetdir output directory
#' @param .title String used to name the plots and the exported files
#' @param .cor Hexadecimal or color name used to color details in plots
#' @param .unit unit measure for the output
#' @export run_analysis
#' @import stringr
#' @import ggplot2
#' @export

run_batch_analysis <- function(modelid, targetdir, .cor = "green", .title = "test", .unit = "z") {
  output_dir <- grep(modelid, list.dirs(targetdir, recursive = F), value = T)
  runs <- list.dirs(output_dir, recursive = F, full.names = F)
  setwd(output_dir)
  for (run in runs) {
    input_label <- readRDS(list.files(run, pattern = "input.Rds", full.names = T))
    output_label <- readRDS(list.files(run, pattern = "output.Rds", full.names = T))
    setwd(run)
    run_analysis(input_label, output_label, cor = .cor, title = .title, unit = .unit)
    setwd(output_dir)
  }
}
