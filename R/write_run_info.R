#' @title write_run_info
#' @description Write compiled model info and generates model hash id
#' @return model hash id (character string)
#' @param targetdir output directory
#' @param run_title run type name
#' @param runmeta list with compiled model and parameters.
#' @param pprosdf pre processed dataframe
#' @author Marcos Alves
#' @import keras
#' @import utils
#' @importFrom digest sha1
#' @export

write_run_info <- function(targetdir, run_title = "Soilc", runmeta, pprosdf) {
  model <- runmeta[["model"]]
  epochs <- runmeta[["epochs"]]
  batch_size <- runmeta[["batch_size"]]
  units <- attr(model, "units")
  activation <- attr(model, "activation")
  loss <- attr(model, "loss")
  optimizer <- attr(model, "optimizer")
  metrics <- attr(model, "metrics")
  dataset_id <- attr(pprosdf, "dfid")

  ######################################################
  ### Storing the results ##############################
  ######################################################

  # writting model information
  x <- list(
    "Model Summary" = capture.output(model %>% summary()),
    "epochs" = epochs, "batch_size" = batch_size, "loss" = loss, "activation" = activation,
    "neurons" = units, "optimizer" = optimizer, "metrics" = metrics, "dataset id" = dataset_id
  )

  y <- capture.output(x)
  m_hash <- sha1(paste0(y, collapse = ""))
  n_layers <- length(grep("units", y))
  y <- append(y, paste0("model ID hash: ", m_hash))

  # Folder name
  current_dir <- paste0(gsub("\\", "/", fileSnapshot()$path, fixed = TRUE))
  run_name <- paste0(run_title, "_", m_hash)
  subDir <- paste0(run_name, "_", gsub(":", ".", Sys.time()))
  dir.create(file.path(targetdir, subDir))
  setwd(file.path(targetdir, subDir))

  # Write model information
  writeLines(y, con = file(paste0("model_info_", m_hash, ".txt")))
  close(file(paste0("model_info_", m_hash, ".txt")))

  return(m_hash)
}
