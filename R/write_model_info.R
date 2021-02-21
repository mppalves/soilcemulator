
write_model_info <- function(ppsample,targetdir, run_title = "Soilc", modelmeta){

  model <- modelmeta[["model"]]
  epochs <- modelmeta[["epochs"]]
  batch_size <- modelmeta[["batch_size"]]
  loss <- modelmeta[["loss"]]
  activation <- modelmeta[["activation"]]
  units <- modelmeta[["units"]]
  smp_size <- modelmeta[["smp_size"]]
  inputs <- ppsample[["inputs"]]
  train_d <- ppsample[["ppsample"]]


  ######################################################
  ### Storing the results ##############################
  ######################################################

  # writting model information
  x        <- list("Model Summary" = capture.output(model %>% summary()),
                   "epochs" = epochs, "batch_size" = batch_size, "loss" = loss, "activation" = activation,
                   "neurons" = units, "data_set split" = smp_size, "cols train data" = inputs,
                   "df head" = head(train_d), "df tail" = tail(train_d), timestamp())

  y        <- capture.output(x)
  m_hash   <- sha1(paste0(y, collapse = ""))
  n_layers <- length(grep("units", y))
  y        <- append(y, paste0("model ID hash: ", m_hash))

  # Folder name
  current_dir <- paste0(gsub("\\", "/", fileSnapshot()$path, fixed=TRUE))
  run_name <- paste0(run_title, substr(m_hash, 1, 10),"_", shaID)
  subDir   <- paste0(run_name, "_", gsub(":", ".", Sys.time()))
  dir.create(file.path(targetdir, subDir))
  setwd(file.path(targetdir, subDir))

  # Write model information
  writeLines(y, con = file(paste0("model_info_", m_hash, ".txt")))

}
