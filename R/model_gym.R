#' @title model_gym
#' @description Run model `fit` on the pre-processed sampled training data and perform model quality evaluations
#' @return Trained model
#' @param ppsample pre processed training dataset \link{ppsample}
#' @param pprosdf pre processed dataset
#' @param features boolean vector
#' @param runmeta list with compiled model and parameters.
#' @param modelid  model id output from \link{write_run_info}
#' @param tag flag
#' @author Marcos Alves
#' @import keras
#' @import tidyr
#' @import grDevices
#' @import utils
#' @importFrom stats cor
#' @importFrom stats predict
#' @importFrom stats t.test
#' @importFrom stats var.test
#' @export


model_gym <- function(pprosdf, ppsample, runmeta, modelid, features = select_features(pprosdf), tag) {

  # initiating magrrit variables
  mae <- NULL
  loss <- NULL


  model <- runmeta[["model"]]
  ppsample_y <- ppsample[["train_labels"]]
  ppsample_x <- ppsample[["train_data"]]
  timestep <- attr(ppsample, "timestep")
  dfid <- attr(ppsample, "dfid")

  ######################################################
  ### Model Fitting ####################################
  ######################################################

  # The patience parameter is the amount of epochs to check for improvement.
  early_stop <- callback_early_stopping(monitor = "loss", patience = 20)
  history <- model %>% fit(
    ppsample_x,
    ppsample_y,
    batch_size = runmeta[["batch_size"]],
    epochs = runmeta[["epochs"]],
    validation_split = 0.05,
    # verbose = 1,
    # callbacks = list(early_stop, tensor_board),
    # view_metrics = F
  )

  # setwd("..")
  # dir.create(as.character(timestep), showWarnings = T)
  # setwd(as.character(timestep))
  save_model_hdf5(model, paste0("model_", modelid, ".h5"))
  save_model_weights_hdf5(model, paste0("model_", modelid, "_weights.h5"))
  save(history, file = paste0("history_", modelid, ".rda"))
  saveRDS(get_weights(model), file = paste0(modelid,"_weights", ".rds"))

  ######################################################
  ### Model evaluation #################################
  ######################################################

  # evaluating results
  c(loss, mae) %<-% (model %>% evaluate(ppsample[["test_data"]], ppsample[["test_labels"]], verbose = 2))

  # saving training plot
  postscript(file = paste0("Learning_curve_", modelid, ".eps"), horizontal = TRUE, onefile = FALSE, width = 9, height = 5, paper = "letter")
  plot(history, metrics = "mean_absolute_error", smooth = T)
  paste0("Mean squared error on test set: ", sprintf("%.2f", mae))
  dev.off()

  test_predictions <- model %>% predict(ppsample[["test_data"]])
  up_lim <- min(nrow(ppsample[["test_data"]]), 2000)
  test_results <- cbind(test_predictions[1:up_lim], ppsample[["test_labels"]][1:up_lim])
  test_corr <- cor(test_predictions[1:up_lim], ppsample[["test_labels"]][1:up_lim])

  # writing results
  RMSE <- function(m, o) {
    sqrt(mean((m - o)^2))
  }

  XRMSE <- RMSE(test_predictions, ppsample[["test_labels"]])
  x <- list("loss" = loss, "Mean absolute error on test set" = mae, test_results, "correlation" = test_corr, "RMSE" = XRMSE)
  write.csv(x, file = paste0("excell_comparisson", "_", modelid, "_", dfid, ".csv"))
  close(file(paste0("excell_comparisson", "_", modelid, "_", dfid, ".csv")))

  valid_data <- tsample(pprosdf, features, plot_test = F, tag=tag)

  # exporting inputs and outputs for quality analysis
  output_data <- model %>% predict(valid_data[["full_train"]])
  input_data <- rbind(valid_data[["full_labels"]])
  saveRDS(output_data, file = paste0("output.Rds"))
  saveRDS(input_data, file = paste0("input.Rds"))

  setwd("..")

  return(model)
}
