#' @title run_batch_rmse
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @description run_analysis for cross valiation batches
#' @param modelid  model id output from \link{write_run_info}
#' @param targetdir output directory
#' @param pprosdf pre processed dataset
#' @param trained_model trained keras model
#' @param features columns selected for training
#' @export

run_batch_rmse <- function(pprosdf, trained_model, features, targetdir, modelid) {
  features <- features[["select"]]
  output_dir <- grep(modelid, list.dirs(targetdir, recursive = F), value = T)
  runs <- list.dirs(output_dir, recursive = F, full.names = F)
  setwd(output_dir)

  RMSE <- function(m, o) {
    return(sqrt(mean((m - o)^2)))
  }

  results <- data.frame(runs, "RMSE" = 0)

  for (run in runs) {
    set.seed(123)
    # dfid <- attr(pprosdf, "dfid")
    selected <- pprosdf$timestep == as.numeric(run)
    datadf <- pprosdf[, features]
    setwd(run)
    col_means <- readRDS(grep("means", list.files(), value = T))
    col_stddevs <- readRDS(grep("stddevs", list.files(), value = T))
    datadf <- scale(datadf, center = col_means, scale = col_stddevs)
    datadf <- datadf[selected, ]
    output <- grepl(pattern = "soil+", colnames(datadf))
    test_data <- as.matrix(datadf[, !output])
    test_labels <- as.matrix(datadf[, output] + 1)
    test_predictions <- trained_model %>% predict(test_data)
    results[results[, 1] == run, "RMSE"] <- RMSE(test_labels, test_predictions)
    setwd(output_dir)
  }
  write.csv(results, file = "rmse_results.csv")
  return(results)
}
