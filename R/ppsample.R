#' @title ppsample
#' @description Processes the pre processed data set by selecting features, randomizing, scaling and slicing.
#' @return Sample training in matrix format
#' @param pprosdf pre processed dataset
#' @param skip_timestep time step to be skipped in the sample (cross validation)
#' @param p train/test data split
#' @param features Boolean vector list containing the selected features
#' @author Marcos Alves
#' @import magclass
#' @import utils
#' @export

ppsample <- function(pprosdf, skip_timestep = NULL, p = 0.9, features) {
  features <- features[["select"]]
  dfid <- attr(pprosdf, "dfid")
  skip <- pprosdf$timestep != skip_timestep

  datadf <- pprosdf[, features]

  datadf <- scale(datadf)
  col_means <- attr(datadf, "scaled:center")
  col_stddevs <- attr(datadf, "scaled:scale")
  datadf <- datadf[skip, ]

  # randomize data in df
  nr <- dim(datadf)[1]
  datadf <- datadf[sample.int(nr), ]

  # divide data in training and testing
  output <- grepl(pattern = "soil+", colnames(datadf))
  smp_size <- floor(p * nrow(datadf))
  train_ind <- sample(seq_len(nrow(datadf)), size = smp_size)

  train_data <- datadf[train_ind, !output]
  train_labels <- datadf[train_ind, output]
  test_data <- datadf[-train_ind, !output]
  test_labels <- datadf[-train_ind, output]

  # converting data frames into matrix
  train_data <- as.matrix(train_data)
  train_labels <- as.matrix(train_labels)
  test_data <- as.matrix(test_data)
  test_labels <- as.matrix(test_labels)

  inputs <- colnames(train_data)

  dir.create(as.character(skip_timestep), showWarnings = T)
  setwd(as.character(skip_timestep))

  saveRDS(col_means, file = paste0("means_", dfid, ".Rds"))
  saveRDS(col_stddevs, file = paste0("stddevs_", dfid, ".Rds"))
  saveRDS(inputs, file = paste0("inputs_", dfid, ".Rds"))

  # writting model information
  x <- list("dfid" = dfid, "train_data" = head(train_data), "inputs" = inputs, "skip_timestep" = skip_timestep)
  y <- capture.output(x)
  con <- file("timestep_info.txt")
  writeLines(y, con = con)
  close(con)

  x <- list(
    "train_data" = train_data,
    "train_labels" = train_labels,
    "test_data" = test_data,
    "test_labels" = test_labels,
    "inputs" = inputs,
    "col_means" = col_means,
    "col_stddev" = col_stddevs
  )

  attr(x, "timestep") <- skip_timestep
  attr(x, "dfid") <- dfid

  return(x)
}
