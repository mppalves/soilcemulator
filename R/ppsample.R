#' @title ppsample
#' @description Processes the pre processed data set by selecting features, randomizing, scaling and slicing.
#' @return Sample training in matrix format
#' @param pprosdf pre processed dataset
#' @param skip_timestep time step to be skipped in the sample (cross validation)
#' @param p train/test data split
#' @param tag tag param
#' @param features Boolean vector list containing the selected features
#' @param scale should values be scaled or not
#' @param offset offset value for 0 centered scale outputs
#' @author Marcos Alves
#' @import magclass
#' @import utils
#' @export

ppsample <- function(pprosdf, skip_timestep = NULL, p = 0.9, features = select_features(pprosdf), offset = 0, tag, scale = T) {
  set.seed(123)
  features <- features[["select"]]
  dfid <- attr(pprosdf, "dfid")
  skip <- pprosdf$timestep != skip_timestep

  datadf <- pprosdf[, features]
  datadf <- datadf[skip, ]

  # randomize data in df
  nr <- dim(datadf)[1]
  datadf <- datadf[sample.int(nr), ]

  # divide data in training and testing
  output <- grepl(pattern = paste0(tag,"+"), colnames(datadf))
  smp_size <- floor(p * nrow(datadf))
  train_ind <- sample(seq_len(nrow(datadf)), size = smp_size)

  train_data <- datadf[train_ind, !output]
  train_labels <- datadf[train_ind, output] + offset
  test_data <- datadf[-train_ind, !output]
  test_labels <- datadf[-train_ind, output] + offset

  # converting data frames into matrix
  train_data <- as.matrix(train_data)
  train_labels <- as.matrix(train_labels)
  test_data <- as.matrix(test_data)
  test_labels <- as.matrix(test_labels)

  if(scale){
    train_data  <- scale(train_data)
    train_labels  <- scale(train_labels)
    col_means   <- attr(train_data, "scaled:center")
    col_stddevs <- attr(train_data, "scaled:scale")
    label_means   <- attr(train_labels, "scaled:center")
    label_stddevs <- attr(train_labels, "scaled:scale")
    test_data   <- scale(test_data, center = col_means, scale = col_stddevs)
    test_labels   <- scale(test_labels, center = label_means, scale = label_stddevs)
  }

  inputs <- colnames(train_data)

  dir.create(as.character(skip_timestep), showWarnings = T)
  setwd(as.character(skip_timestep))
  if(scale){
  saveRDS(col_means, file = paste0("mean_col_", dfid, ".rds"))
  saveRDS(col_stddevs, file = paste0("stddevs_col_", dfid, ".rds"))
  saveRDS(label_means, file = paste0("mean_lab_", dfid, ".rds"))
  saveRDS(label_stddevs, file = paste0("stddevs_lab_", dfid, ".rds"))
  } else {
    col_means <- NULL
    col_stddevs <- NULL
    label_means <- NULL
    label_stddevs <- NULL
  }
  saveRDS(inputs, file = paste0("inputs_", dfid, ".rds"))

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
    "col_stddevs" = col_stddevs,
    "label_means" = label_means,
    "label_stddevs" = label_stddevs
  )

  attr(x, "timestep") <- skip_timestep
  attr(x, "dfid") <- dfid

  return(x)
}


