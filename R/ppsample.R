#' @title pprosdf
#' @description Pre processes the machine learning emulator based on Mapie input files
#' @return Data frame
#' @param input input file
#' @param targetdir output directory
#' @param repositories input repository
#' @param flag file flag
#' @author Marcos Alves
#' @import magclass
#' @importFrom gms download_unpack
#' @importFrom stringi stri_split_fixed
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate
#' @importFrom digest sha1
#'
rawdf <- out
ppsample <- function(rawdf, skip_timestep=NULL, p=0.9){

  dfid <- attr(rawdf, "id")
  skip <- rawdf$timestep!=skip_timestep
  drop_names <- "cell+|fak+|reg+|year+|cond+|tdiff+|data+|timestep"
  drops <- grepl(pattern = drop_names, names(rawdf), ignore.case=TRUE)
  datadf <- rawdf[, !drops]

  datadf  <- scale(datadf)
  col_means   <- attr(datadf, "scaled:center")
  col_stddevs <- attr(datadf, "scaled:scale")
  datadf <- datadf[skip,]

  # randomize data in df
  nr <- dim(datadf)[1]
  datadf <- datadf[sample.int(nr), ]

  # divide data in training and testing
  output       <- grepl(pattern = "soil+", colnames(datadf))
  smp_size     <- floor(p * nrow(datadf))
  train_ind    <- sample(seq_len(nrow(datadf)), size = smp_size)

  train_data   <- datadf[train_ind,!output]
  train_labels <- datadf[train_ind,output]
  test_data    <- datadf[-train_ind,!output]
  test_labels  <- datadf[-train_ind,output]

  #converting data frames into matrix
  train_data   <- as.matrix(train_data)
  train_labels <- as.matrix(train_labels)
  test_data    <- as.matrix(test_data)
  test_labels  <- as.matrix(test_labels)

  inputs       <- colnames(train_data)

  saveRDS(col_means, file = paste0("means_", dfid, ".Rds"))
  saveRDS(col_stddevs, file = paste0("stddevs_", dfid, ".Rds"))
  saveRDS(inputs, file = paste0("inputs_", dfid, ".Rds"))


  return(list("train_data"=train_data,
              "train_labels"=train_labels,
              "test_data" = test_data,
              "test_labels"=test_labels,
              "inputs" = inputs,
              "col_means" = col_means,
              "col_stddev" = col_stddevs))

}
