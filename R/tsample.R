#' @title tsample
#' @description Pre processes validation dataset
#' @return matrix
#' @param pprosdf input file
#' @param features output directory
#' @param plot_test Whether the output should reduced to cluster cells for plotting
#' @author Marcos Alves
#' @import magclass
#' @import utils
#' @import dplyr
#' @export

tsample <- function(pprosdf, features, plot_test = F) {

  # Initiatin magrittr variables
  Region <- NULL
  Cell <- NULL

  features <- features[["select"]]
  dfid <- attr(pprosdf, "dfid")
  if (plot_test) {
    datadf <- pprosdf[which(pprosdf[, "Year"] == 2000 & pprosdf[, "lsu_ha"] == 0), features]
    cells <- mutate(pprosdf[which(pprosdf[, "Year"] == 2000 & pprosdf[, "lsu_ha"] == 0), ], cells = paste0(Region, "_", Cell))[, "cells"]
  } else {
    datadf <- pprosdf[, features]
    cells <- mutate(pprosdf[which(pprosdf[, "Year"] == 2000 & pprosdf[, "lsu_ha"] == 0), ], cells = paste0(Region, "_", Cell))[, "cells"]
  }
  #datadf <- scale(datadf)

  # divide data in training and testing
  output <- grepl(pattern = "soil+", colnames(datadf))
  full_train <- datadf[, !output]
  full_labels <- datadf[, output]


  # converting data frames into matrix
  full_train <- as.matrix(full_train)
  full_labels <- as.matrix(full_labels)


  x <- list(
    "full_train" = full_train,
    "full_labels" = full_labels,
    "cell" = cells
  )

  return(x)
}
