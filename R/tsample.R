#' @title tsample
#' @description Pre processes validation dataset
#' @return matrix
#' @param pprosdf input file
#' @param features output directory
#' @param plot_test Whether the output should reduced to cluster cells for plotting
#' @param tag flag
#' @param means means
#' @param std standard deviation
#' @author Marcos Alves
#' @import magclass
#' @import utils
#' @import dplyr
#' @export

tsample <- function(pprosdf, features, plot_test = F, tag, means, std) {

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
  datadf <- scale(datadf, center = means, scale = std)

  # divide data in features and labes
  output <- grepl(pattern = paste0(tag, "+"), colnames(datadf))
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
