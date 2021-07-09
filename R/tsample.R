#' @title tsample
#' @description Pre processes validation dataset
#' @return matrix
#' @param pprosdf input file
#' @param features output directory
#' @param plot_test Whether the output should reduced to cluster cells for plotting
#' @param tag flag
#' @param means_col means
#' @param std_col standard deviation
#' @param mean_lab means
#' @param std_lab standard deviation
#' @author Marcos Alves
#' @import magclass
#' @import utils
#' @import dplyr
#' @export

tsample <- function(pprosdf, features, plot_test = F, tag, means_col, std_col, mean_lab, std_lab) {

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


  # divide data in features and labes
  output <- grepl(pattern = paste0(tag, "+"), colnames(datadf))
  full_features <- datadf[, !output]
  full_labels <- datadf[, output]
  full_features <- scale(full_features, center = means_col, scale = std_col)
  full_labels <- scale(full_labels, center = mean_lab, scale = std_lab)

  # converting data frames into matrix
  full_features <- as.matrix(full_features)
  full_labels <- as.matrix(full_labels)


  x <- list(
    "full_features" = full_features,
    "full_labels" = full_labels,
    "cell" = cells
  )

  return(x)
}
