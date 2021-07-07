#' @title pprosdf
#' @description Pre processes the machine learning emulator based on Mapie input files
#' @return Data frame
#' @param input input file
#' @param targetdir output directory
#' @param repositories input repository
#' @param flag file flag
#' @param cut_100 limit training data to "y2010"
#' @param t_size time step size used for cross valiadation
#' @param download should data be downloaded
#' @author Marcos Alves
#' @import magclass
#' @importFrom gms download_unpack
#' @importFrom stringi stri_split_fixed
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate
#' @importFrom digest sha1
#' @export

# library(magclass)
# library(gms)
# library(stringi)
# library(tidyr)
# library(dplyr)
# library(digest)

pprosdf <- function(input, targetdir, repositories, flag, cut_100 = T, t_size = 10, download = T) {
  # input = "rev4.51+mrmagpie_past2_h12_1d3efffd6e793f8aa6f3bf4219bd8ea3_cellularmagpie_debug.tgz"
  # targetdir = "C:/Users/pedrosa/Desktop/test/"
  # repositories = list("C:/Users/pedrosa/Desktop"= NULL)
  # flag = "soilc_lab"

  Cell <- NULL
  Region <- NULL
  Year <- NULL
  Data1 <- NULL
  Data2 <- NULL
  Value <- NULL
  tag_files <- NULL

  if(download) {filemap <- download_unpack(input, targetdir = targetdir, repositories = repositories, unpack = TRUE)}

  if (dir.exists(targetdir)) {
    setwd(targetdir)
  } else {
    dir.create(targetdir)
    setwd(targetdir)
     } else if (dir.exists(targetdir)) {
     stop(paste("Output directory was not created sucessfully", targetdir))
  }

  files <- list.files(targetdir)
  tag_index <- grep(flag, files)
  if (length(tag_index) < 1) {
    stop(paste0("None of the files in ", targetdir, "is flagged as ", flag))
  }
  if(download) { unlink(files[-tag_index]) }

  if (length(tag_files) > 2) {
    stop(c("More than 2 tagged files. Cannot handle this case. ", print(tag_files)))
  }

  features <- read.magpie(grep("Envi", files[tag_index], value = T, ignore.case = T))
  labels <- read.magpie(grep("stock", files[tag_index], value = T, ignore.case = T))

  # #############
  # ###Scaling###
  # #############
  # #labels
  # lmin <- min(labels)
  # lmax <- max(labels)
  # labels  <- (labels - lmin)/(lmax - lmin)
  # #features
  # fmin <- apply(features, 3, min)
  # fmax <- apply(features, 3, max)
  # features    <- (features - as.magpie(fmin))/(as.magpie(fmax) - as.magpie(fmin))

  gcm <- unlist(stri_split_fixed(getNames(features)[1], "_", n = 2))[2]
  features_exp <- add_columns(features, addnm = paste0(flag, "_", gcm))
  features_exp <- add_dimension(features_exp, dim = 3.1, add = "lsu_density", nm = getNames(labels))
  years <- intersect(getYears(features), getYears(labels))
  features_exp[, years, paste0(flag, "_", gcm)] <- labels[, years, ]
  out <- as.data.frame(features_exp)
  out <- pivot_wider(out, id_cols = c(Cell, Region, Year, Data1), names_from = Data2, values_from = Value)
  out <- mutate(out, Data1 = as.numeric(gsub("p", ".", Data1)), Year = as.numeric(as.character(Year)))

  colnames(out)[grep("Data1", colnames(out))] <- "lsu_ha"
  if (!all(sapply(out[, 4:ncol(out)], is.numeric))) {
    warning("Check the output dataframe for non-numeric features")
  }
  years <- as.data.frame(out[, "Year"])
  timestep <- unlist(years - years %% t_size)
  out$timestep <- timestep

  if (cut_100 == T) {
    out <- out[out$Year <= 2100, ]
  }

  dfid <- digest::digest(out, "xxhash32")
  attr(out, "dfid") <- dfid
  # attr(out, "min") <- lmin
  # attr(out, "max") <- lmax
  saveRDS(out, file = paste0("training_data_", dfid, ".rds"))
  return(out)
}
