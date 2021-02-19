#' @title pprosdf
#' @description Pre processes the machine learning emulator based on Mapie input files
#' @return Data frame
#' @author Marcos Alves
#' @import magclass
#' @import gms download_unpack
#' @importFrom stringi stri_split_fixed
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate
#'

# library(magclass)
# library(gms)
# library(stringi)
# library(tidyr)
# library(dplyr)

pprosdf <- function(input, targetdir, repositories, flag) {
  # input = "rev4.51+mrmagpie_past2_h12_1d3efffd6e793f8aa6f3bf4219bd8ea3_cellularmagpie_debug.tgz"
  # targetdir = "C:/Users/pedrosa/Desktop/test/"
  # repositories = list("C:/Users/pedrosa/Desktop"= NULL)
  # flag = "soilc_lab"

  filemap <- download_unpack(input, targetdir = targetdir, repositories = repositories, unpack = TRUE)

  if(dir.exists(targetdir)){
    setwd(targetdir)
  } else {
    stop(paste("Output directory was not created sucessfully",targetdir))
  }

  files <- list.files(targetdir)
  tag_index <- grep(flag, files)
  unlink(files[-tag_index])

  if(length(tag_files)>2){
    stop(c("More than 2 tagged files. Cannot handle this case. ", print(tag_files)))
  }

  features     <- read.magpie(grep("Envi",files[tag_index], value = T))
  labels       <- read.magpie(grep("pstock",files[tag_index], value = T))
  gcm          <- unlist(stri_split_fixed(getNames(features)[1], "_", n=2))[2]
  features_exp <- add_columns(features, addnm = paste0(flag,"_",gcm))
  features_exp <- add_dimension(features_exp, dim = 3.1, add = "lsu_density", nm = getNames(labels))

  features_exp[,,paste0("soilc_",gcm)] <- labels
  out <- as.data.frame(features_exp)
  out <- pivot_wider(out, id_cols = c(Cell, Region, Year, Data1), names_from = Data2, values_from = Value)
  out <- mutate(out, Data1 = as.numeric(gsub("p",".",Data1)))

  colnames(out)[grep("Data1", colnames(out))] <- "lsu_ha"
  if(!all(sapply(out[,4:ncol(out)],is.numeric))){
    warning("Check the output dataframe for non-numeric features")
  }
  return(out)
}

