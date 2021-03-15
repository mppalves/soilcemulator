#' @title select_features
#' @description Tool to select the relevant features used for training from the datasets
#' @return list containing `input_shape` and boolean vector of selected columns
#' @param pprosdf  pre processed dataframe
#' @author Marcos Alves
#' @export

select_features <- function(pprosdf) {
  remove_cols <- c("cell", "region", "year", "timestep")
  if (is.data.frame(pprosdf)) {
    features <- paste0(remove_cols, collapse = "+|")
    select <- grepl(pattern = features, names(pprosdf), ignore.case = TRUE)
    return(list("input_shape" = (sum(!select) - 1), "select" = !select))
  } else {
    stop("pprosdf is not a dataframe")
  }
}
