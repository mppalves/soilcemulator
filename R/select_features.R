#' @title select_features
#' @description Tool to select the relevant features used for training from the datasets
#' @return list containing `input_shape` and boolean vector of selected columns
#' @param pprosdf  pre processed dataframe
#' @param sel_feat character vector with column id characters
#' @author Marcos Alves
#' @export

select_features <- function(pprosdf, sel_feat) {
  if (is.data.frame(pprosdf)) {
    features <- paste0(sel_feat, collapse = "+|")
    select <- grepl(pattern = features, names(pprosdf), ignore.case = TRUE)
    return(list("input_shape" = (sum(select) - 1), "select" = select))
  } else {
    stop("pprosdf is not a dataframee")
  }
}
