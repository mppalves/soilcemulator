select_features <- function(pprosdf,sel_feat){
  if(is.data.frame(pprosdf)){
    features <- paste0(sel_feat, collapse ="+|")
    select <- grepl(pattern = features, names(pprosdf), ignore.case=TRUE)
    return(list("input_shape"=(sum(select)-1), "select"=select))
  }else{
    stop("pprosdf is not a dataframee")
  }
}
