#' Test data_transformed mod_obj
#'
#' @param x - mod_obj
#'
#' @export

is.data_transformed <- function(x){
  nms <- c("data_transformed", "data")
  test <- all(nms %in% attributes(x)$names)
  if(!test){
    warning(paste(nms[!(nms %in% attributes(x)$names)],": missing element \n"), immediate. = TRUE)
  }
  return(test)
}
