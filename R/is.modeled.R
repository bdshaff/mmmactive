#' Test modeled mod_obj
#'
#' @param x - mod_obj
#'
#' @export

is.modeled <- function(x){
  nms <- c("Model")
  test <- all(nms %in% attributes(x)$names)
  if(!test){
    warning(paste(nms[!(nms %in% attributes(x)$names)],": missing element \n"), immediate. = TRUE)
  }
  return(test)
}
