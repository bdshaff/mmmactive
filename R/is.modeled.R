#' Test modeled mod_obj
#'
#' @export

is.modeled <- function(mod_obj){
  nms <- c("Model", "lmModel")
  test <- all(nms %in% attributes(mod_obj)$names)
  if(!test){
    warning(paste(nms[!(nms %in% attributes(mod_obj)$names)],": missing element \n"), immediate. = TRUE)
  }
  return(test)
}
