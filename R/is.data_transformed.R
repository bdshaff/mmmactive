#' Test data_transformed mod_obj
#'
#' @export

is.data_transformed <- function(mod_obj){
  nms <- c("data_transformed", "data")
  test <- all(nms %in% attributes(mod_obj)$names)
  if(!test){
    warning(paste(nms[!(nms %in% attributes(mod_obj)$names)],": missing element \n"), immediate. = TRUE)
  }
  return(test)
}
