#' Test transform_ready mod_obj
#'
#' @export

is.transform_ready <- function(mod_obj){
  nms <- c("data_input", "fit_curves")
  test <- all(nms %in% attributes(mod_obj)$names)
  if(!test){
    warning(paste(nms[!(nms %in% attributes(mod_obj)$names)],": missing element to launch Transform\n"), immediate. = TRUE)
  }
  return(test)
}
