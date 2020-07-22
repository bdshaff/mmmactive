#' Test transform_ready mod_obj
#'
#' @param x - mod_obj
#'
#' @export

is.transform_ready <- function(x){
  nms <- c("data_input", "fit_curves")
  test <- all(nms %in% attributes(x)$names)
  if(!test){
    warning(paste(nms[!(nms %in% attributes(x)$names)],": missing element to launch Transform\n"), immediate. = TRUE)
  }
  return(test)
}
