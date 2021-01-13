#' Test activated mod_obj
#'
#' @param x - mod_obj
#'
#' @export

is.activated <- function(x) {
  if (!is.mod_obj(x)) {
    stop("Input must be of class mod_obj.")
  }
  nms <- c("ModelForm", "Panel", "Time", "BeginDate", "EndDate", "cs", "kpi", "spec")
  test <- sum(nms %in% attributes(x)$names) == 8
  if(!test){
    warning(paste(nms[!(nms %in% attributes(x)$names)],": missing element to activate mod_obj\n"), immediate. = TRUE)
  }
  return(test)
}
