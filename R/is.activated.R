#' Test activated mod_obj
#'
#' @export

is.activated <- function(x) {
  if (!is.mod_obj(x)) {
    stop("Input must be of class mod_obj.")
  }
  nms <- c("ModelForm", "Panel", "Time", "BeginDate", "EndDate", "SimStart", "SimEnd", "mroi_step", "cs", "kpi", "spec")
  test <- sum(nms %in% attributes(mod_obj)$names) == 11
  if(!test){
    warning(paste(nms[!(nms %in% attributes(mod_obj)$names)],": missing element to activate mod_obj\n"), immediate. = TRUE)
  }
  return(test)
}
