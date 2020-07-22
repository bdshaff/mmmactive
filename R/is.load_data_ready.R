#' Test load_data_ready mod_obj
#'
#' @param x - mod_obj
#'
#' @export

is.load_data_ready <- function(x){
  if(!is.activated(x)){
    warning("mod_obj must be activated to on-load data.\n")
  }
  test = "data_group_selector" %in% attributes(x)$names
  if(!test){
    warning("must add group selector.\n")
  }
  return(test)
}
