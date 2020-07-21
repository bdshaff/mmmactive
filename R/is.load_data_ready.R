#' Test load_data_ready mod_obj
#'
#' @param x - mod_obj
#'
#' @export

is.load_data_ready <- function(mod_obj){
  if(!is.activated(mod_obj)){
    warning("mod_obj must be activated to on-load data.\n")
  }
  test = "data_group_selector" %in% attributes(mod_obj)$names
  if(!test){
    warning("must add group selector.\n")
  }
  return(test)
}
