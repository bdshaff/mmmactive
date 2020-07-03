#' Test load_data_ready mod_obj
#'

is.load_data_ready <- function(mod_obj){
  if(!is.activated(mod_obj)){
    stop("mod_obj must be activated to on-load data.")
  }
  test = "data_group_selector" %in% attributes(mod_obj)$names
  if(!test){
    warning("must add group selector.")
  }
  return(test)
}
