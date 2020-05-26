#' Add Group Selector
#'
#' @param mod_obj - model object
#' @param vehicles - cahacter vector
#' @param regions  - character vectors
#'

add_group_selector = function(mod_obj, vehicles, regions){
  mod_obj$data_group_selector =
    list(vehicle = vehicles,
         region = regions)
  return(mod_obj)
}
