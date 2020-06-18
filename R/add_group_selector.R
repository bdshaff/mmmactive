#' Add Group Selector
#'
#' This function vehicle and region values to be included.
#' The inclusion in necessary for data loading and transformation.
#'
#' @param mod_obj - model object
#' @param vehicles - character vector
#' @param regions  - character vectors
#'
#' @return mod_obj
#'

add_group_selector <- function(mod_obj, vehicles, regions) {
  mod_obj$data_group_selector <-
    list(
      vehicle = vehicles,
      region = regions
    )
  return(mod_obj)
}
