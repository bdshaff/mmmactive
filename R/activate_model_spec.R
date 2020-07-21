#' Activate Model Specification
#'
#' This function onloads the model specification file and includes only modeled variables
#'
#' @param mod_obj - model object
#' @param input_file_ModelSpec - file path to ModelSpec.csv
#'
#' @return mod_obj
#'
#' @importFrom magrittr %>%
#' @export

activate_model_spec <- function(mod_obj, input_file_ModelSpec) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }

  mod_obj$spec <- readr::read_csv(input_file_ModelSpec, col_types = readr::cols()) %>% dplyr::filter(Include == 1)
  return(mod_obj)
}
