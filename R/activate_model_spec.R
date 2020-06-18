#' Activate Model Specification
#'
#' This function onloads the model specification file and includes only modeled variables
#'
#' @param mod_obj - model object
#' @param input_file_ModelSpec - file path to ModelSpec.csv
#'
#' @return mod_obj
#'

activate_model_spec <- function(mod_obj, input_file_ModelSpec) {
  mod_obj$spec <- read_csv(input_file_ModelSpec, col_types = cols()) %>% filter(Include == 1)
  return(mod_obj)
}
