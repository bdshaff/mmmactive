#' Activate Model Specification
#'
#' This function onloads the model specification file and includes only modeled variables
#'
#' @param mod_obj - model object
#' @param input_file_ModelSpec - file path to ModelSpec.csv
#' @param model_spec_table - model_spec_table
#'
#' @return mod_obj
#'
#' @importFrom magrittr %>%
#' @export

activate_model_spec = function(mod_obj, input_file_ModelSpec = NULL, model_spec_table = NULL){

  if(!is.null(input_file_ModelSpec)){
    mod_obj$spec <- readr::read_csv(input_file_ModelSpec, col_types = readr::cols()) %>% dplyr::filter(Include == 1)
  }else if(!is.null(model_spec_table)){
    mod_obj$spec <- model_spec_table %>% dplyr::filter(Include == 1)
  }

  return(mod_obj)
}
