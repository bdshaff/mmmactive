#' Activate Model Specification
#'
#' @param mod_obj - model object
#' @param input_file_ModelSpec - file path
#'

activate_model_spec = function(mod_obj, input_file_ModelSpec){
  mod_obj$spec = read_csv(input_file_ModelSpec, col_types = cols()) %>% filter(Include == 1)
  return(mod_obj)
}
