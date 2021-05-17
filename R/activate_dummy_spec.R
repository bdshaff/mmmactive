#' Activate Dummy Specification
#'
#' This function onloads the dummy variable specification
#'
#' @param mod_obj - model object
#' @param input_file_ModelSpec - file path to ModelSpec.csv
#' @param model_spec_table - model_spec_table
#'
#' @return mod_obj
#'
#' @importFrom magrittr %>%
#' @export

activate_dummy_spec = function(mod_obj, input_file_DummySpec = NULL, dummy_spec_table = NULL){
#hey hey
  if(!is.null(input_file_DummySpec)){
    dummy_spec = read_csv(input_file_DummySpec)
    mod_obj$dummy_spec = dummy_spec
  }else if(!is.null(dummy_spec_table)){
    mod_obj$dummy_spec = dummy_spec_table
  }
  return(mod_obj)
}
