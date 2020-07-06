#' Load_Data
#'
#' This function loads data onto a mod_obj for the paths supplied with the `input_files` vector of paths.
#' Internally the Load_ModelData(), Load_FMIData(), Load_MSRPData(), and Load_SpendData() functions are called.
#'
#' @param mod_obj - model mod_object
#' @param input_files - paths to input files
#'
#' @return mod_obj


Load_Data <- function(mod_obj, input_files = list(
                        input_file_ModelData = NULL,
                        input_file_FMIData = NULL,
                        input_file_SpendData = NULL,
                        input_file_MSRPData = NULL
                      )) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.load_data_ready(mod_obj)) {
    stop("mod_obj missing group selector needed to load data.")
  }

  if (!is.null(input_files$input_file_ModelData)) {
    message("Loading ModelData")
    mod_obj <- Load_ModelData(mod_obj, input_files$input_file_ModelData)
  } else {
    warning("No ModelData file path. Data not loaded")
  }

  if (!is.null(input_files$input_file_FMIData)) {
    message("Loading FMIData")
    mod_obj <- Load_FMIData(mod_obj, input_files$input_file_FMIData)
  } else {
    warning("No FMIData file path. Data not loaded")
  }

  if (!is.null(input_files$input_file_SpendData)) {
    message("Loading SpendData")
    mod_obj <- Load_SpendData(mod_obj, input_files$input_file_SpendData)
  } else {
    warning("No SpendData file path. Data not loaded")
  }

  if (!is.null(input_files$input_file_MSRPData)) {
    message("Loading MSRPData")
    mod_obj <- Load_MSRPData(mod_obj, input_files$input_file_MSRPData)
  } else {
    warning("No MSRPData file path. Data not loaded")
  }

  return(mod_obj)
}
