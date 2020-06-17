#' Load_Data
#'
#' @param obj - model object
#' @param input_files
#' @param NAMEPLATE
#' @param nmp
#'


Load_Data = function(obj, input_files = list(input_file_ModelData = NULL,
                                             input_file_FMIData  = NULL,
                                             input_file_SpendData = NULL,
                                             input_file_MSRPData = NULL),
                     NAMEPLATE = NULL,
                     nmp = NULL){

  if(!is.null(input_files$input_file_ModelData)){
    message("Loading ModelData")
    obj = Load_ModelData(obj, input_files$input_file_ModelData)
  }else{
    warning("No ModelData file path. Data not loaded")
  }

  if(!is.null(input_files$input_file_FMIData)){
    message("Loading FMIData")
    obj = Load_FMIData(obj, input_files$input_file_FMIData)
  }else{
    warning("No FMIData file path. Data not loaded")
  }

  if(!is.null(input_files$input_file_SpendData)){
    message("Loading SpendData")
    obj = Load_SpendData(obj, input_files$input_file_SpendData, NAMEPLATE, nmp)
  }else{
    warning("No SpendData file path. Data not loaded")
  }

  if(!is.null(input_files$input_file_MSRPData)){
    message("Loading MSRPData")
    obj = Load_MSRPData(obj, input_files$input_file_MSRPData, NAMEPLATE)
  }else{
    warning("No MSRPData file path. Data not loaded")
  }

  return(obj)
}
