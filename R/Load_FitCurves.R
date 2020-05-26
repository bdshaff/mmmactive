#' Load_FitCurves
#'
#' @param obj - model object
#' @param input_file_ModelFitCurves - file path
#'


Load_FitCurves = function(obj, input_file_ModelFitCurves){

  datasheets = excel_sheets(input_file_ModelFitCurves)

  obj$fit_curves =
    map(datasheets, ~ clean_names(read_xlsx(input_file_ModelFitCurves, sheet = .x))) %>%
    set_names(str_replace(str_extract(datasheets, "_[a-zA-Z0-9.-]*$"), "_", ""))

  return(obj)

}
