#' Load_FMIData
#'
#' @param obj - model object
#' @param input_file_MSRPData
#' @param NAMEPLATE
#'

Load_MSRPData <- function(obj, input_file_MSRPData, NAMEPLATE = NULL){
  Mtable = read_csv(input_file_MSRPData) %>%
    filter(Model == NAMEPLATE)

  obj$Mtable = Mtable
  return(obj)
}
