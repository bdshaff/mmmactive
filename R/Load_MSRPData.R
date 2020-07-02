#' Load_MSRPData
#'
#' This function loads the MSRP data onto the mod_obj
#'
#' @param mod_obj - model mod_object
#' @param input_file_MSRPData - path to MSRP data file.
#' @param NAMEPLATE - Nameplate (ex. Titan)
#'
#' @return mod_obj
#'

Load_MSRPData <- function(mod_obj, input_file_MSRPData, NAMEPLATE = NULL) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }

  Mtable <- read_csv(input_file_MSRPData) %>%
    filter(Model == NAMEPLATE)

  mod_obj$Mtable <- Mtable
  return(mod_obj)
}
