#' Load_MSRPData
#'
#' This function loads the MSRP data onto the mod_obj
#'
#' @param mod_obj - model mod_object
#' @param input_file_MSRPData - path to MSRP data file.
#'
#' @return mod_obj
#'

Load_MSRPData <- function(mod_obj, input_file_MSRPData) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.load_data_ready(mod_obj)) {
    stop("mod_obj missing group selector needed to load data.")
  }

  NAMEPLATE <- mod_obj$NAMEPLATE

  Mtable <- readr::read_csv(input_file_MSRPData) %>%
    dplyr::filter(Model == NAMEPLATE)

  mod_obj$Mtable <- Mtable
  return(mod_obj)
}
