#' Activate Model Setup
#'
#' This function dispatches a number of model metadata onto the mod_obj.
#'
#' @param mod_obj - model object
#' @param input_file_ModelSetup - file path to ModelSetup.csv
#'
#' @return mod_obj
#'

activate_model_setup <- function(mod_obj, input_file_ModelSetup) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }

  Model_setup <- read_csv(input_file_ModelSetup, col_types = cols())
  mod_obj$ModelForm <- Model_setup$Value[Model_setup$Parameter == "ModelForm"]
  mod_obj$Panel <- Model_setup$Value[Model_setup$Parameter == "Panel"]
  mod_obj$Time <- Model_setup$Value[Model_setup$Parameter == "Time"]
  mod_obj$BeginDate <- mdy(Model_setup$Value[Model_setup$Parameter == "BeginDate"])
  mod_obj$EndDate <- mdy(Model_setup$Value[Model_setup$Parameter == "EndDate"])
  mod_obj$SimStart <- mdy(Model_setup$Value[Model_setup$Parameter == "SimStart"])
  mod_obj$SimEnd <- mdy(Model_setup$Value[Model_setup$Parameter == "SimEnd"])
  mod_obj$mroi_step <- as.numeric(Model_setup$Value[Model_setup$Parameter == "Mroi"])
  mod_obj$cs <- Model_setup$Value[Model_setup$Parameter == "Crossection"]
  mod_obj$kpi <- Model_setup$Value[Model_setup$Parameter == "kpi"]
  mod_obj$NMP <- Model_setup$Value[Model_setup$Parameter == "NMP"]
  mod_obj$nmp <- Model_setup$Value[Model_setup$Parameter == "nmp"]
  mod_obj$NAMEPLATE <- Model_setup$Value[Model_setup$Parameter == "NAMEPLATE"]
  return(mod_obj)
}
