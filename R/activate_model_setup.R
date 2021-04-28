#' Activate Model Setup
#'
#' This function dispatches a number of model metadata onto the mod_obj.
#'
#' @param mod_obj - model object
#' @param input_file_ModelSetup - file path to ModelSetup.csv
#' @param model_setup_table - output from `generate_model_setup()`
#'
#' @return mod_obj
#'
#' @importFrom magrittr %>%
#' @export

activate_model_setup = function(mod_obj, input_file_ModelSetup =  NULL, model_setup_table = NULL){


  if(!is.null(input_file_ModelSetup)){
    Model_setup <- readr::read_csv(input_file_ModelSetup, col_types = readr::cols())
    mod_obj$ModelForm <- Model_setup$Value[Model_setup$Parameter == "ModelForm"]
    mod_obj$Panel <- Model_setup$Value[Model_setup$Parameter == "Panel"]
    mod_obj$Time <- Model_setup$Value[Model_setup$Parameter == "Time"]
    mod_obj$BeginDate <- lubridate::mdy(Model_setup$Value[Model_setup$Parameter == "BeginDate"])
    mod_obj$EndDate <- lubridate::mdy(Model_setup$Value[Model_setup$Parameter == "EndDate"])
    mod_obj$cs <- Model_setup$Value[Model_setup$Parameter == "Crossection"]
    mod_obj$kpi <- Model_setup$Value[Model_setup$Parameter == "kpi"]
    mod_obj$NAMEPLATE <- Model_setup$Value[Model_setup$Parameter == "NAMEPLATE"]
    mod_obj$NMP <- Model_setup$Value[Model_setup$Parameter == "NMP"]
    mod_obj$nmp <- unlist(str_split(Model_setup$Value[Model_setup$Parameter == "nmp"],","))
    mod_obj$rgn <- unlist(str_split(Model_setup$Value[Model_setup$Parameter == "rgn"],","))

    mod_obj$data_group_selector <- list(nameplate = mod_obj$nmp, region = mod_obj$rgn)
  }else if(!is.null(model_setup_table)){
    Model_setup <- model_setup_table
    mod_obj$ModelForm <- Model_setup$Value[Model_setup$Parameter == "ModelForm"]
    mod_obj$Panel <- Model_setup$Value[Model_setup$Parameter == "Panel"]
    mod_obj$Time <- Model_setup$Value[Model_setup$Parameter == "Time"]
    mod_obj$BeginDate <- lubridate::ymd(Model_setup$Value[Model_setup$Parameter == "BeginDate"])
    mod_obj$EndDate <- lubridate::ymd(Model_setup$Value[Model_setup$Parameter == "EndDate"])
    mod_obj$cs <- Model_setup$Value[Model_setup$Parameter == "Crossection"]
    mod_obj$kpi <- Model_setup$Value[Model_setup$Parameter == "kpi"]
    mod_obj$nmp <- unlist(str_split(Model_setup$Value[Model_setup$Parameter == "nmp"],","))
    mod_obj$rgn <- unlist(str_split(Model_setup$Value[Model_setup$Parameter == "rgn"],","))

    mod_obj$data_group_selector <- list(nameplate = mod_obj$nmp, region = mod_obj$rgn)
  }

  return(mod_obj)
}
