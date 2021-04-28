#' Create Model Setup
#'
#' This function creates a model setup table data.frame
#'
#' @return model_setup_table
#'
#' @export

create_model_setup = function(ModelForm = "lin_lin", Panel = "Y", Crossection = NULL,
                                Time = "month", BeginDate = NULL, EndDate = NULL,
                                kpi = NULL, nmp = NULL, rgn = NULL){

  Parameter = c("ModelForm", "Panel", "Crossection", "Time",
                "BeginDate", "EndDate", "kpi", "nmp","rgn")

  Value = c(ModelForm, Panel, Crossection, Time,
            BeginDate, EndDate, kpi, nmp, rgn)

  model_setup_table = data.frame(Parameter, Value)

  return(model_setup_table)

}
