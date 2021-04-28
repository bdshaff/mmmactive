#' Create Model Spect
#'
#' This function creates a model spec template data.frame
#'
#' @return model_spec_table
#'
#' @export

create_model_spec = function(mod_obj){

  Orig_Variable = c("Intercept",names(mod_obj$data_input$monthly)[-c(1,2,3)], names(mod_obj$data_input$weekly)[-c(1,2,3)])
  Trans_Variable = str_c(Orig_Variable,"_trans")
  Variable_Type = c("Trend", rep("Trend",length(names(mod_obj$data_input$monthly)[-c(1,2,3)])),
                    rep("Marketing", length(names(mod_obj$data_input$weekly)[-c(1,2,3)])))

  model_spec_table =
    data.frame(Orig_Variable, Trans_Variable, AggregateVariable = "NA", Variable_Type,
               Include = 0, VaryBy = "None", Transform = "Y", TransformType = "None",
               Lag = 0, Scale = 1, Effective = 2, Recency = 1, Decay = 10, Period = 7,  Alpha = 0, Window = 0,
               Prior_Mean = 1, Prior_SD = 1, PriorSD_Adj = 1, Sign = 1, Override = "N")

  kpi_row = which(model_spec_table$Orig_Variable == mod_obj$kpi)

  model_spec_table[kpi_row, "Variable_Type"] = "Dependent"
  model_spec_table[kpi_row, "AggregateVariable"] = "KPI"
  model_spec_table[kpi_row, "VaryBy"] = mod_obj$cs
  model_spec_table[kpi_row, "Include"] = 1

  model_spec_table[1, "AggregateVariable"] = "Base"
  model_spec_table[1, "Include"] = 1
  model_spec_table[1, "Trans_Variable"] = "Intercept"

  return(model_spec_table)
}
