#' Update Model Setup
#'
#' This function updates a model spec for a given variable row
#'
#' @return model_setup_table
#'
#' @export


update_model_spec = function(model_spec_table, var,
                             Trans_Variable = NULL,
                             AggregateVariable = NULL,
                             Variable_Type = NULL,
                             Include = NULL,
                             VaryBy = NULL,
                             Transform = NULL,
                             TransformType = NULL,
                             Lag = NULL,
                             Scale = NULL,
                             Effective = NULL,
                             Recency = NULL,
                             Decay = NULL,
                             Period = NULL,
                             Alpha = NULL,
                             Window = NULL,
                             Prior_Mean = NULL,
                             Prior_SD = NULL,
                             PriorSD_Adj = NULL,
                             Sign = NULL,
                             Override = NULL){

  Orig_Variable = model_spec_table$Orig_Variable

  if(!is.null(Trans_Variable)){
    model_spec_table[Orig_Variable == var, "Trans_Variable"] = Trans_Variable
  }
  if(!is.null(AggregateVariable)){
    model_spec_table[Orig_Variable == var, "AggregateVariable"] = AggregateVariable
  }
  if(!is.null(Variable_Type)){
    model_spec_table[Orig_Variable == var, "Variable_Type"] = Variable_Type
  }
  if(!is.null(Include)){
    model_spec_table[Orig_Variable == var, "Include"] = Include
  }
  if(!is.null(VaryBy)){
    model_spec_table[Orig_Variable == var, "VaryBy"] = VaryBy
  }
  if(!is.null(Transform)){
    model_spec_table[Orig_Variable == var, "Transform"] = Transform
  }
  if(!is.null(TransformType)){
    model_spec_table[Orig_Variable == var, "TransformType"] = TransformType
  }
  if(!is.null(Lag)){
    model_spec_table[Orig_Variable == var, "Lag"] = Lag
  }
  if(!is.null(Scale)){
    model_spec_table[Orig_Variable == var, "Scale"] = Scale
  }
  if(!is.null(Effective)){
    model_spec_table[Orig_Variable == var, "Effective"] = Effective
  }
  if(!is.null(Recency)){
    model_spec_table[Orig_Variable == var, "Recency"] = Recency
  }
  if(!is.null(Decay)){
    model_spec_table[Orig_Variable == var, "Decay"] = Decay
  }
  if(!is.null(Period)){
    model_spec_table[Orig_Variable == var, "Period"] = Period
  }
  if(!is.null(Alpha)){
    model_spec_table[Orig_Variable == var, "Alpha"] = Alpha
  }
  if(!is.null(Window)){
    model_spec_table[Orig_Variable == var, "Window"] = Window
  }
  if(!is.null(Prior_Mean)){
    model_spec_table[Orig_Variable == var, "Prior_Mean"] = Prior_Mean
  }
  if(!is.null(Prior_SD)){
    model_spec_table[Orig_Variable == var, "Prior_SD"] = Prior_SD
  }
  if(!is.null(PriorSD_Adj)){
    model_spec_table[Orig_Variable == var, "PriorSD_Adj"] = PriorSD_Adj
  }
  if(!is.null(Sign)){
    model_spec_table[Orig_Variable == var, "Sign"] = Sign
  }
  if(!is.null(Override)){
    model_spec_table[Orig_Variable == var, "Override"] = Override
  }

  print(model_spec_table %>% filter(Include == 1))
  return(model_spec_table)

}
