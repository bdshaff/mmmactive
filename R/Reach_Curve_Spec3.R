#' Reach_Curve_Spec3
#'
#' @param mod_obj
#' @param fiscal_year
#'
#' @export

Reach_Curve_Spec3 = function(mod_obj, fiscal_year){

  if(!is.null(mod_obj$DecompUnnested)){
    decomposition_table = mod_obj$DecompUnnested$unneseted_decomposition_table
    reach_curve_spec =
      decomposition_table %>%
      filter(Variable_Type == "Marketing",
             !str_detect(variable, "halo")) %>%
      mutate(FY = str_sub(month - months(3), 1, 4),
             Model = Source_Model) %>%
      group_by(Trans_Variable = variable, AggregateVariable, Model, FY) %>%
      summarise(contribution = sum(contribution)) %>%
      filter(FY == as.character(fiscal_year), contribution > 0)  %>%
      left_join(
        mod_obj$spec_unnested %>%
          select(Orig_Variable, Trans_Variable, AggregateVariable, Effective, Recency, Decay, Period)
      ) %>%
      mutate(media_agg = toupper(AggregateVariable)) %>%
      left_join(
        mod_obj$Stable %>%
          ungroup() %>%
          filter(FY == fiscal_year) %>%
          select(spend, media_agg = AggregateVariable)
      ) %>%
      select(-media_agg)
  }else{
    decomposition_table = mod_obj$Decomp$decomposition_table
    reach_curve_spec =
      decomposition_table %>%
      filter(Variable_Type == "Marketing",
             !str_detect(variable, "halo")) %>%
      mutate(FY = str_sub(month - months(3), 1, 4),
             Model = mod_obj$kpi) %>%
      group_by(Trans_Variable = variable, AggregateVariable, Model, FY) %>%
      summarise(contribution = sum(contribution)) %>%
      filter(FY == as.character(fiscal_year), contribution > 0)  %>%
      left_join(
        mod_obj$spec %>%
          select(Orig_Variable, Trans_Variable, AggregateVariable, Effective, Recency, Decay, Period)
      ) %>%
      mutate(media_agg = toupper(AggregateVariable)) %>%
      left_join(
        mod_obj$Stable %>%
          ungroup() %>%
          filter(FY == fiscal_year) %>%
          select(spend, media_agg = AggregateVariable)
      ) %>%
      select(-media_agg)
  }

  mod_obj$reach_curve_spec = reach_curve_spec

  return(mod_obj)

}
