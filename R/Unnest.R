#' Model Decomp Unnesting
#'
#' Unnesting functionality
#'
#' @param sales_mod_obj - model object
#' @param sub_mod_obj - model object
#'
#' @importFrom magrittr %>%
#' @return mod_obj
#' @export

Unnest <- function(sales_mod_obj, sub_mod_obj){

  if(is.null(sales_mod_obj$DecompUnnested)){
    salesmodel_contributions = sales_mod_obj$Decomp$decomposition_matrix
  }else{
    salesmodel_contributions = sales_mod_obj$DecompUnnested$unneseted_decomposition_matrix
  }

  submodel_contributions = sub_mod_obj$Decomp$decomposition_matrix
  nmp = sales_mod_obj$nmp

  salesmodel_response_var <- sales_mod_obj$spec %>%
    dplyr::filter(Variable_Type == "Dependent") %>%
    dplyr::pull(Trans_Variable)

  #Contributions from this variable from in the salesmodel will be unnested
  submodel_response_var <- sub_mod_obj$spec %>%
    dplyr::filter(Variable_Type == "Dependent") %>%
    dplyr::pull(Trans_Variable)

  if(submodel_response_var %in% names(salesmodel_contributions)){
    message(str_c("Unnesting ", submodel_response_var, " variable"))
  }else{
    stop(str_c(submodel_response_var, " not found in the sales model contribution matrix"))
  }

  if(is.null(sales_mod_obj$DecompUnnested$unnested_variables)){
    unnested_variables_list = c(submodel_response_var)
  }else{
    unnested_variables_list = c(unnested_variables_list, submodel_response_var)
  }

  contributions_to_unnest = salesmodel_contributions[,submodel_response_var][[1]]

  if(sum(!(contributions_to_unnest >= 0)) > 0){
    stop(str_c(submodel_response_var, " is not >= 0 everywhere. Can only unnest positive variabels."))
  }

  #Contributions from these variables in the submodel will be distrubuted into the unnested variable
  submodel_unnest_vars = sub_mod_obj$spec[sub_mod_obj$spec$Variable_Type %in% c("Marketing","Trend") & sub_mod_obj$spec$Orig_Variable != "Intercept","Trans_Variable"][[1]]


  to_distribute = submodel_contributions[submodel_contributions$nameplate == nmp, names(submodel_contributions) %in% c(submodel_unnest_vars)]
  to_distribute_pct = to_distribute/rowSums(to_distribute)
  names(to_distribute_pct) = str_c(sub_mod_obj$kpi, "_sub_",names(to_distribute_pct))
  to_distribute_pct = cbind(submodel_contributions[submodel_contributions$nameplate == nmp, names(submodel_contributions) %in% c(sub_mod_obj$Time)],to_distribute_pct)


  to_unneset = salesmodel_contributions %>% select(!!sym(sales_mod_obj$cs), !!sym(sales_mod_obj$Time)) %>% left_join(to_distribute_pct)
  unneseted_contributions = to_unneset[,-c(1,2)] * contributions_to_unnest
  unneseted_contributions = cbind(to_unneset[,c(1,2)], unneseted_contributions)

  unneseted_decomp_matrix =
    salesmodel_contributions %>%
    select(-!!sym(submodel_response_var)) %>%
    left_join(unneseted_contributions)


  unneseted_decomposition_table <-
    unneseted_decomp_matrix %>%
    select(-c(!!sym(salesmodel_response_var), pred, means_by, fitted, residuals)) %>%
    pivot_longer(-c(!!sym(sales_mod_obj$cs), !!sym(sales_mod_obj$Time), !!sym(sales_mod_obj$kpi), sales, sales_tiv), names_to = "variable", values_to = "contribution")


  submodel_categorization = sub_mod_obj$spec[,c("Trans_Variable","AggregateVariable","Variable_Type")]
  submodel_categorization$Trans_Variable = str_c(sub_mod_obj$kpi, "_sub_", submodel_categorization$Trans_Variable)
  submodel_categorization$Source_Model = sub_mod_obj$kpi

  salesmodel_categorization = sales_mod_obj$spec[,c("Trans_Variable","AggregateVariable","Variable_Type")]
  salesmodel_categorization$Source_Model = sales_mod_obj$kpi

  categorization =
    bind_rows(salesmodel_categorization,
              submodel_categorization)

  unneseted_decomposition_table_categorized =
    unneseted_decomposition_table %>%
    left_join(categorization, by = c("variable" = "Trans_Variable")) %>%
    replace_na(list("AggregateVariable" = "Base", "Variable_Type" = "Trend", "Source_Model" = sales_mod_obj$kpi))

  sales_mod_obj$DecompUnnested = list(
    unneseted_decomposition_matrix = unneseted_decomp_matrix,
    unneseted_decomposition_table = unneseted_decomposition_table_categorized,
    unnested_variables = unnested_variables_list
  )

  return(sales_mod_obj)

}
