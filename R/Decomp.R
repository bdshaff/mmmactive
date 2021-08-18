#' Model Decomposition and Contribution Calculations
#'
#' This Decomp function is compatible with the Bayesian models fitted with the MSMP framework
#' The format of the output is compatible with the legacy reach generation function.
#'
#' @param mod_obj - model object
#' @param min_ref_var_names - vector of variable indexes
#' @param mean_ref_var_names - vector of variable indexes
#'
#' @importFrom magrittr %>%
#' @return mod_obj
#' @export

Decomp = function(mod_obj, min_ref_var_names = NULL, mean_ref_var_names = NULL) {

  if(mod_obj$Model$method == "bayesian_linear_regression"){
    model_matrix = mod_obj$Model$mod_matrix
    coefs = mod_obj$Model$coefs$Estimate
    names(coefs) = mod_obj$Model$coefs$Variables
    fitted_values = mod_obj$Model$fitted_value
  }else if(mod_obj$Model$method == "linear_regression"){
    model_matrix = model.matrix(mod_obj$Model)
    colnames(model_matrix)[str_detect(colnames(model_matrix),"(Intercept)")] = "Intercept"
    coefs = mod_obj$Model$coefficients
    fitted_values = mod_obj$Model$fitted.values
  }else if(mod_obj$Model$method %in% c("stan_glm","lm")){
    model_matrix = model.matrix(mod_obj$Model)
    colnames(model_matrix)[str_detect(colnames(model_matrix),"(Intercept)")] = "Intercept"
    coefs = mod_obj$Model$coefficients
    names(coefs)[str_detect(names(coefs),"(Intercept)")] = "Intercept"
    fitted_values = mod_obj$Model$fitted.values
  }

  if (!is.modeled(mod_obj)) {
    stop("mod_obj must have a fitted model to run model decomposition.")
  }

  response_var <- mod_obj$spec %>%
    dplyr::filter(Variable_Type == "Dependent") %>%
    dplyr::pull(Trans_Variable)

  dims <- dim(model_matrix)
  coef_matrix <- matrix(rep(coefs, each = dims[1]), nrow = dims[1], ncol = dims[2])
  contributions_matrix <- coef_matrix * model_matrix

  #check fitted values were correctly decomposed
  all.equal(rowSums(contributions_matrix), mod_obj$Model$fitted.values)

  percent_contributions_matrix <- contributions_matrix / fitted_values

  #check all rows sum to 1 and there are nrow of them
  sum(rowSums(percent_contributions_matrix)) == nrow(percent_contributions_matrix)


  if(mod_obj$kpi == "sales_div_tiv"){
    decomposition_matrix <-
      mod_obj$data %>%
      select(!!sym(mod_obj$cs), !!sym(mod_obj$Time), !!sym(response_var)) %>%
      left_join(
        mod_obj$data_input$monthly %>%
          group_by(!!sym(mod_obj$cs)) %>%
          select(!!sym(mod_obj$cs), !!sym(mod_obj$Time), !!sym(mod_obj$kpi), sales_tiv) %>%
          mutate(means_by = mean(!!sym(mod_obj$kpi)))
      )
  }else{
    decomposition_matrix <-
      mod_obj$data %>%
      select(!!sym(mod_obj$cs), !!sym(mod_obj$Time), !!sym(response_var)) %>%
      left_join(
        mod_obj$data_input$monthly %>%
          group_by(!!sym(mod_obj$cs)) %>%
          select(!!sym(mod_obj$cs), !!sym(mod_obj$Time), !!sym(mod_obj$kpi)) %>%
          mutate(means_by = mean(!!sym(mod_obj$kpi)))
      )
  }

  decomposition_matrix$fitted <- fitted_values
  decomposition_matrix$residuals <- mod_obj$Model$residuals

  if(mod_obj$kpi == "sales_div_tiv"){
    decomposition_matrix$sales = pull(decomposition_matrix[, mod_obj$kpi]) * pull(decomposition_matrix[, "sales_tiv"])
    unit_contributions_matrix <- percent_contributions_matrix * decomposition_matrix$sales
  }else{
    unit_contributions_matrix <- percent_contributions_matrix * pull(decomposition_matrix[, mod_obj$kpi])
  }

  decomposition_matrix <- decomposition_matrix %>% bind_cols(as.data.frame(unit_contributions_matrix))

  if(mod_obj$kpi == "sales_div_tiv"){
    decomposition_matrix$pred <- decomposition_matrix$fitted * decomposition_matrix$means_by * pull(decomposition_matrix[, "sales_tiv"])

    if(!is.null(min_ref_var_names)){
      decomposition_matrix$min_ref_collect = 0
      for(i in 1:length(min_ref_var_names)){

        min_val = min(decomposition_matrix[,min_ref_var_names[i]][[1]], na.rm = TRUE)

        decomposition_matrix  =
          decomposition_matrix %>%
          mutate(min_ref_collect = min_ref_collect + !!sym(min_ref_var_names[i]) - min_val,
                 !!sym(min_ref_var_names[i]) := min_val)
      }
    }

    if(!is.null(mean_ref_var_names)){
      decomposition_matrix$mean_ref_collect = 0
      for(i in 1:length(mean_ref_var_names)){

        mean_val = mean(decomposition_matrix[,mean_ref_var_names[i]][[1]], na.rm = TRUE)

        decomposition_matrix  =
          decomposition_matrix %>%
          mutate(mean_ref_collect = mean_ref_collect + !!sym(mean_ref_var_names[i]) - mean_val,
                 !!sym(mean_ref_var_names[i]) := mean_val)
      }
    }


    decomposition_table <-
      decomposition_matrix %>%
      select(-c(!!sym(response_var), pred, means_by, fitted, residuals)) %>%
      pivot_longer(-c(!!sym(mod_obj$cs), !!sym(mod_obj$Time), !!sym(mod_obj$kpi), sales, sales_tiv), names_to = "variable", values_to = "contribution")
  }else{
    decomposition_matrix$pred <- decomposition_matrix$fitted * decomposition_matrix$means_by

    if(!is.null(min_ref_var_names)){
      decomposition_matrix$min_ref_collect = 0
      for(i in 1:length(min_ref_var_names)){

        min_val = min(decomposition_matrix[,min_ref_var_names[i]][[1]], na.rm = TRUE)

        decomposition_matrix  =
          decomposition_matrix %>%
          mutate(min_ref_collect = min_ref_collect + !!sym(min_ref_var_names[i]) - min_val,
                 !!sym(min_ref_var_names[i]) := min_val)
      }
    }

    if(!is.null(mean_ref_var_names)){
      decomposition_matrix$mean_ref_collect = 0
      for(i in 1:length(mean_ref_var_names)){

        mean_val = mean(decomposition_matrix[,mean_ref_var_names[i]][[1]], na.rm = TRUE)

        decomposition_matrix  =
          decomposition_matrix %>%
          mutate(mean_ref_collect = mean_ref_collect + !!sym(mean_ref_var_names[i]) - mean_val,
                 !!sym(mean_ref_var_names[i]) := mean_val)
      }
    }

    decomposition_table <-
      decomposition_matrix %>%
      select(-c(!!sym(response_var), pred, means_by, fitted, residuals)) %>%
      pivot_longer(-c(!!sym(mod_obj$cs), !!sym(mod_obj$Time), !!sym(mod_obj$kpi)), names_to = "variable", values_to = "contribution")
  }

  decomposition_table_categorized =
    decomposition_table %>%
    mutate(variable = str_remove(variable, "^[^_]*:")) %>%
    left_join(mod_obj$spec[,c("Trans_Variable","AggregateVariable","Variable_Type")], by = c("variable" = "Trans_Variable")) %>%
    replace_na(list("AggregateVariable" = "Base", "Variable_Type" = "Trend"))

  mod_obj$Decomp = list(decomposition_table = decomposition_table_categorized,
                        decomposition_matrix = decomposition_matrix)

  return(mod_obj)
}
