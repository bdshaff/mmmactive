#' Model Decomposition and Contribution Calculations
#'
#' This Decomp function is compatible with the Bayesian models fitted with the MSMP framework
#' The format of the output is compatible with the legacy reach generation function.
#'
#' @param mod_obj - model object
#' @param min_ref - vector of variable indexes
#' @param max_ref - vector of variable indexes
#' @param mean_ref - vector of variable indexes
#'
#' @importFrom magrittr %>%
#' @return mod_obj
#' @export

Decomp2 <- function(mod_obj) {

  if (!is.modeled(mod_obj)) {
    stop("mod_obj must have a fitted model to run model decomposition.")
  }

  response_var <- mod_obj$spec %>%
    dplyr::filter(Variable_Type == "Dependent") %>%
    dplyr::pull(Trans_Variable)

  dims <- dim(model.matrix(mod_obj$Model))
  coef_matrix <- matrix(rep(mod_obj$Model$coefficients, each = dims[1]), nrow = dims[1], ncol = dims[2])
  contributions_matrix <- coef_matrix * model.matrix(mod_obj$Model)

  all.equal(rowSums(contributions_matrix), mod_obj$Model$fitted.values)

  percent_contributions_matrix <- contributions_matrix / mod_obj$Model$fitted.values

  #sum(rowSums(percent_contributions_matrix) == 1) == 0


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

  decomposition_matrix$fitted <- mod_obj$Model$fitted.values
  decomposition_matrix$residuals <- mod_obj$Model$residuals

  if(mod_obj$kpi == "sales_div_tiv"){
    decomposition_matrix$sales = pull(decomposition_matrix[, mod_obj$kpi]) * pull(decomposition_matrix[, "sales_tiv"])
    unit_contributions_matrix <- percent_contributions_matrix * decomposition_matrix$sales
  }else{
    unit_contributions_matrix <- percent_contributions_matrix * pull(decomposition_matrix[, mod_obj$kpi])
  }

  #all.equal(as.numeric(rowSums(unit_contributions_matrix)), pull(decomposition_matrix[, mod_obj$kpi]))

  decomposition_matrix <- decomposition_matrix %>% bind_cols(as.data.frame(unit_contributions_matrix))

  if(mod_obj$kpi == "sales_div_tiv"){
    decomposition_matrix$pred <- decomposition_matrix$fitted * decomposition_matrix$means_by * pull(decomposition_matrix[, "sales_tiv"])
    decomposition_table <-
      decomposition_matrix %>%
      select(-c(!!sym(response_var), pred, means_by, fitted, residuals)) %>%
      pivot_longer(-c(!!sym(mod_obj$cs), !!sym(mod_obj$Time), !!sym(mod_obj$kpi), sales, sales_tiv), names_to = "variable", values_to = "contribution")
  }else{
    decomposition_matrix$pred <- decomposition_matrix$fitted * decomposition_matrix$means_by
    decomposition_table <-
      decomposition_matrix %>%
      select(-c(!!sym(response_var), pred, means_by, fitted, residuals)) %>%
      pivot_longer(-c(!!sym(mod_obj$cs), !!sym(mod_obj$Time), !!sym(mod_obj$kpi)), names_to = "variable", values_to = "contribution")
  }

  decomposition_table_categorized =
    decomposition_table %>%
    left_join(mod_obj$spec[,c("Trans_Variable","AggregateVariable","Variable_Type")], by = c("variable" = "Trans_Variable")) %>%
    replace_na(list("AggregateVariable" = "BASE", "Variable_Type" = "Trend"))

  mod_obj$Decomp = list(decomposition_table = decomposition_table_categorized,
                        decomposition_matrix = decomposition_matrix)

  return(mod_obj)
}
