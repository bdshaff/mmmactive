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


Decomp <- function(mod_obj, min_ref = NULL, max_ref = NULL, mean_ref = NULL) {
  if (!is.modeled(mod_obj)) {
    stop("mod_obj must have a fitted model to run model decomposition.")
  }

  NMP <- mod_obj$NMP
  kpi <- mod_obj$kpi
  cs <- mod_obj$cs
  ts <- mod_obj$Time
  mod_data <- mod_obj$data
  spec <- mod_obj$spec

  dpnd_var <- spec %>%
    dplyr::filter(Variable_Type == "Dependent") %>%
    dplyr::pull(Trans_Variable)

  dpnd_var_raw <- str_remove(dpnd_var, "_trans")

  indp_var <- spec %>%
    dplyr::filter(Variable_Type != "Dependent") %>%
    dplyr::pull(Trans_Variable)

  mod_form <- toupper(mod_obj$ModelForm)
  mod_coef <- mod_obj$Model$coefficients

  kpi_data <- mod_obj$data_input$monthly %>%
    dplyr::ungroup() %>%
    dplyr::select(
      !!rlang::sym(cs),
      !!rlang::sym(ts),
      !!rlang::sym(kpi)
    )

  conversion <- dplyr::bind_cols(
    mod_obj$data_input$monthly %>%
      dplyr::select(
        !!rlang::sym(ts),
        !!rlang::sym(cs),
        !!rlang::sym(kpi)
      ),
    mod_obj$data_transformed$monthly %>%
      dplyr::ungroup() %>%
      dplyr::select(sales_div_tiv_trans)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      !!rlang::sym(ts),
      !!rlang::sym(cs),
      !!rlang::sym(kpi),
      !!rlang::sym(dpnd_var)
    ) %>%
    dplyr::mutate(conv = !!rlang::sym(kpi) / !!rlang::sym(dpnd_var))

  if (!str_detect(dpnd_var, "sales")) {
    stop("Please include regular sales data(not meansby) in model data. ")
  }

  # Populating model data DF including intercept
  mod_data_wide <- mod_data %>%
    dplyr::mutate(Intercept = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      !!rlang::sym(ts),
      !!rlang::sym(cs),
      tidyselect::one_of(c(dpnd_var, indp_var))
    ) %>%
    tidyr::pivot_longer(
      tidyselect::one_of(c(dpnd_var, indp_var)),
      names_to = "var",
      values_to = "value"
    ) %>%
    dplyr::mutate(var = str_c(!!rlang::sym(cs), var, sep = "_")) %>%
    dplyr::select(-!!rlang::sym(cs)) %>%
    tidyr::pivot_wider(names_from = var, values_from = value)


  ## Preparing dataset for dependent & indenpendent variable
  dpnd_var_decomp <- mod_data_wide %>% dplyr::select(!!rlang::sym(ts), tidyselect::ends_with(dpnd_var))
  indp_var_decomp <- mod_data_wide %>% dplyr::select(-tidyselect::ends_with(dpnd_var))


  if (length(indp_var) <= 0) {
    stop("Please specify which variables you would like to decomp.")
  }
  if (mod_form == "LIN_LIN") {
    indp_var_list <- indp_var_decomp %>%
      dplyr::select(-!!rlang::sym(ts)) %>%
      as.list()

    var_name_list <- mod_coef %>%
      dplyr::select(!!rlang::sym(cs), Variables) %>%
      tidyr::unite("Variables", !!rlang::sym(cs), Variables, sep = "_") %>%
      dplyr::pull(Variables)

    ind_decomp <- dplyr::bind_cols(
      purrr::imap(indp_var_list, ~ .x * mod_coef$Estimate[str_detect(.y, var_name_list)]) %>%
        tibble::as_tibble(),
      indp_var_decomp %>% dplyr::select(!!rlang::sym(ts))
    )


    decomp <- dplyr::bind_rows(dpnd_var_decomp %>% tidyr::pivot_longer(-!!rlang::sym(ts), names_to = "var", values_to = "value"), ind_decomp %>%
      tidyr::pivot_longer(-!!rlang::sym(ts), names_to = "var", values_to = "value")) %>%
      tidyr::separate(var, c(cs, "var"), extra = "merge") %>%
      dplyr::group_by(!!rlang::sym(ts), !!rlang::sym(cs), var) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
      tidyr::pivot_wider(names_from = var, values_from = value) %>%
      dplyr::select(!!rlang::sym(ts), !!rlang::sym(cs), dpnd_var, tidyselect::everything()) %>%
      dplyr::ungroup()
  }
  else if (mod_form == "LOG_LOG") {
    stop("Sorry, haven't implemented yet.")
  }

  constraint <- function(df, values, f) {
    if (is.null(values)) {
      df$C <- 0
      return(df)
    }
    function_values <- apply(
      as.data.frame(df[, values]),
      2, f
    )
    df[, values] <- sweep(
      df[, values] %>% as.data.frame(),
      2, function_values, "-"
    )
    df$C <- sum(function_values)
    return(df)
  }

  # Applying references to data
  decomp <- constraint(decomp, min_ref, min)
  decomp <- constraint(decomp, max_ref, max)
  decomp <- constraint(decomp, mean_ref, mean)

  ## transforming marketshare to unit sales by imposing the 'conversion' from transformed sales to regular sales
  decomp_prep <- decomp %>%
    dplyr::ungroup() %>%
    dplyr::left_join(conversion %>% dplyr::ungroup() %>% dplyr::select(!!rlang::sym(ts), !!rlang::sym(cs), conv)) %>%
    dplyr::select_all(toupper) %>%
    dplyr::select(!!rlang::sym(toupper(ts)), !!rlang::sym(toupper(cs)), CONV, tidyselect::everything(), -!!rlang::sym(toupper(dpnd_var))) %>%
    tidyr::gather("var", "value", 4:ncol(.)) %>%
    dplyr::mutate(value = CONV * value) %>%
    dplyr::group_by(!!rlang::sym(toupper(ts)), !!rlang::sym(toupper(cs)), var) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    tidyr::spread(var, value) %>%
    dplyr::select(!!rlang::sym(toupper(ts)), !!rlang::sym(toupper(cs)), C, tidyselect::everything())

  ## Calling in categorization to apply grouping of variables

  categorization <- spec %>% dplyr::select(Trans_Variable, AggregateVariable)
  categorization <- categorization %>%
    dplyr::arrange(Trans_Variable) %>%
    dplyr::mutate(
      Trans_Variable = str_trim(stringi::stri_trans_toupper(Trans_Variable, locale = "")),
      AggregateVariable = str_trim(stringi::stri_trans_toupper(AggregateVariable, locale = ""))
    )
  names(categorization) <- c("variable", "Agg1")
  var_cat <- base::as.data.frame(names(decomp_prep), stringsAsFactors = FALSE)
  var_cat <- base::apply(var_cat, 1, function(x) {
    if (x == "MONTH") {
      return("Date")
    }
    if (x == "FY") {
      return("FY")
    }
    if (x == "GEO") {
      return("GEO")
    }
    new_name <- categorization$Agg1[stringr::str_detect(
      x,
      fixed(categorization$variable)
    )]
    if (base::length(new_name) > 1) {
      new_name <- categorization$Agg1[stringdist::amatch(x,
        categorization$variable,
        maxDist = Inf
      )]
    }
    new_name <- new_name[1]
    result <- if (base::is.na(new_name)) {
      "BASE"
    } else {
      new_name
    }
    return(result)
  })

  # Var cat table - setting min,mean,max refs

  var_cat_tbl <- data.frame(
    variable = names(decomp_prep),
    modelType = mod_obj$ModelForm, Categories = c(var_cat),
    stringsAsFactors = FALSE
  )
  var_cat_tbl$ReferencePoints <- ""
  var_cat_tbl$ReferencePoints[min_ref] <- "Min"
  var_cat_tbl$ReferencePoints[mean_ref] <- "Average"
  var_cat_tbl$ReferencePoints[max_ref] <- "Max"

  ## preparing decomp conv to contribution by first taking the fitted value
  tot <- decomp_prep %>%
    dplyr::select(!!rlang::sym(toupper(ts)), !!rlang::sym(toupper(cs)), tidyselect::everything()) %>%
    tidyr::gather("variable", "value", 3:ncol(.)) %>%
    dplyr::group_by(!!rlang::sym(toupper(ts)), !!rlang::sym(toupper(cs))) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::rename(tot = value)

  ## Dividing the fitted value to turn tidyselect::everything to % contribution & multiplying by KPI to get to unit contributions
  decomp_conv <- decomp_prep %>%
    dplyr::select(!!rlang::sym(toupper(ts)), !!rlang::sym(toupper(cs)), tidyselect::everything()) %>%
    tidyr::gather("variable", "value", 3:ncol(.)) %>%
    dplyr::left_join(tot) %>%
    dplyr::left_join(kpi_data %>% dplyr::rename_all(toupper)) %>%
    dplyr::mutate(pct = value / tot, converted = pct * !!rlang::sym(toupper(kpi))) %>%
    dplyr::select(-value, -tot) %>%
    dplyr::rename(value = converted)

  # Contributions table - by region & month
  contribution_return <- decomp_conv %>%
    dplyr::mutate(FY = dplyr::if_else(month(!!rlang::sym(toupper(ts))) > 3, year(!!rlang::sym(toupper(ts))), year(!!rlang::sym(toupper(ts))) - 1)) %>%
    dplyr::left_join(var_cat_tbl) %>%
    dplyr::select(!!rlang::sym(toupper(cs)), !!rlang::sym(toupper(ts)), FY, variable, Categories, value) %>%
    dplyr::group_by(!!rlang::sym(toupper(cs)), FY, !!rlang::sym(toupper(ts)), Categories) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    reshape2::dcast(paste(toupper(cs), " + ", toupper(ts), " + ", "FY ~ Categories"), fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "value") %>%
    dplyr::left_join(kpi_data %>% dplyr::rename_all(toupper)) %>%
    dplyr::select(!!rlang::sym(toupper(cs)), FY, !!rlang::sym(toupper(ts)), !!rlang::sym(toupper(kpi)), tidyselect::everything()) %>%
    dplyr::rename(Group = (toupper(cs)), FiscalYear = FY, Date = (toupper(ts)), !!rlang::sym(toupper(NMP)) := toupper(kpi))

  # Contributions pct table - by region & month
  contribution2_return <- decomp_conv %>%
    dplyr::mutate(FY = dplyr::if_else(month(!!rlang::sym(toupper(ts))) > 3, year(!!rlang::sym(toupper(ts))), year(!!rlang::sym(toupper(ts))) - 1)) %>%
    dplyr::left_join(var_cat_tbl) %>%
    dplyr::select(!!rlang::sym(toupper(cs)), !!rlang::sym(toupper(ts)), FY, variable, Categories, pct) %>%
    dplyr::group_by(!!rlang::sym(toupper(cs)), FY, !!rlang::sym(toupper(ts)), Categories) %>%
    dplyr::summarise(pct = sum(pct, na.rm = TRUE)) %>%
    reshape2::dcast(paste(toupper(cs), " + ", toupper(ts), " + ", "FY ~ Categories"), fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "pct") %>%
    dplyr::left_join(kpi_data %>% dplyr::rename_all(toupper)) %>%
    dplyr::select(!!rlang::sym(toupper(cs)), FY, !!rlang::sym(toupper(ts)), !!rlang::sym(toupper(kpi)), tidyselect::everything()) %>%
    dplyr::rename(Group = (toupper(cs)), FiscalYear = FY, Date = (toupper(ts)), !!rlang::sym(toupper(NMP)) := toupper(kpi))

  # Regular summary table
  summary_return <- contribution_return %>%
    tidyr::gather("variable", "value", 4:ncol(.)) %>%
    dplyr::group_by(Group, FiscalYear, variable) %>%
    dplyr::summarise(value = sum(value)) %>%
    reshape2::dcast(Group + FiscalYear ~ variable, fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "value") %>%
    dplyr::select(Group, FiscalYear, !!rlang::sym(NMP), BASE, tidyselect::everything())

  # PCT summary table
  summary2_return <- summary_return
  summary2_return[, -(1:3)] <- summary2_return[, -(1:3)] / rowSums(summary2_return[, -(1:3)])

  # decomp_national
  decomp_national <- contribution_return %>%
    tidyr::gather("var", "value", 4:ncol(.)) %>%
    dplyr::group_by(FiscalYear, var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dcast(var ~ FiscalYear, fun.aggregate = function(x) {
      sum(x, na.rm = TRUE)
    }, value.var = "value") %>%
    dplyr::rename(Categories = var)


  decomp_national <- rbind(decomp_national[decomp_national$Categories == NMP, ], decomp_national[decomp_national$Categories != NMP, ])

  # Variable Categorization Table
  variableTbl <- var_cat_tbl

  var_cat_tbl_national <- var_cat_tbl %>% dplyr::left_join(decomp_national)

  # VarCont Table
  varCont <- decomp_prep %>%
    dplyr::left_join(kpi_data %>% dplyr::rename_all(toupper)) %>%
    tidyr::gather("variable", "value", 3:ncol(.)) %>%
    dplyr::left_join(var_cat_tbl %>% dplyr::select(variable, Categories)) %>%
    dplyr::group_by(!!rlang::sym(toupper(cs)), !!rlang::sym(toupper(ts)), variable, Categories) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::mutate(Categories = dplyr::if_else(variable == toupper(kpi), NMP, Categories)) %>%
    reshape2::dcast(paste("variable + Categories + ", toupper(cs), " ~ ", toupper(ts)), fun.aggregate = function(x) {
      sum(x,
        na.rm = TRUE
      )
    }, value.var = "value") %>%
    dplyr::mutate(Model = "Sales") %>%
    dplyr::select(
      variable, Categories, Model, !!rlang::sym(toupper(cs)),
      tidyselect::everything()
    ) %>%
    dplyr::rename(Group = !!rlang::sym(toupper(cs)))

  # Popluating list of tables that need to be referenced for unnesting

  decomp_list <- list(
    contributionsTbl = contribution_return,
    contributionsTblPerc = contribution2_return, summary = summary_return,
    summaryPerc = summary2_return, variableTbl = var_cat_tbl,
    varCont = varCont,
    nationalsum = decomp_national
  )

  mod_obj$decomp_list <- decomp_list
  return(mod_obj)
}
