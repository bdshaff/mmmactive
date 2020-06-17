#' Decomp
#'
#' @param obj - model object
#'

Decomp <- function(obj, min_ref = NULL, max_ref = NULL, mean_ref = NULL) {
  dpnd_var <- obj$spec %>%
    filter(Variable_Type == "Dependent") %>%
    pull(Trans_Variable)

  indp_var <- obj$spec %>%
    filter(Variable_Type != "Dependent") %>%
    pull(Trans_Variable)

  cs <- obj$cs
  ts <- obj$Time
  kpi <- obj$kpi
  mod_data <- obj$data %>% mutate(Intercept = 1)

  kpi_data <- obj$data_input$monthly %>%
    ungroup() %>%
    select(!!sym(cs), !!sym(ts), !!sym(kpi))

  mod_form <- toupper(obj$ModelForm)
  mod_coef <- obj$Model$coefficients

  if (!str_detect(dpnd_var, "sales")) {
    stop("Please include regular sales data(not meansby) in model data. ")
  }

  mod_data_wide <- mod_data %>%
    ungroup() %>%
    select(
      !!sym(ts),
      !!sym(cs), one_of(c(dpnd_var, indp_var))
    ) %>%
    pivot_longer(one_of(c(
      dpnd_var,
      indp_var
    )), names_to = "var", values_to = "value") %>%
    mutate(var = str_c(!!sym(cs), var, sep = "_")) %>%
    select(-!!sym(cs)) %>%
    pivot_wider(
      names_from = var,
      values_from = value
    )

  dpnd_var_decomp <- mod_data_wide %>% select(!!sym(ts), ends_with(dpnd_var))
  indp_var_decomp <- mod_data_wide %>% select(-ends_with(dpnd_var))
  if (length(indp_var) <= 0) {
    stop("Please specify which variables you would like to decomp.")
  }
  if (mod_form == "LIN_LIN") {
    indp_var_list <- indp_var_decomp %>% select(-!!sym(ts)) %>%
      as.list()

    var_name_list <- mod_coef %>%
      select(!!sym(cs), Variables) %>%
      unite("Variables", !!sym(cs), Variables, sep = "_") %>%
      pull(Variables)

    ind_decomp <- bind_cols(
      imap(indp_var_list, ~ .x * mod_coef$Estimate[str_detect(.y, var_name_list)]) %>%
        as_tibble(),
      indp_var_decomp %>% select(!!sym(ts))
    )

    decomp <- bind_rows(
      dpnd_var_decomp %>% pivot_longer(-!!sym(ts), names_to = "var", values_to = "value"),
      ind_decomp %>% pivot_longer(-!!sym(ts), names_to = "var", values_to = "value")
    ) %>%
      separate(var, c(cs, "var"), extra = "merge") %>%
      group_by(!!sym(ts), !!sym(cs), var) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      pivot_wider(names_from = var, values_from = value) %>%
      select(!!sym(ts), !!sym(cs), Intercept, everything(), -!!sym(dpnd_var)) %>%
      ungroup() %>%
      mutate(fitted = rowSums(.[3:ncol(.)]))
  }
  else if (mod_form == "LOG_LOG") {
    stop("Sorry, haven't implemented yet.")
  }

  ref_base_correction <- 0
  if (!is.null(min_ref)) {
    ref_base_correction <- c(ref_base_correction, apply(as.data.frame(select(
      decomp,
      min_ref
    )), MARGIN = 2, FUN = function(x) min(x, na.rm = TRUE)))
  }
  if (!is.null(max_ref)) {
    ref_base_correction <- c(ref_base_correction, apply(as.data.frame(select(
      decomp,
      min_ref
    )), MARGIN = 2, FUN = function(x) max(x, na.rm = TRUE)))
  }
  if (!is.null(mean_ref)) {
    ref_base_correction <- c(ref_base_correction, apply(as.data.frame(select(
      decomp,
      min_ref
    )), MARGIN = 2, FUN = function(x) {
      mean(x,
           na.rm = TRUE
      )
    }))
  }
  if (length(ref_base_correction) != 1) {
    ref_base_correction <- ref_base_correction[-1]
  }
  constraint <- function(df, values, f) {
    if (is.null(values)) {
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
    return(df)
  }
  decomp <- constraint(decomp, min_ref, min)
  decomp <- constraint(decomp, max_ref, max)
  decomp <- constraint(decomp, mean_ref, mean)


  decomp %>%
    left_join(kpi_data) %>%
    mutate(convs_factor = !!sym(kpi) / fitted) %>%
    select(!!sym(ts), !!sym(cs), convs_factor, everything(), -!!sym(kpi), -fitted) %>%
    gather("var", "value", 4:ncol(.)) %>%
    mutate(value = convs_factor * value) %>%
    group_by(!!sym(ts), !!sym(cs), var) %>%
    summarise(value = sum(value)) %>%
    spread(var, value) %>%
    mutate(FY = if_else(month(month) < 4, as.numeric(year(month) - 1), as.numeric(year(month)))) %>%
    left_join(kpi_data) %>%
    select(!!sym(ts), !!sym(cs), FY, !!sym(kpi), Intercept, everything()) %>%
    ungroup() %>%
    mutate(C = !!sym(kpi) - Reduce("+", .[5:ncol(.)])) %>%
    select_all(toupper) -> decomp_prep



  #  decomp_prep[, toupper(dpnd_var)] <- pull(decomp, dpnd_var)
  categorization <- obj$spec %>% select(Trans_Variable, AggregateVariable)
  categorization <- categorization %>%
    arrange(Trans_Variable) %>%
    mutate(Trans_Variable = str_trim(stringi::stri_trans_toupper(Trans_Variable,
                                                                 locale = ""
    )), AggregateVariable = str_trim(stringi::stri_trans_toupper(AggregateVariable,
                                                                 locale = ""
    )))

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
    if (x == sym(toupper(kpi))) {
      return("KPI")
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
  var_cat_tbl <- data.frame(
    variable = names(decomp_prep),
    modelType = obj$ModelForm, Categories = c(var_cat), stringsAsFactors = FALSE
  )
  var_cat_tbl$ReferencePoints <- ""
  var_cat_tbl$ReferencePoints[min_ref] <- "Min"
  var_cat_tbl$ReferencePoints[mean_ref] <- "Average"
  var_cat_tbl$ReferencePoints[max_ref] <- "Max"

  decomp_regional <- decomp_prep %>%
    gather("variable", "value", 4:ncol(.)) %>%
    left_join(var_cat_tbl %>% select(variable, Categories)) %>%
    group_by(FY, !!sym(toupper(cs)), variable, Categories) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    reshape2::dcast(paste(toupper(cs), " + ", "Categories ~ FY"), fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "value")

  decomp_national <- decomp_prep %>%
    gather(
      "variable",
      "value", 4:ncol(.)
    ) %>%
    left_join(var_cat_tbl %>%
                select(variable, Categories)) %>%
    group_by(
      FY, variable,
      Categories
    ) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    reshape2::dcast(variable + Categories ~ FY, fun.aggregate = function(x) {
      sum(x,
          na.rm = TRUE
      )
    }, value.var = "value")
  var_cat_tbl_national <- var_cat_tbl %>% left_join(decomp_national)
  summary <- decomp_prep %>%
    gather(
      "variable", "value",
      4:ncol(.)
    ) %>%
    left_join(var_cat_tbl %>% select(
      variable,
      Categories
    )) %>%
    group_by(FY, variable, Categories) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    reshape2::dcast(FY ~
                      Categories,
                    fun.aggregate = function(x) sum(x, na.rm = TRUE),
                    value.var = "value"
    ) %>%
    select(FY, KPI, everything())
  summary_pct_national <- summary
  summary_pct_national[, -(1:2)] <- summary_pct_national[
    ,
    -(1:2)
    ] / rowSums(summary_pct_national[, -(1:2)])
  contributionsTbl <- decomp_prep %>%
    gather(
      "variable",
      "value", 4:ncol(.)
    ) %>%
    left_join(var_cat_tbl %>%
                select(variable, Categories)) %>%
    group_by(
      !!sym(toupper(cs)),
      !!sym(toupper(ts)), FY, variable, Categories
    ) %>%
    summarise(value = sum(value,
                          na.rm = TRUE
    )) %>%
    reshape2::dcast(paste(
      toupper(cs),
      " + ", "FY", " + ", toupper(ts), " ~ Categories"
    ),
    fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "value"
    ) %>%
    select(
      !!sym(toupper(cs)), FY, !!sym(toupper(ts)), KPI,
      everything()
    )
  contributionsTblPerc <- contributionsTbl
  contributionsTblPerc[, -(1:4)] <- contributionsTblPerc[
    ,
    -(1:4)
    ] / rowSums(contributionsTblPerc[, -(1:4)])
  summary_regional <- decomp_prep %>%
    gather(
      "variable",
      "value", 4:ncol(.)
    ) %>%
    left_join(var_cat_tbl %>%
                select(variable, Categories)) %>%
    group_by(
      !!sym(toupper(cs)),
      FY, variable, Categories
    ) %>%
    summarise(value = sum(value,
                          na.rm = TRUE
    )) %>%
    reshape2::dcast(paste(
      toupper(cs),
      " + FY ~ Categories"
    ), fun.aggregate = function(x) {
      sum(x,
          na.rm = TRUE
      )
    }, value.var = "value") %>%
    select(
      !!sym(toupper(cs)),
      FY, KPI, everything()
    )
  summary_pct_regional <- summary_regional
  summary_pct_regional[, -(1:3)] <- summary_pct_regional[
    ,
    -(1:3)
    ] / rowSums(summary_pct_regional[, -(1:3)])
  base::message("\nDecomp start period:  ", obj$BeginDate)
  base::message("Decomp end period:    ", obj$EndDate)
  varCont <- decomp_prep %>%
    gather(
      "variable", "value",
      4:ncol(.)
    ) %>%
    left_join(var_cat_tbl %>% select(
      variable,
      Categories
    )) %>%
    group_by(
      !!sym(toupper(cs)), !!sym(toupper(ts)),
      variable, Categories
    ) %>%
    summarise(value = sum(value,
                          na.rm = TRUE
    )) %>%
    reshape2::dcast(paste(
      "variable + Categories + ",
      toupper(cs), " ~ ", toupper(ts)
    ), fun.aggregate = function(x) {
      sum(x,
          na.rm = TRUE
      )
    }, value.var = "value") %>%
    mutate(Model = "Sales") %>%
    select(
      variable, Categories, Model, !!sym(toupper(cs)),
      everything()
    ) %>%
    dplyr::rename(Group = !!sym(toupper(cs))) %>%
    mutate(variable = case_when(Categories == "KPI" ~
                                  toupper(dpnd_var), TRUE ~ variable))
  contributionsTbl <- contributionsTbl %>% rename(
    Group = !!sym(toupper(cs)),
    FiscalYear = FY, Date = !!sym(toupper(ts))
  )
  contributionsTblPerc <- contributionsTblPerc %>% rename(
    Group = !!sym(toupper(cs)),
    FiscalYear = FY, Date = !!sym(toupper(ts))
  )
  summary <- summary_regional %>% rename(
    Group = !!sym(toupper(cs)),
    FiscalYear = FY
  )
  summaryPerc <- summary_pct_regional %>% rename(
    Group = !!sym(toupper(cs)),
    FiscalYear = FY
  )
  decomp_list <- list(
    contributionsTbl = contributionsTbl,
    contributionsTblPerc = contributionsTblPerc, summary = summary,
    summaryPerc = summaryPerc, variableTbl = var_cat_tbl,
    varCont = varCont
  )
  obj$decomp_list <- decomp_list
  return(obj)
}

