#' Model Decomposition and Contribution Calculations
#'
#' This Decomp function is compatible with the Bayesian models fitted with the MSMP framework
#' The format of the output is compatible with the legacy reach generation function.
#'
#' @param mod_obj - model object
#' @param kpi - kpi variable string
#' @param min_ref - vector of variable indexes
#' @param max_ref - vector of variable indexes
#' @param mean_ref - vector of variable indexes
#'
#' @return mod_obj
#'


Decomp <- function(mod_obj, NMP, min_ref = NULL, max_ref = NULL, mean_ref = NULL) {
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
    filter(Variable_Type == "Dependent") %>%
    pull(Trans_Variable)
  dpnd_var_raw <- str_remove(dpnd_var, "_trans")
  indp_var <- spec %>%
    filter(Variable_Type != "Dependent") %>%
    pull(Trans_Variable)

  mod_form <- toupper(mod_obj$ModelForm)
  mod_coef <- mod_obj$Model$coefficients

  kpi_data <- mod_obj$data_input$monthly %>%
    ungroup() %>%
    select(!!sym(cs), !!sym(ts), !!sym(kpi))

  conversion <- bind_cols(mod_obj$data_input$monthly %>%
    select(!!sym(ts), !!sym(cs), !!sym(kpi)), mod_obj$data_transformed$monthly %>% ungroup() %>% select(sales_div_tiv_trans)) %>%
    ungroup() %>%
    select(!!sym(ts), !!sym(cs), !!sym(kpi), !!sym(dpnd_var)) %>%
    mutate(conv = !!sym(kpi) / !!sym(dpnd_var))

  if (!str_detect(dpnd_var, "sales")) {
    stop("Please include regular sales data(not meansby) in model data. ")
  }

  # Populating model data DF including intercept
  mod_data_wide <- mod_data %>%
    mutate(Intercept = 1) %>%
    ungroup() %>%
    select(!!sym(ts), !!sym(cs), one_of(c(dpnd_var, indp_var))) %>%
    pivot_longer(one_of(c(dpnd_var, indp_var)), names_to = "var", values_to = "value") %>%
    mutate(var = str_c(!!sym(cs), var, sep = "_")) %>%
    select(-!!sym(cs)) %>%
    pivot_wider(names_from = var, values_from = value)


  ## Preparing dataset for dependent & indenpendent variable
  dpnd_var_decomp <- mod_data_wide %>% select(!!sym(ts), ends_with(dpnd_var))
  indp_var_decomp <- mod_data_wide %>% select(-ends_with(dpnd_var))


  if (length(indp_var) <= 0) {
    stop("Please specify which variables you would like to decomp.")
  }
  if (mod_form == "LIN_LIN") {
    indp_var_list <- indp_var_decomp %>%
      select(-!!sym(ts)) %>%
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


    # ind_decomp <- bind_cols(
    #   imap(indp_var_list, ~ .x * mod_coef$Estimate[str_detect(.y, mod_coef$Variables)]) %>%
    #     as_tibble(),
    #   indp_var_decomp %>% select(!!sym(ts))
    # )


    decomp <- bind_rows(dpnd_var_decomp %>% pivot_longer(-!!sym(ts), names_to = "var", values_to = "value"), ind_decomp %>%
      pivot_longer(-!!sym(ts), names_to = "var", values_to = "value")) %>%
      separate(var, c(cs, "var"), extra = "merge") %>%
      group_by(!!sym(ts), !!sym(cs), var) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      pivot_wider(names_from = var, values_from = value) %>%
      select(!!sym(ts), !!sym(cs), dpnd_var, everything()) %>%
      ungroup()
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
    ungroup() %>%
    left_join(conversion %>% ungroup() %>% select(!!sym(ts), !!sym(cs), conv)) %>%
    select_all(toupper) %>%
    select(!!sym(toupper(ts)), !!sym(toupper(cs)), CONV, everything(), -!!sym(toupper(dpnd_var))) %>%
    gather("var", "value", 4:ncol(.)) %>%
    mutate(value = CONV * value) %>%
    group_by(!!sym(toupper(ts)), !!sym(toupper(cs)), var) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    spread(var, value) %>%
    select(!!sym(toupper(ts)), !!sym(toupper(cs)), C, everything())

  ## Calling in categorization to apply grouping of variables

  #  decomp_prep[, toupper(dpnd_var)] <- pull(decomp, dpnd_var)
  categorization <- spec %>% select(Trans_Variable, AggregateVariable)
  categorization <- categorization %>%
    arrange(Trans_Variable) %>%
    mutate(
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
    select(!!sym(toupper(ts)), !!sym(toupper(cs)), everything()) %>%
    gather("variable", "value", 3:ncol(.)) %>%
    group_by(!!sym(toupper(ts)), !!sym(toupper(cs))) %>%
    summarise(value = sum(value)) %>%
    rename(tot = value)

  ## Dividing the fitted value to turn everything to % contribution & multiplying by KPI to get to unit contributions
  decomp_conv <- decomp_prep %>%
    select(!!sym(toupper(ts)), !!sym(toupper(cs)), everything()) %>%
    gather("variable", "value", 3:ncol(.)) %>%
    left_join(tot) %>%
    left_join(kpi_data %>% rename_all(toupper)) %>%
    mutate(pct = value / tot, converted = pct * !!sym(toupper(kpi))) %>%
    select(-value, -tot) %>%
    rename(value = converted)

  # Contributions table - by region & month
  contribution_return <- decomp_conv %>%
    mutate(FY = if_else(month(!!sym(toupper(ts))) > 3, year(!!sym(toupper(ts))), year(!!sym(toupper(ts))) - 1)) %>%
    left_join(var_cat_tbl) %>%
    select(!!sym(toupper(cs)), !!sym(toupper(ts)), FY, variable, Categories, value) %>%
    group_by(!!sym(toupper(cs)), FY, !!sym(toupper(ts)), Categories) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    reshape2::dcast(paste(toupper(cs), " + ", toupper(ts), " + ", "FY ~ Categories"), fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "value") %>%
    left_join(kpi_data %>% rename_all(toupper)) %>%
    select(!!sym(toupper(cs)), FY, !!sym(toupper(ts)), !!sym(toupper(kpi)), everything()) %>%
    rename(Group = (toupper(cs)), FiscalYear = FY, Date = (toupper(ts)), !!sym(toupper(NMP)) := toupper(kpi))

  # Contributions pct table - by region & month
  contribution2_return <- decomp_conv %>%
    mutate(FY = if_else(month(!!sym(toupper(ts))) > 3, year(!!sym(toupper(ts))), year(!!sym(toupper(ts))) - 1)) %>%
    left_join(var_cat_tbl) %>%
    select(!!sym(toupper(cs)), !!sym(toupper(ts)), FY, variable, Categories, pct) %>%
    group_by(!!sym(toupper(cs)), FY, !!sym(toupper(ts)), Categories) %>%
    summarise(pct = sum(pct, na.rm = TRUE)) %>%
    reshape2::dcast(paste(toupper(cs), " + ", toupper(ts), " + ", "FY ~ Categories"), fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "pct") %>%
    left_join(kpi_data %>% rename_all(toupper)) %>%
    select(!!sym(toupper(cs)), FY, !!sym(toupper(ts)), !!sym(toupper(kpi)), everything()) %>%
    rename(Group = (toupper(cs)), FiscalYear = FY, Date = (toupper(ts)), !!sym(toupper(NMP)) := toupper(kpi))

  # Regular summary table
  summary_return <- contribution_return %>%
    gather("variable", "value", 4:ncol(.)) %>%
    group_by(Group, FiscalYear, variable) %>%
    summarise(value = sum(value)) %>%
    reshape2::dcast(Group + FiscalYear ~ variable, fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "value") %>%
    select(Group, FiscalYear, !!sym(NMP), BASE, everything())

  # PCT summary table
  summary2_return <- summary_return
  summary2_return[, -(1:3)] <- summary2_return[, -(1:3)] / rowSums(summary2_return[, -(1:3)])

  # decomp_national
  decomp_national <- contribution_return %>%
    gather("var", "value", 4:ncol(.)) %>%
    group_by(FiscalYear, var) %>%
    summarise(value = sum(value)) %>%
    dcast(var ~ FiscalYear, fun.aggregate = function(x) {
      sum(x, na.rm = TRUE)
    }, value.var = "value") %>%
    rename(Categories = var)


  decomp_national <- rbind(decomp_national[decomp_national$Categories == NMP, ], decomp_national[decomp_national$Categories != NMP, ])

  # Variable Categorization Table
  variableTbl <- var_cat_tbl

  var_cat_tbl_national <- var_cat_tbl %>% left_join(decomp_national)

  # VarCont Table
  varCont <- decomp_prep %>%
    left_join(kpi_data %>% rename_all(toupper)) %>%
    gather("variable", "value", 3:ncol(.)) %>%
    left_join(var_cat_tbl %>% select(variable, Categories)) %>%
    group_by(!!sym(toupper(cs)), !!sym(toupper(ts)), variable, Categories) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    mutate(Categories = if_else(variable == toupper(kpi), NMP, Categories)) %>%
    reshape2::dcast(paste("variable + Categories + ", toupper(cs), " ~ ", toupper(ts)), fun.aggregate = function(x) {
      sum(x,
        na.rm = TRUE
      )
    }, value.var = "value") %>%
    mutate(Model = "Sales") %>%
    select(
      variable, Categories, Model, !!sym(toupper(cs)),
      everything()
    ) %>%
    dplyr::rename(Group = !!sym(toupper(cs)))

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
