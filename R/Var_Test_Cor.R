#' Var_Test_Cor
#'
#' Test Corr vs Resids
#'
#' @param mod_obj - model mod_object
#' @param print - TRUE or FALSE
#'
#' @return data.frame
#' @export

Var_Test_Cor <- function(mod_obj = NULL, print = NULL) {
  if (!is.modeled(mod_obj)) {
    stop("mod_obj must have a fitted model to run model decomposition.")
  }

  data_input <- mod_obj$data_input
  spec <- mod_obj$spec
  fit_curves <- mod_obj$fit_curves
  spec_trans <- spec %>% dplyr::filter(Transform == "Y")
  cross_section <- mod_obj$cs
  trans_variable <- mod_obj$spec$Trans_Variable
  kpi <- mod_obj$kpi
  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]

  # All Variables in Raw Data
  all_vars <-
    purrr::map(names(data_input),
               ~ names(data_input[[.x]])) %>%
    purrr::set_names(names(data_input))

  # Used Variables in Spec File
  u_vars <-
    purrr::map(names(data_input),
               ~ spec_trans$Orig_Variable[spec_trans$Orig_Variable %in% names(data_input[[.x]])]) %>%
    purrr::set_names(names(data_input))

  # Unused Variables
  un_vars <-
    purrr::map(names(data_input),
               ~ all_vars[[.x]][!all_vars[[.x]] %in% c(u_vars[[.x]], "vehicle", "region", "week", "month")]) %>%
    purrr::set_names(names(data_input))

  # Raw data of Unused Variables
  tmpDF <-
    purrr::map(names(data_input),
               ~ data_input[[.x]] %>% dplyr::select(tidyselect::one_of(un_vars[[.x]]))) %>%
    purrr::set_names(names(data_input))

  # Splitting by region
  tmpDFsplit_monthly <-
    tmpDF[["monthly"]] %>%
    dplyr::group_by(!!rlang::sym(cross_section)) %>%
    dplyr::group_split()

  tmpDFsplit_weekly <-
    tmpDF[["weekly"]] %>%
    dplyr::group_by(!!rlang::sym(cross_section)) %>%
    dplyr::group_split()

  ####################################################################################################################
  # Selecting params for AdResponse and AdStock
  eMax <- 8
  rMax <- 5
  pMax <- 70
  dMax <- 20
  lMax <- 9

  perams_adr <-
    list(
      "Effective" = seq(1, eMax, 1),
      "Recency" = seq(1, rMax, 1),
      "Period" = seq(7, pMax, 7),
      "Decay" = seq(10, dMax, 10)
      )

  perams_ads <- list("Lag" = seq(0, lMax, 1), "Scale" = 1)

  params_adr <-
    expand.grid(perams_adr) %>%
    dplyr::mutate(random = ifelse(Effective >= Recency, 1, 0)) %>%
    dplyr::filter(random == 1)

  params_ads <- expand.grid(perams_ads) %>% dplyr::mutate(random = 1)

  ####################################################################################################################
  # ADResponse
  ####################################################################################################################

  # Creating Spec File with unused Variables
  adr_var <- as.data.frame(names(tmpDF$weekly)) %>%
    dplyr::rename("Orig_Variable" = "names(tmpDF$weekly)") %>%
    dplyr::filter(!Orig_Variable %in% c("region", "vehicle")) %>%
    dplyr::mutate(
      Orig_Variable = as.character(Orig_Variable),
      random = 1
    )

  adr_join <- adr_var %>%
    dplyr::full_join(params_adr) %>%
    dplyr::select(-random)

  adr_spec <- adr_join %>%
    dplyr::mutate(
      Trans_Variable = paste0(Orig_Variable, "E", Effective, "R", Recency, "P", Period, "D", Decay),
      Variable_Type = "Marketing",
      Transform = "Y",
      TransformType = "adr",
      Lag = 0,
      Scale = 1
    )

  # Getting all possible transformations for the unused variables
  tmpDFtransform_adr <-
    purrr::map(tmpDFsplit_weekly,
               ~ TransformSplit(.x, spec_split = adr_spec, fit_curves = fit_curves, print =TRUE))%>%
    dplyr::bind_rows()

  # Grouping into months ---mean #%reach
  weeklyDF1 <-
    dplyr::bind_cols(data_input$weekly %>% dplyr::select("vehicle", "region", "week"), tmpDFtransform_adr) %>%
    dtplyr::lazy_dt()

  weeklyDF <- weeklyDF1 %>%
    dplyr::ungroup() %>%
    dplyr::mutate(month = lubridate::floor_date(week, unit = "month")) %>%
    dplyr::select(-week) %>%
    dplyr::filter(month >= mod_obj$BeginDate & month <= mod_obj$EndDate) %>%
    dplyr::group_by(vehicle, region, month) %>%
    dplyr::summarise_all(.funs = function(x) mean(x, na.rm = TRUE)) %>%
    tibble::as_tibble()

  # Adding residuals to get correlation
  weeklyDF$resids <- mod_obj$Model$residuals

  corr_readyDF1 <- weeklyDF %>%
    dplyr::group_by(!!rlang::sym(cross_section)) %>%
    dplyr::group_split()

  get_cor <- function(df) {
    corDF <- stats::cor(df$resids, df %>% dplyr::select(-c(resids, vehicle, region, month)))
    t <- corDF %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("variable") %>%
      dplyr::mutate(region = unique(df$region))
    return(t)
  }

  corr_regDF1 <-
    purrr::map(corr_readyDF1, ~ get_cor(.x)) %>%
    dplyr::bind_rows()

  # Averaging correlation across regions
  corr_totDF1 <- corr_regDF1 %>%
    dplyr::filter(!is.na(V1)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(V1 = mean(V1))

  ####################################################################################################################
  # ADStock
  ####################################################################################################################

  # Creating Spec File with unused Variables
  ads_var <- as.data.frame(names(tmpDF$monthly)) %>%
    dplyr::rename("Orig_Variable" = "names(tmpDF$monthly)") %>%
    dplyr::filter(!Orig_Variable %in% c("region", "vehicle", "sales", "sales_tiv", "pnur", "msrp", mod_obj$kpi)) %>%
    dplyr::mutate(Orig_Variable = as.character(Orig_Variable), random = 1)

  ads_join <- ads_var %>%
    dplyr::full_join(params_ads) %>%
    dplyr::select(-random)

  # ---Check TransformType categorization. Scale always 1?
  ads_spec <- ads_join %>%
    dplyr::mutate(
      Trans_Variable = paste0(
        Orig_Variable, "_trans",
        ifelse(Scale == 1, "_log", ""),
        ifelse(Lag != 0, paste0("_lag", Lag), "")
      ),
      Variable_Type = "Trend",
      Transform = "Y",
      TransformType = ifelse(grepl("pnur|msrp", Orig_Variable) |
                               Orig_Variable %in% c("factor1", "factor2", "factor3", "factor4"), "None", "log_lag"),
      Effective = 0,
      Reach = 0,
      Period = 0,
      Decay = 0
    )

  # Getting all possible transformations for the unused variables
  tmpDFtransform_ads <-
    purrr::map(tmpDFsplit_monthly, ~ TransformSplit(.x, spec_split = ads_spec, fit_curves = fit_curves, print = TRUE)) %>%
    dplyr::bind_rows()


  # Grouping into months ---sum
  monthlyDF1 <-
    dplyr::bind_cols(data_input$monthly %>% dplyr::select("vehicle", "region", "month"), tmpDFtransform_ads) %>%
    dtplyr::lazy_dt()

  monthlyDF <- monthlyDF1 %>%
    dplyr::ungroup() %>%
    dplyr::mutate(month = lubridate::floor_date(month, unit = "month")) %>%
    dplyr::filter(month >= mod_obj$BeginDate & month <= mod_obj$EndDate) %>%
    dplyr::group_by(vehicle, region, month) %>%
    dplyr::summarise_all(.funs = function(x) sum(x, na.rm = TRUE)) %>%
    tibble::as_tibble()

  # Adding residuals to get correlation
  monthlyDF$resids <- mod_obj$Model$residuals

  corr_readyDF2 <- monthlyDF %>%
    dplyr::group_by(!!rlang::sym(cross_section)) %>%
    dplyr::group_split()

  corr_regDF2 <-
    purrr::map(corr_readyDF2, ~ get_cor(.x)) %>%
    dplyr::bind_rows()

  # Averaging correlation across regions
  corr_totDF2 <- corr_regDF2 %>%
    dplyr::filter(!is.na(V1)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(V1 = mean(V1))

  ####################################################################################################################
  # Binding AdResponse and AdStock Correlations
  ####################################################################################################################

  corr <-
    dplyr::bind_rows(corr_totDF1, corr_totDF2) %>%
    dplyr::mutate(orig_variable = ifelse(grepl("trans", variable),
                                         str_split_fixed(variable, "_trans", 2)[, 1],
                                         str_split_fixed(variable, "E", 2)[, 1]))

  # Selecting top 5 transformations based on Corr for each variables
  final <- corr %>%
    dplyr::group_by(orig_variable) %>%
    dplyr::arrange(dplyr::desc(V1), .by_group =TRUE) %>%
    dplyr::slice(seq_len(5))

  return(final)
}
