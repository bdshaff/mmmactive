#' Execute Model Fitting
#'
#' Fitting an MMM model on top of a mod_obj
#'
#' @param mod_obj - model object
#' @param Method - Fitting method
#'
#' @return mod_obj
#'
#' @export

Run_Model3 <- function(mod_obj, method = NULL) {
  if (!is.activated(mod_obj)) {
    stop("mod_obj must be activated.")
  }
  if (!is.data_transformed(mod_obj)) {
    stop("mod_obj must have transformed data to run model.")
  }

  if (!is.null(mod_obj$cs)) {
    cs <- mod_obj$cs
  }

  if (!is.null(mod_obj$Time)) {
    ts <- mod_obj$Time
  }

  spec <- mod_obj$spec
  x <- mod_obj$data
  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]
  IV <- spec$Trans_Variable[spec$Variable_Type != "Dependent"]
  spec <- spec[spec$Variable_Type != "Dependent", ]


  varyby_vars <- spec %>%
    filter(VaryBy != "None") %>%
    pull(Trans_Variable)
  varyby_rep_vars <- spec %>%
    filter(VaryBy != "None") %>%
    select(VaryBy, Trans_Variable) %>%
    unite(Trans_VariableVaryBy, sep = ":") %>%
    pull()

  for (i in seq_along(varyby_vars)) {
    IV <- str_replace(IV, varyby_vars[i], varyby_rep_vars[i])
  }


  if ("Intercept" %in% spec$Orig_Variable) {
    eq_lm <- paste(IV[!IV %in% "Intercept"], collapse = " + ")
    eq_lm <- paste(DepVar, " ~ ", eq_lm)
    eq_lm <- stats::as.formula(eq_lm)

    intercept_prior_mean <- as.numeric(spec[spec$Orig_Variable == "Intercept", "Prior_Mean"])
    intercept_prior_sd <- as.numeric(spec[spec$Orig_Variable == "Intercept", "Prior_SD"])
  } else {
    eq_lm <- paste(IV[!IV %in% "Intercept"], collapse = " + ")
    eq_lm <- paste(DepVar, " ~ -1 +", eq_lm)
    eq_lm <- stats::as.formula(eq_lm)

    intercept_prior_mean <- NULL
    intercept_prior_sd <- NULL
  }

  if (nrow(unique(x[, cs])) > 1) {
    eq_lm <- update(eq_lm, paste("~ . +", cs))
  }

  if (method == "stan_glm") {
    mod_matrix <- model.matrix(eq_lm, data = x)
    trms <- terms(eq_lm)
    trm_labs <- attr(trms, "term.labels")

    prior_table <-
      data.frame(assign = attr(mod_matrix, "assign")) %>%
      left_join(
        data.frame(varyby_rep_vars = trm_labs, assign = seq_along(trm_labs)) %>%
          left_join(
            data.frame(Trans_Variable = varyby_vars, varyby_rep_vars) %>%
              left_join(
                spec %>% filter(VaryBy != "None") %>% select(Trans_Variable, Prior_Mean, Prior_SD)
              ) %>% select(-Trans_Variable)
          )
      )

    if(!is.null(intercept_prior_mean)){
      prior_table =
        prior_table %>%
        mutate(varyby_rep_vars = if_else(assign == 0, "(Intercept)", varyby_rep_vars)) %>%
        mutate(Prior_Mean = if_else(assign == 0, intercept_prior_mean, Prior_Mean)) %>%
        mutate(Prior_SD = if_else(assign == 0, intercept_prior_sd, Prior_SD)) %>%
        replace_na(list(Prior_Mean = 0, Prior_SD = 1))
    }else{
      prior_table =
        prior_table %>%
        replace_na(list(Prior_Mean = 0, Prior_SD = 1))
    }

    prior_table <-
      bind_rows(
        prior_table %>%
          filter(!(varyby_rep_vars %in% (spec %>% filter(VaryBy == "None" & Orig_Variable != "Intercept") %>%
                                           pull(Trans_Variable)))),
        prior_table %>%
          select(-Prior_Mean, -Prior_SD) %>%
          right_join(
            spec %>%
              filter(VaryBy == "None" & Orig_Variable != "Intercept") %>%
              select(Trans_Variable, Prior_Mean, Prior_SD),
            by = c("varyby_rep_vars" = "Trans_Variable")
          )
      ) %>%
      arrange(assign, varyby_rep_vars)

    prior_locations <- prior_table[prior_table$assign != 0, "Prior_Mean"]
    prior_scales <- prior_table[prior_table$assign != 0, "Prior_SD"]

    bfit <- stan_glm(eq_lm,
                     data = x,
                     prior_intercept = normal(intercept_prior_mean, intercept_prior_sd),
                     prior = normal(prior_locations, prior_scales))


    mod_obj$Model <- bfit
    mod_obj$Model$prior_table <- prior_table
    mod_obj$Model$eq <- eq_lm
    mod_obj$Model$method <- "stan_glm"
    mod_obj$Model$R2 <- 1 - (var(residuals(bfit)) / (var(residuals(bfit)) + var(fitted(bfit))))
    mod_obj$Model$vif <- try(car::vif(bfit), silent = TRUE)
    mod_obj$Model$DW <- try(broom::tidy(car::durbinWatsonTest(bfit)), silent = TRUE)
  } else if (method == "lm") {
    lfit <- lm(eq_lm, data = x)
    mod_obj$Model <- lfit
    mod_obj$Model$eq <- eq_lm
    mod_obj$Model$method <- "lm"
    mod_obj$Model$R2 <- 1 - (var(residuals(lfit)) / (var(residuals(lfit)) + var(fitted(lfit))))
    mod_obj$Model$vif <- try(car::vif(lfit), silent = TRUE)
    mod_obj$Model$DW <- try(broom::tidy(car::durbinWatsonTest(lfit)), silent = TRUE)
  } else{
    stop("method must be one of stan_glm, lm")
  }

  return(mod_obj)
}





