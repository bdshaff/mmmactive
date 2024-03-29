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

Run_Model <- function(mod_obj, method = NULL) {
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

  # do some checking and make sure the model variables (DepVar and IV) are in data
  if (DepVar %in% names(x)) {
    y <- x[[DepVar]]
  } else {
    stop("The dependent variable", DepVar, "is not in the model dataset. ")
  }

  if ("Intercept" %in% (spec$Trans_Variable)) {
    x$Intercept <- 1
  }

  if (FALSE %in% (IV %in% names(x))) {
    stop(
      "Some of the independent variables ",
      setdiff(IV, names(x)[IV %in% names(x)]),
      " are not in the model x set. Please check _Variables.csv file. "
    )
  }


  if(nrow(unique(x[,cs])) > 1){
    eq_lm <- paste(IV, collapse = " + ")
    eq_lm <- paste(eq_lm, " + ", cs)
    eq_lm <- paste(DepVar, " ~ -1 +", eq_lm)
    eq_lm <- stats::as.formula(eq_lm)
  }else if(nrow(unique(x[,cs])) == 1){
    eq_lm <- paste(IV, collapse = " + ")
    eq_lm <- paste(DepVar, " ~ -1 +", eq_lm)
    eq_lm <- stats::as.formula(eq_lm)
  }

  lmModel <- stats::lm(eq_lm, data = x)

  if(nrow(unique(x[,cs])) > 1){
    eq_lm2 <- paste(IV[!IV %in% "Intercept"], collapse = " + ")
    eq_lm2 <- paste(eq_lm2, " + ", cs)
    eq_lm2 <- paste(DepVar, " ~ +", eq_lm2)
    eq_lm2 <- stats::as.formula(eq_lm2)
  }else if(nrow(unique(x[,cs])) == 1){
    eq_lm2 <- paste(IV[!IV %in% "Intercept"], collapse = " + ")
    eq_lm2 <- paste(DepVar, " ~ ", eq_lm2)
    eq_lm2 <- stats::as.formula(eq_lm2)
  }

  lmModel2 <- stats::lm(eq_lm2, data = x)

  if(method == "linear_regression"){

    names(lmModel$coefficients)[str_detect(names(lmModel$coefficients),"(Intercept)")] = "Intercept"
    mod_obj$Model <- lmModel
    mod_obj$Model$eq <- eq_lm

  }else if(method == "bayesian_linear_regression"){

    priors <- dplyr::select(spec, Orig_Variable, Trans_Variable, Prior_Mean, Prior_SD)
    big_number <- 100000 # for diffuse priors
    Method <- "Bayes"

    eq <- ifelse(spec$VaryBy[1] == "None", spec$Trans_Variable[1], paste(spec$VaryBy[1], spec$Trans_Variable[1], sep = ":"))
    for (k in 2:nrow(spec)) {
      tmp <- ifelse(spec$VaryBy[k] == "None", spec$Trans_Variable[k], paste(spec$VaryBy[k], spec$Trans_Variable[k], sep = ":"))
      eq <- paste(eq, tmp, sep = "+")
    }

    eq <- paste(eq, " -1")
    eq <- paste(DepVar, " ~ ", eq)
    eq <- stats::as.formula(eq)

    print("calling my_bayes()...")
    bayes_mod_obj_inter <- my_bayes(formula = eq, data = x)
    mod_matrix <- stats::model.matrix(eq, x)

    b <- bayes_mod_obj_inter$coefficients
    b$Variables <- row.names(t(mod_matrix))
    priors <- b
    priors$Prior_Mean <- 0
    priors$Prior_SD <- big_number
    # first populate the priors with user defined priors in the Variables.csv
    for (i in 1:nrow(spec)) {
      v <- spec$Trans_Variable[i]
      priors$Prior_Mean[grep(v, priors$Variables)] <- spec$Prior_Mean[i]
      priors$Prior_SD[grep(v, priors$Variables)] <- spec$Prior_SD[i]
    }

    hb_var <- spec$Trans_Variable[spec$VaryBy != "None" & spec$Orig_Variable != "Intercept"]
    #  hb_var <- spec$Trans_Variable[spec$Orig_Variable != "Intercept"]

    if ("PriorSD_Adj" %in% names(spec)) {
      spec$PriorSD_Adj <- abs(spec$PriorSD_Adj)
    } else {
      spec$PriorSD_Adj <- 1
    }

    if (length(hb_var) >= 1) {
      for (j in 1:length(hb_var)) {
        # if override == "N", calculate emperical prior and populate the priors dataframe
        if (toupper(spec$Override[spec$Trans_Variable == hb_var[j]]) == "N") {
          tmp <- priors$Estimate[grep(hb_var[j], priors$Variables)]
          if (spec$Sign[spec$Trans_Variable == hb_var[j]] > 0) {
            if (is.nan(mean(tmp[tmp > 0]))) {
              cat("The code is not able to find a emperical prior for", hb_var[j], "\n")
              cat("A diffuse prior is used. If you want an informative prior, you can override.\n")
            } else {
              priors$Prior_Mean[grep(hb_var[j], priors$Variables)] <- mean(tmp[tmp > 0])
            }
          } else if (spec$Sign[spec$Trans_Variable == hb_var[j]] > 0) {
            if (is.nan(mean(tmp[tmp < 0]))) {
              cat("The code is not able to find a emperical prior for", hb_var[j], "\n")
              cat("A diffuse prior is used. If you want an informative prior, you can override.\n")
            } else {
              priors$Prior_Mean[grep(hb_var[j], priors$Variables)] <- mean(tmp[tmp < 0])
            }
          } else {
            priors$Prior_Mean[grep(hb_var[j], priors$Variables)] <- mean(tmp)
          }
          # for now let's set the prior_sd = prior_mean*PriorSD_Adj. The PriorSD_Adj defauls to 1 in the _Variables.csv

          priors$Prior_SD[grep(hb_var[j], priors$Variables)] <-
            priors$Prior_Mean[grep(hb_var[j], priors$Variables)] * spec$PriorSD_Adj[spec$Trans_Variable == hb_var[j]]
          if (sum(priors$Prior_SD[grep(hb_var[j], priors$Variables)]) == 0) {
            priors$Prior_SD[grep(hb_var[j], priors$Variables)] <- big_number
          }
        }
      }
    }

    order_of_var <- row.names(t(mod_matrix))
    a <- priors
    a$Variables <- factor(a$Variables, levels = order_of_var)
    a <- dplyr::arrange(a, Variables)
    a$Variables <- as.character(a$Variables)
    priors <- a

    #call to my_bayes
    bayes_mod_obj <- my_bayes(formula = eq, data = x, priors = priors)

    varyby <- unique(mod_obj$spec$VaryBy)[unique(mod_obj$spec$VaryBy) != "None"]
    if (length(varyby) > 0) {
      b <- bayes_mod_obj$coefficients %>%
        tidyr::separate(Variables, into = c(varyby, "Variables"), sep = "\\.", fill = "left")
      b[[varyby]] <- gsub(varyby, "", b[[varyby]])

      tmp <- data.frame(varyby = unique(mod_obj$data[[varyby]]))
      names(tmp) <- varyby

      full_b <- expand.grid(
        varyby = tmp[[varyby]],
        Variables = mod_obj$spec$Trans_Variable[mod_obj$spec$Variable_Type != "Dependent"]
      )
      names(full_b)[1] <- varyby

      full_b$Variables <- as.character(full_b$Variables)
      full_b[[varyby]] <- as.character(full_b[[varyby]])
      full_b <- dplyr::left_join(full_b, b)
      v <- unique(full_b$Variables[is.na(full_b$Estimate)])
      if (length(v) > 0) {
        for (j in 1:length(v)) {
          full_b$Estimate[full_b$Variable == v[j]] <- b$Estimate[b$Variables == v[j]]
          full_b$Error[full_b$Variable == v[j]] <- b$Error[b$Variables == v[j]]
        }
      }
      full_b$Tvalue <- full_b$Estimate / full_b$Error
    } else {
      varyby <- mod_obj$CS
      tmp <- data.frame(varyby = unique(mod_obj$data[[varyby]]))
      names(tmp) <- varyby
      full_b <- expand.grid(
        varyby = tmp[[varyby]],
        Variables = mod_obj$spec$Trans_Variable[toupper(mod_obj$spec$Variable_Type) != "DEPENDENT"]
      )
      full_b$varyby <- as.character(full_b$varyby)
      full_b$Variables <- as.character(full_b$Variables)
      names(full_b)[1] <- varyby

      full_b <- dplyr::left_join(full_b, bayes_mod_obj$coefficients)
    }

    bayes_mod_obj$coefs <- bayes_mod_obj$coefficients
    bayes_mod_obj$coefficients <- full_b
    mod_obj$Model <- bayes_mod_obj
    mod_obj$Model$mod_matrix <- mod_matrix
    mod_obj$Model$eq <- eq
    mod_obj$Model$eq_lm <- eq_lm

    mod_obj$Model$act_pred <- act_pred(mod_obj)
    mod_obj$Model_interLM <- bayes_mod_obj_inter
  }else{
    stop("method must be one of linear_regression, bayesian_linear_regression")
  }


  if(length(IV) > 2){
    mod_obj$Model$VIF <- car::vif(lmModel2)
  }

  mod_obj$Model$DW <- broom::tidy(car::durbinWatsonTest(lmModel2))
  mod_obj$Model$method <- method

  return(mod_obj)
}





