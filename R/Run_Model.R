#' Run_Model
#'
#' @param obj - model object
#' @param Method - method
#'

Run_Model <- function(obj, Method = "Bayes") {
  spec <- obj$spec
  x <- obj$data

  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]
  IV <- spec$Trans_Variable[spec$Variable_Type != "Dependent"]
  spec <- spec[spec$Variable_Type != "Dependent", ]
  priors <- dplyr::select(spec, Orig_Variable, Trans_Variable, Prior_Mean, Prior_SD)

  if (!is.null(obj$cs)) {
    cs <- obj$cs
  }

  if (!is.null(obj$Time)) {
    ts <- obj$Time
  }

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
      dplyr::setdiff(IV, names(x)[IV %in% names(x)]),
      " are not in the model x set. Please check _Variables.csv file. "
    )
  }

  eq <- paste(IV, collapse = " + ")
  eq <- paste(eq, " - 1")
  eq <- paste(DepVar, " ~ ", eq)
  eq <- as.formula(eq)

  eq_lm <- paste(IV[!IV %in% "Intercept"], collapse = " + ")
  eq_lm <- paste(eq_lm, " + ", cs)
  eq_lm <- paste(DepVar, " ~ ", eq_lm)
  eq_lm <- as.formula(eq_lm)


  print("calling my_bayes()...")
  bayes_obj <- my_bayes(formula = eq, data = x, priors = priors)
  obj$Model <- bayes_obj
  obj$Model$act_pred <- x[, c(obj$Time, DepVar)]
  obj$Model$act_pred$predicted <- obj$Model$fitted_value
  obj$Model$act_pred$residual <- obj$Model$residuals
  obj$Model$MAPE <- MAPE(obj$Model$act_pred[[DepVar]], obj$Model$act_pred$predicted)



  obj$lmModel <- lm(eq_lm, data = x)
  obj$Model$VIF <- data.frame(vif(obj$lmModel))
  obj$Model$VIF <- tibble::rownames_to_column(obj$Model$VIF, var = "Variables")
  obj$Model$result_all <- full_join(obj$Model$VIF, obj$Model$coefficients)
  obj$Model$result_all <- obj$Model$result_all[, c("Variables", "GVIF", "Estimate", "Error", "Tvalue")]
  names(obj$Model$result_all)[names(obj$Model$result_all) == "Variables"] <- "Trans_Variable"
  obj$Model$result_all <- full_join(spec, obj$Model$result_all)

  obj$Model$DW <- durbinWatsonTest(obj$lmModel)
  obj$Model$result_all$R2 <- rep(obj$Model$R2, nrow(obj$Model$result_all))
  obj$Model$result_all$DW <- rep(obj$Model$DW$dw, nrow(obj$Model$result_all))
  #  obj$Model$DWtest <- (dwtest(obj$Model$formula, data=obj$data))

  return(obj)
}
