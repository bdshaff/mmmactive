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

Run_Model <- function(mod_obj, method = "linear_regression") {
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

  if (!is.null(mod_obj$cs)) {
    cs <- mod_obj$cs
  }

  if (!is.null(mod_obj$Time)) {
    ts <- mod_obj$Time
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
      setdiff(IV, names(x)[IV %in% names(x)]),
      " are not in the model x set. Please check _Variables.csv file. "
    )
  }

  if(nrow(unique(x[,cs])) > 1){

    eq_lm <- paste(IV[!IV %in% "Intercept"], collapse = " + ")
    eq_lm <- paste(eq_lm, " + ", cs)
    eq_lm <- paste(DepVar, " ~ ", eq_lm)
    eq_lm <- stats::as.formula(eq_lm)

  }else if(nrow(unique(x[,cs])) == 1){

    eq_lm <- paste(IV[!IV %in% "Intercept"], collapse = " + ")
    eq_lm <- paste(DepVar, " ~ ", eq_lm)
    eq_lm <- stats::as.formula(eq_lm)

  }

  mod_obj$Model <- stats::lm(eq_lm, data = x)

  if(length(IV) > 2){
    mod_obj$Model$VIF <- car::vif(mod_obj$Model)
  }

  mod_obj$Model$DW <- broom::tidy(car::durbinWatsonTest(mod_obj$Model))
  mod_obj$Model$method <- method
  mod_obj$Model$eq <- eq_lm

  return(mod_obj)
}





