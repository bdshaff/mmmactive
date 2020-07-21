#' my_bayes
#'
#' @param formula - formula
#' @param data - data
#' @param priors - priors
#' @export

my_bayes <- function(formula, data, priors = NULL) {
  big_number <- 10000 # for difuse priors

  DepVar <- all.vars(formula)[1]
  IV <- all.vars(formula)[-1]

  y <- data[[DepVar]]

  formula <- as.formula(formula)
  X <- model.matrix(formula, data = data)
  Xdf <- data.frame(X)
  IV <- names(Xdf)

  if (is.null(priors)) {
    print("priors is not specified. The priors will be set to difuse priors.")
    priors <- data.frame(IV, rep(0, length(IV)), rep(sqrt(big_number), length(IV)))
    names(priors) <- c("Trans_Variable", "Prior_Mean", "Prior_SD")
    priors$Prior_Mean <- as.numeric(priors$Prior_Mean)
    priors$Prior_SD <- as.numeric(priors$Prior_SD)
  }

  # setup diffuse priors and run the model to estimate the model error (sigma2)
  betabar <- rep(0, ncol(X))
  sig2 <- 1
  sd2 <- rep(big_number, ncol(X))
  B <- diag(sd2, ncol(X))
  A <- sig2 * diag(1 / sd2, ncol(X))

  ivar <- ncol(X)
  U <- chol(A) # cholesky root of precision matrix
  W <- rbind(X, U) # combine the U matrix with design matrix
  nu <- c(y, (U %*% betabar))
  WpW <- t(W) %*% W
  IWpW <- solve(WpW, tol = 1e-25)
  btilde <- IWpW %*% t(W) %*% nu # basicaly OLS

  yhat <- X %*% btilde
  e2 <- sum((yhat - y)^2)
  sig2_hat <- e2 / (nrow(X) - ncol(X)) # this is the estimated sigma squared

  # use the estimate model error to run the final bayes model
  betabar <- priors$Prior_Mean
  sig2 <- sig2_hat
  sd2 <- priors$Prior_SD^2
  B <- diag(sd2, ncol(X))
  A <- sig2 * diag(1 / sd2, ncol(X))

  ivar <- ncol(X)
  U <- chol(A) # cholesky root of precision matrix
  W <- rbind(X, U) # combine the U matrix with design matrix
  nu <- c(y, (U %*% betabar))
  WpW <- t(W) %*% W
  IWpW <- solve(WpW, tol = 1e-25)
  btilde <- IWpW %*% t(W) %*% nu
  var_cov <- sig2 * solve(t(X) %*% X + A, tol = 1e-25) # this is the error
  error <- sqrt(diag(var_cov))
  tvalue <- btilde / error
  fitted_value <- X %*% btilde
  residuals <- y - fitted_value
  SS_tot <- sum((y - mean(y))^2)
  #  SS_res <- sum((residuals)^2)
  SS_res <- t(residuals) %*% residuals
  sigma <- sqrt(SS_res / (nrow(X) - ncol(X)))
  R2 <- 1 - (SS_res / SS_tot)

  coefficients <- data.frame(cbind(as.vector(btilde), as.vector(error)))
  names(coefficients) <- c("Estimate", "Error")
  coefficients$Tvalue <- coefficients$Estimate / coefficients$Error

  coefficients$Variables <- IV
  coefficients <- coefficients[, c("Variables", "Estimate", "Error", "Tvalue")]

  obj <- list(
    formula = formula, coefficients = coefficients,
    fitted_value = as.vector(fitted_value),
    residuals = as.vector(residuals),
    sigma = sigma,
    R2 = R2
  )

  class(obj) <- "mmmodelr"
  return(obj)
}
