#' Reach
#'
#' @param fa - fa
#' @param fb - fb
#' @param fc - fc
#' @param fGRPs - fGRPs
#'
#' @export

Reach = function(fa, fb, fc, fGRPs) {
  # Return reach data subject to fitted formula to a value r=a/(1+b*(GRPs/1000)^c)

  # fa = Alpha Coefficient in reach model fb = Beta Coefficient in reach model fc = Gamma Coefficient in reach model
  # fGRPs = single data point of GRPs at which to calculate reach

  fReach <- as.numeric(fGRPs > 0) * fa / (1 + fb * (fGRPs / 1000)^fc)
  # Return calculated reach value
  return(fReach)

  # Example Use of Reach Function (solution=1.222065) test=Reach(0.79,-1,0.5,125)
}
