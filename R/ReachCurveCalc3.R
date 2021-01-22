#' ReachCurveCalc3
#'
#' @param reach_curve_spec
#' @param grps_data
#' @param fit_curves
#'
#' @export


ReachCurveCalc3 = function(reach_curve_spec, grps_data, fit_curves){

  all_reach =
    map(1:nrow(reach_curve_spec), ~generate_reach(reach_curve_spec[.x,], grps_data, fit_curves)) %>%
    bind_cols()

  return(all_reach)

}
