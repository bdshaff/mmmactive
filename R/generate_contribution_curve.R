#' generate_contribution_curve
#'
#' @param reach_curve_spec_row
#' @param reach_curve
#'
#' @export

generate_contribution_curve = function(reach_curve_spec_row, reach_curve){
  contribution <- as.integer(reach_curve_spec_row$contribution)
  contribution_percent <- c((lag(reach_curve)/reach_curve)[1:21],(reach_curve/lag(reach_curve))[22:101])
  contribution_curve = contribution_percent
  contribution_curve[21] = contribution

  for (x in 21:1) {
    contribution_curve[x - 1] <- contribution_percent[x] * contribution_curve[x]
  }

  for (x in 22:length(contribution_percent)) {
    contribution_curve[x] <- contribution_percent[x] * contribution_curve[x - 1]
  }
  return(contribution_curve)
}
