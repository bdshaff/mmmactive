#' generate_reach
#'
#' @param reach_curve_spec_row
#' @param grps_data
#' @param fit_curves
#'
#' @export

generate_reach = function(reach_curve_spec_row, grps_data, fit_curves){

  nrow(grps_data) == 104
  curve_name = str_c(as.character(reach_curve_spec_row$Model),"_",as.character(reach_curve_spec_row$Orig_Variable))
  grps_vector = grps_data[1:104, curve_name][[1]]

  reach_curve = generate_reach_curve(reach_curve_spec_row, grps_vector, fit_curves)
  contribution_curve = generate_contribution_curve(reach_curve_spec_row, reach_curve)
  spend_range = reach_curve_spec_row$spend * seq(from = 0, to = 5, by = 0.05)

  res = data.frame(spend_range, reach_curve, contribution_curve)
  names(res) <- str_c(curve_name, " ", c("Spend", "Reach", "Contribution"))
  print(paste(curve_name, "done"))

  return(res)

}
