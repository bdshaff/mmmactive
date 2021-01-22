#' generate_reach_curve
#'
#' @param reach_curve_spec_row
#' @param grps_vector
#' @param fit_curves - fit_curves
#'
#' @export

generate_reach_curve = function(reach_curve_spec_row, grps_vector, fit_curves){

  EF <- as.integer(reach_curve_spec_row$Effective)
  RF <- as.integer(reach_curve_spec_row$Recency)
  Period <- as.integer(reach_curve_spec_row$Period)
  Decay <- as.integer(reach_curve_spec_row$Decay)/100
  Channel <- toupper(reach_curve_spec_row$Orig_Variable)

  alpha_beta_gamma = suppressWarnings(get_alpha_beta_gamma(channel_name = Channel, fit_curves = fit_curves))

  percent_range = seq(from = 0, to = 5, by = 0.05)
  grps_matrix_previous_year = matrix(rep(grps_vector[1:52], 101), nrow = 52)
  grps_matrix_current_year = matrix(rep(grps_vector[53:104], 101), nrow = 52)
  percents_matrix = matrix(rep(seq(from = 0, to = 5, by = 0.05),each = 52), nrow = 52)
  ones_matrix = matrix(1, ncol = 101, nrow = 52)
  grps_matrix = as.data.frame(rbind(grps_matrix_previous_year * ones_matrix, grps_matrix_current_year * percents_matrix))
  names(grps_matrix) = percent_range


  start = Sys.time()

  split_total =
    grps_matrix %>%
    mutate(week_num = 1:nrow(grps_matrix)) %>%
    pivot_longer(-week_num, names_to = "percent", values_to = "grps") %>%
    arrange(percent) %>%
    relocate(percent, .before = week_num) %>%
    group_by(percent) %>%
    group_split()

  split_reach_tables = map(split_total, ~get_reach_tables(.x, E = EF, R = RF, window = Period, decay = Decay, alpha_beta_gamma))
  split_reach_tables_clean = keep(split_reach_tables, ~is.data.frame(.x))

  totaladresponse = map(split_reach_tables_clean, ~pull(.x, "reach"))

  end = Sys.time()
  print(end - start)


  TotalAdResponse <- matrix(unlist(totaladresponse), nrow = 104, byrow = FALSE)
  VarReach <- colSums((TotalAdResponse)[53:104, 1:101])
  VarReach <- VarReach - VarReach[[1]]

  return(VarReach)
}
