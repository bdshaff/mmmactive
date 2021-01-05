#' get_reach_tables
#'
#' @param grps_split - grps_split
#' @param E - E
#' @param R - R
#' @param window - window
#' @param decay - decay
#' @param alpha_beta_gamma - alpha_beta_gamma
#'
#' @export


get_reach_tables = function(grps_split, E, R, window, decay, alpha_beta_gamma){
  grps_vec = grps_split$grps

  begin_range = 1:(length(grps_vec) - window + 1)

  eGRPs = map_dbl(begin_range, ~effective_grps_window(grps_vec, begin = .x, window = window, decay = decay))
  teGRPs = map_dbl(begin_range, ~effective_grps_aggregate(grps_vec, begin = .x, window = window, decay = decay))

  TB = bind_cols(grps_split,
                 eGRPs = c(rep(0, window - 1), eGRPs),
                 teGRPs = c(rep(0, window - 1), teGRPs))

  TB$reach = map2_dbl(TB$eGRPs, TB$teGRPs, ~get_channel_reach(E = E, R = R, eGRPs = .x, teGRPs = .y,
                                                              alpha_beta_gamma = alpha_beta_gamma))

  return(TB)
}
