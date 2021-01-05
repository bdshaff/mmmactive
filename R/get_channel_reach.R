#' get_channel_reach
#'
#' @param obj - mod_obj
#'
#' @export

get_channel_reach = function(E, R, eGRPs, teGRPs, alpha_beta_gamma){
  Alpha = alpha_beta_gamma$Alpha
  Beta = alpha_beta_gamma$Beta
  Gamma = alpha_beta_gamma$Gamma

  reach_value = 0
  if(R == 0 | E == 0){
    return(reach_value)
  }else if((R < E) & (teGRPs - eGRPs == 0)){
    v1 = Reach(Alpha(E), Beta(E), Gamma(E), teGRPs)
    v2 = Reach(Alpha(R), Beta(R), Gamma(R), eGRPs)
    if (v1 > v2) {
      reach_value  <- v2
    }else{
      reach_value  <- v1
    }
  }else if(R < E){
    reach_value  <- 0
    for (k in R:(E - 1)) {
      reach_value  <- reach_value +
        (
          Reach(Alpha(k), Beta(k), Gamma(k), eGRPs) -
            Reach(Alpha(k + 1), Beta(k + 1), Gamma(k + 1), eGRPs)
        ) *
        Reach(Alpha(E - k), Beta(E - k), Gamma(E - k), teGRPs - eGRPs) / 100
    }
    reach_value  <- reach_value  + Reach(Alpha(E), Beta(E), Gamma(E), eGRPs)
  }else if(R == E){
    reach_value  <- Reach(Alpha(R), Beta(R), Gamma(R), eGRPs)
  }
  return(reach_value)
}
