#' effective_grps_window
#'
#' @param grps_vec - grps_vec
#' @param begin - begin
#' @param window - window
#' @param decay - decay
#' @param simple - simple
#'
#' @export


effective_grps_window = function(grps_vec, begin, window, decay, simple = TRUE){

  mat = matrix(c(1:length(grps_vec), grps_vec), nrow = length(grps_vec))
  colnames(mat) = c("week_num", "grps")
  mat = mat[mat[,"week_num"] %in% c(begin:(begin + window - 1)),]
  decay_factor = (1-decay)^(window - 1:window)

  if(is.matrix(mat)){
    eGRPs = sum(mat[,"grps"]*decay_factor)
    week_num = max(mat[,"week_num"])
  }else if(is.vector(mat)){
    eGRPs = mat["grps"]*decay_factor
    week_num = mat["week_num"]
  }

  if(simple){
    res = eGRPs
  }else{
    res = data.frame(week_num = week_num, eGRPs = eGRPs)
  }

  return(res)
}
