#' effective_grps_aggregate
#'
#' @param grps_vec - grps_vec
#' @param begin - begin
#' @param window - window
#' @param decay - decay
#' @param simple - simple
#'
#' @export


effective_grps_aggregate = function(grps_vec, begin, window, decay, simple = TRUE){

  mat = matrix(c(1:length(grps_vec), grps_vec), nrow = length(grps_vec))
  colnames(mat) = c("week_num", "grps")
  mat = mat[mat[,"week_num"] %in% 1:(begin + window - 1),]

  if(is.matrix(mat)){
    decay_factor = (1-decay)^(max(mat[,"week_num"])- mat[,"week_num"])
    teGRPs = sum(mat[,"grps"]*decay_factor)
    week_num = max(mat[,"week_num"])
  }else if(is.vector(mat)){
    decay_factor = (1-decay)
    teGRPs = mat["grps"]*decay_factor
    week_num = mat["week_num"]
  }

  if(simple){
    res = teGRPs
  }else{
    res = data.frame(week_num = week_num, teGRPs = teGRPs)
  }

  return(res)
}
