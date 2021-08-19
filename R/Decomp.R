#' Model Decomposition and Contribution Calculations
#'
#' This Decomp function is compatible with the Bayesian models fitted with the MSMP framework
#' The format of the output is compatible with the legacy reach generation function.
#'
#' @param mod_obj - model object
#' @param min_ref_var_names - vector of variable indexes
#' @param mean_ref_var_names - vector of variable indexes
#' @param method - one of "top_down" or "bottom_up"
#'
#' @importFrom magrittr %>%
#' @return mod_obj
#' @export

Decomp = function(mod_obj, min_ref_var_names = NULL, mean_ref_var_names = NULL, method = "top_down") {
  if(method == "top_down"){
    mod_obj <- top_down_decomp(mod_obj,
                               min_ref_var_names = min_ref_var_names,
                               mean_ref_var_names = mean_ref_var_names)
  }else if(method == "bottom_up"){
    mod_obj <- bottom_up_decomp(mod_obj,
                                min_ref_var_names = min_ref_var_names,
                                mean_ref_var_names = mean_ref_var_names)
  }else{
    stop("Decomp method must be one of (top_down,bottom_up)")
  }
  return(mod_obj)
}
