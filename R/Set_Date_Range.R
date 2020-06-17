#' Set_Date_Range
#'
#' @param mod_obj - model object
#'

Set_Date_Range = function(mod_obj, na.rm = TRUE){
  mod_obj$data =
    mod_obj$data %>%
    filter(!!sym(mod_obj$Time) >= mod_obj$BeginDate,
           !!sym(mod_obj$Time) <= mod_obj$EndDate)

  if(na.rm){
    mod_obj$data[is.na(mod_obj$data)] = 0
  }

  return(mod_obj)
}
