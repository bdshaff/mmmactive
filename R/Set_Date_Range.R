#' Set_Date_Range
#'
#' Set the data range for the mod_obj before model fitting
#' Uses the BeginData and EndDate attributes of the mod_obj to set the range on the transformed data.
#'
#' @param mod_obj - model object
#' @param na.rm - logical
#'
#' @return mod_obj
#' @export

Set_Date_Range <- function(mod_obj, na.rm = TRUE) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.activated(mod_obj)) {
    stop("mod_obj must be activated.")
  }
  if (!is.data_transformed(mod_obj)) {
    stop("mod_obj must have transformed data to set date range.")
  }

  mod_obj$data <-
    mod_obj$data %>%
    dplyr::filter(
      !!rlang::sym(mod_obj$Time) >= mod_obj$BeginDate,
      !!rlang::sym(mod_obj$Time) <= mod_obj$EndDate
    )

  if (na.rm) {
    mod_obj$data[is.na(mod_obj$data)] <- 0
  }

  return(mod_obj)
}
