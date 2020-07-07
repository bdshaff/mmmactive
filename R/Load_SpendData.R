#' Load_SpendData
#'
#' Loads the spend data onto the mod_obj.
#'
#' @param mod_obj - model mod_object
#' @param input_file_SpendData - path to the spend data file
#'
#' @return mod_obj
#'

Load_SpendData <- function(mod_obj, input_file_SpendData) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.load_data_ready(mod_obj)) {
    stop("mod_obj missing group selector needed to load data.")
  }

  NAMEPLATE <- mod_obj$NAMEPLATE
  nmp <- mod_obj$nmp

  Stable <-
    readr::read_csv(input_file_SpendData) %>%
    dplyr::mutate(Date = mdy(Date)) %>%
    dplyr::filter(
      Model == NAMEPLATE,
      Date >= "2018-10-01",
      Date <= "2019-09-01"
    ) %>%
    dplyr::mutate(
      FY = "FY19",
      media_agg = `Media Channel`,
      model_agg = nmp
    ) %>%
    dplyr::group_by(FY, model_agg, media_agg) %>%
    dplyr::summarise(Spend = sum(Spend, na.rm = TRUE))

  mod_obj$Stable <- Stable
  return(mod_obj)
}
