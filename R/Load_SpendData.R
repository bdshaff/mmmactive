#' Load_SpendData
#'
#' Loads the spend data onto the mod_obj.
#'
#' @param mod_mod_obj - model mod_object
#' @param input_file_SpendData - path to the spend data file
#' @param NAMEPLATE - Nameplate (ex. Titan)
#' @param nmp - Nameplate (ex. ttn)
#'
#' @return mod_obj
#'

Load_SpendData <- function(mod_obj, input_file_SpendData, NAMEPLATE = NULL, nmp = NULL) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }

  Stable <-
    read_csv(input_file_SpendData) %>%
    mutate(Date = mdy(Date)) %>%
    filter(
      Model == NAMEPLATE,
      Date >= "2018-10-01",
      Date <= "2019-09-01"
    ) %>%
    mutate(
      FY = "FY19",
      media_agg = `Media Channel`,
      model_agg = nmp
    ) %>%
    group_by(FY, model_agg, media_agg) %>%
    summarise(Spend = sum(Spend, na.rm = TRUE))

  mod_obj$Stable <- Stable
  return(mod_obj)
}
