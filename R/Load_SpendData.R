#' Load_SpendData
#'
#' Loads the spend data onto the mod_obj.
#'
#' @param mod_obj - model mod_object
#' @param input_file_SpendData - path to the spend data file
#'
#' @return mod_obj
#' @export

Load_SpendData <- function(mod_obj, input_file_SpendData) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.load_data_ready(mod_obj)) {
    stop("mod_obj missing group selector needed to load data.")
  }

  NAMEPLATE <- mod_obj$NAMEPLATE
  nmp <- mod_obj$nmp

  BEGINDATE <- mod_obj$BeginDate
  ENDDATE <- mod_obj$EndDate

  BigStable <-
    readr::read_csv(input_file_SpendData) %>%
    dplyr::mutate(Date = mdy(Date)) %>%
    dplyr::filter(
      Model == NAMEPLATE,
      Date >= BEGINDATE,
      Date <= ENDDATE
    ) %>%
    dplyr::mutate(
      FY = year(Date - months(3)),
      media_agg = Categorization,
      model_agg = nmp
    )

  Stable <-
    BigStable %>%
    dplyr::group_by(FY, model_agg, media_agg) %>%
    dplyr::summarise(Spend = sum(Spend, na.rm = TRUE))

  mod_obj$spend_data <- spend_data
  mod_obj$Stable <- Stable
  return(mod_obj)
}
