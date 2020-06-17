#' Load_SpendData
#'
#' @param obj - model object
#' @param input_file_SpendData
#' @param NAMEPLATE
#' @param nmp
#'

Load_SpendData <- function(obj, input_file_SpendData, NAMEPLATE = NULL, nmp = NULL){
  Stable =
    read_csv(input_file_SpendData) %>%
    mutate(Date = mdy(Date)) %>%
    filter(Model == NAMEPLATE,
           Date >= "2018-10-01",
           Date <= "2019-09-01") %>%
    mutate(FY = "FY19",
           media_agg = `Media Channel`,
           model_agg = nmp) %>%
    group_by(FY, model_agg, media_agg) %>%
    summarise(Spend = sum(Spend, na.rm = TRUE))

  obj$Stable = Stable
  return(obj)
}



