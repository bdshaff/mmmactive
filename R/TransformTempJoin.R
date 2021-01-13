#' TransformTempJoin
#'
#' @param data_input - list of weekly and monthly data.frames
#' @param DFFS - list of split data.frames
#' @param trans_variable - variable
#'
#' @export

TransformTempJoin <- function(data_input, DFFS, trans_variable) {
  DF <- list()
  for (tmp in names(data_input)) {
    DF[[tmp]] <- data_input[[tmp]] %>% dplyr::bind_cols(dplyr::ungroup(DFFS[[tmp]]) %>% dplyr::select(tidyselect::one_of(trans_variable)))
    cat(str_c("The ", tmp, " level data transformed and augmented \n"))
  }

  weeklyDF <-
    DF$weekly %>%
    dplyr::mutate(month = floor_date(week, unit = "month")) %>%
    dplyr::group_by(nameplate, region, month) %>%
    dplyr::select(tidyselect::one_of(trans_variable)) %>%
    dplyr::summarise_all(.funs = function(x) mean(x, na.rm = TRUE)) %>%
    dplyr::group_by(nameplate, region, month)


  monthlyDF <-
    DF$monthly %>%
    dplyr::mutate(month = floor_date(month, unit = "month")) %>%
    dplyr::group_by(nameplate, region, month) %>%
    dplyr::select(tidyselect::one_of(trans_variable)) %>%
    dplyr::summarise_all(.funs = function(x) sum(x, na.rm = TRUE)) %>%
    dplyr::group_by(nameplate, region, month)

  joinedDF <- monthlyDF %>% dplyr::full_join(weeklyDF)

  cat(str_c(names(data_input), " data releveled and joined \n"))

  return(joinedDF)
}
