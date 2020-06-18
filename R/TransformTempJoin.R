#' TransformTempJoin
#'
#' @param data_input
#' @param DFFS
#' @param trans_variable
#'

TransformTempJoin <- function(data_input, DFFS, trans_variable) {
  DF <- list()
  for (tmp in names(data_input)) {
    DF[[tmp]] <- data_input[[tmp]] %>% bind_cols(ungroup(DFFS[[tmp]]) %>% select(one_of(trans_variable)))
    cat(str_c("The ", tmp, " level data transformed and augmented \n"))
  }

  weeklyDF <-
    DF$weekly %>%
    mutate(month = floor_date(week, unit = "month")) %>%
    group_by(vehicle, region, month) %>%
    select(one_of(trans_variable)) %>%
    summarise_all(.funs = function(x) mean(x, na.rm = TRUE)) %>%
    group_by(vehicle, region, month)


  monthlyDF <-
    DF$monthly %>%
    mutate(month = floor_date(month, unit = "month")) %>%
    group_by(vehicle, region, month) %>%
    select(one_of(trans_variable)) %>%
    summarise_all(.funs = function(x) sum(x, na.rm = TRUE)) %>%
    group_by(vehicle, region, month)

  joinedDF <- monthlyDF %>% full_join(weeklyDF)

  cat(str_c(names(data_input), " data releveled and joined \n"))

  return(joinedDF)
}
