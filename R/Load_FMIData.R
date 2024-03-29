#' Load_FMIData
#'
#' This function loads FMI data onto a mod_obj.
#'
#' @param mod_obj - model mod_object
#' @param input_file_FMIData - path to FMI data file
#'
#' @return mod_obj
#' @export

Load_FMIData <- function(mod_obj, input_file_FMIData) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.load_data_ready(mod_obj)) {
    stop("mod_obj missing group selector needed to load data.")
  }

  FMI <-
    read.csv(input_file_FMIData, stringsAsFactors = FALSE) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::filter(region == "100") %>%
    dplyr::filter(vehicle %in% c(
      "alt", "arm", "fro", "lef", "max", "mur",
      "nv", "pth", "rge", "sen", "ttn", "ver","kcs"
    )) %>%
    dplyr::mutate(week = lubridate::ymd(week))

  # Ftable <-
  #   reshape2::dcast(FMI, vehicle + week ~ var + type + tier,
  #     value.var = "value",
  #     fun.aggregate = sum
  #   ) %>% as.data.frame(.)

  Ftable <- FMI %>%
    dplyr::select(vehicle, week, var, type, tier, value) %>%
    tidyr::pivot_wider(names_from = c(var, type, tier),
                       values_from = value,
                       values_fn = function(x) sum(x, na.rm = TRUE),
                       values_fill = 0) %>%
    dplyr::arrange(vehicle, week) %>%
    as.data.frame()

  Ftable[is.na(Ftable)] <- 0

  mod_obj$Ftable <- Ftable
  return(mod_obj)
}
