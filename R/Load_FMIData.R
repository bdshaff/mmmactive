#' Load_FMIData
#'
#' This function loads FMI data onto a mod_obj.
#'
#' @param mod_obj - model mod_object
#' @param input_file_FMIData - path to FMI data file
#'
#' @return mod_obj
#'

Load_FMIData <- function(mod_obj, input_file_FMIData) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.load_data_ready(mod_obj)) {
    stop("mod_obj missing group selector needed to load data.")
  }

  FMI <-
    read.csv(input_file_FMIData, stringsAsFactors = FALSE) %>%
    collect(n = Inf) %>%
    filter(region == "100") %>%
    filter(vehicle %in% c(
      "alt", "arm", "fro", "lef", "max", "mur",
      "nv", "pth", "rge", "sen", "ttn", "ver"
    )) %>%
    mutate(week = ymd(week))

  Ftable <-
    data.table::dcast(FMI, vehicle + week ~ var + type + tier,
      value.var = "value",
      fun.aggregate = sum
    ) %>% as.data.frame(.)

  Ftable[is.na(Ftable)] <- 0

  mod_obj$Ftable <- Ftable
  return(mod_obj)
}
