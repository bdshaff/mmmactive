#' Load_FMIData
#'
#' @param obj - model object
#' @param input_file_FMIData
#'

Load_FMIData = function(obj, input_file_FMIData){
  FMI =
    read.csv(input_file_FMIData, stringsAsFactors = FALSE) %>%
    collect(n = Inf) %>%
    filter(region == "100") %>%
    filter(vehicle %in% c("alt", "arm", "fro", "lef", "max", "mur",
                          "nv", "pth", "rge", "sen", "ttn", "ver")) %>%
    mutate(week = ymd(week))

  Ftable =
    data.table::dcast(FMI, vehicle + week ~ var + type + tier,
                      value.var = "value",
                      fun.aggregate = sum) %>% as.data.frame(.)

  Ftable[is.na(Ftable)]  = 0

  obj$Ftable = Ftable
  return(obj)
}
