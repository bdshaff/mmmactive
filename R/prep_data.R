#' prep_data
#'
#' @param fmi_data - fmi_data
#' @param From - From
#' @param To - To
#' @param Allparams - Allparams
#' @param Contrib1 - Contrib1
#' @param panel - panel
#'
#' @export

prep_data = function(fmi_data, From, To, Allparams, Contrib1, panel){
  #formatting a dataset with 104 weeks of GRPs / Impressions data (not contributions)

  # Timeframe
  timeframe <- as.data.frame(seq.Date(from = as.Date(From), to = as.Date(To), by = "week"))
  names(timeframe) <- "week"
  if (length(timeframe) > 104) {
    stop("You need to select only 104 weeks")
  }

  # Select Data
  AllData <- as.data.frame(fmi_data[, colnames(fmi_data) %in% Allparams]) # grab columns to adresponse
  AllData <- cbind(select(fmi_data, vehicle, week), AllData)
  AllData <- AllData %>% filter(vehicle == panel)
  AllData <- gather(AllData, "Category", "Value", 3:ncol(AllData))

  DataJoin <- left_join(Contrib1[, c("variable", "Categories", "Model", "LU")], AllData, by = c("LU" = "Category"))
  Data <- dcast(DataJoin, week ~ Model + variable, fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "Value")

  # 104 Weeks
  Data <- left_join(timeframe, Data)
  names(Data)[1] <- "Week"
  Data$Week <- NULL
  neg_num <- as.data.frame(paste("Week", seq(from = -52, to = -1)))
  names(neg_num) <- "Week"
  pos_num <- as.data.frame(paste("Week", seq(from = 1, to = 52)))
  names(pos_num) <- "Week"
  Week <- rbind(neg_num, pos_num)
  Data <- cbind(Week, Data)

  Data <- Data %>%
    mutate(Week = as.character(Week)) %>%
    as_tibble()

  return(Data)

}
