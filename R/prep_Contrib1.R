#' prep_Contrib1
#'
#' @param Contrib - Contrib
#' @param spend - spend
#' @param year - year
#' @param panel - panel
#'
#' @export


prep_Contrib1 = function(Contrib, spend, year, panel){

  # Warange the contrib table


  Contrib1 <-
    Contrib %>%
    gather("Year", "Contributions", 5:ncol(.)) %>%
    filter(Year == year) %>%
    group_by(variable, Categories, Model, Year) %>%
    summarise(Contributions = sum(Contributions, na.rm = TRUE)) %>%
    mutate(TMP = 1) %>%
    filter(grepl("_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])", variable) | grepl("_([D][0-9][0-9][P][0-9])", variable)) %>%
    arrange(variable) %>%
    mutate(variable1 = variable)


  Contrib1a <-
    Contrib1 %>%
    filter(grepl("_([D][0-9][0-9][P][0-9])", variable)) %>%
    mutate(Type = "L")

  stockvars <- Contrib1a$variable
  Contrib1a$variable1 <- tolower(stockvars)

  Contrib1b <-
    Contrib1 %>%
    filter(grepl("_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])", variable)) %>%
    mutate(Type = "R")

  Contrib1 <- rbind(Contrib1a, Contrib1b)

  spend <-
    spend %>% filter(model_agg == panel | model_agg == toupper(panel))

  #adding spend to contrib table
  Contrib1 <- inner_join(Contrib1, spend, by = c("Categories" = "media_agg"))
  Contrib1 <- Contrib1 %>%
    filter(!is.na(Spend)) %>%
    filter(Contributions > 0)

  # Parameters
  param1 <- "_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])" # formatting for adresponse
  param2 <- "_([D][0-9][0-9][P][0-9])" # formatting for adstock
  param3 <- "_([d][0-9][0-9][p][0-9])" # formatting for adstock


  Allparams <-
    Contrib1$variable1 %>%
    str_replace_all(param1, "") %>%
    str_replace_all(param3, "") %>%
    str_replace_all(., c("_log" = "", "PANEL_[A-Z][A-Z][A-Z]_M_" = "")) %>%
    tolower(.) %>%
    str_replace_all(paste(panel, "_", sep = ""), "")

  Contrib1$LU <- Allparams


  # Parameters
  Contrib1$E <- str_extract(Contrib1$variable1, "([E][0-9])")
  Contrib1$E <- str_replace(Contrib1$E, "E", "")
  Contrib1$R <- str_extract(Contrib1$variable1, "([R][0-9])")
  Contrib1$R <- str_replace(Contrib1$R, "R", "")
  Contrib1$P <- str_extract(Contrib1$variable1, "[P][0-9][0-9]")
  Contrib1$P <- str_replace(Contrib1$P, "P", "")
  Contrib1$P <- as.integer(Contrib1$P)
  Contrib1$P <- Contrib1$P / 7
  Contrib1$D <- str_extract(Contrib1$variable1, "[D][0-9][0-9]")
  Contrib1$D <- str_replace(Contrib1$D, "D", "")
  Contrib1$p <- str_extract(Contrib1$variable1, "[p][0-9]")
  Contrib1$p <- str_replace(Contrib1$p, "p", "")
  Contrib1$d <- str_extract(Contrib1$variable1, "[d][0-9][0-9]")
  Contrib1$d <- str_replace(Contrib1$d, "d", "")


  Contrib1$TableName <- paste(Contrib1$Model, Contrib1$variable, sep = "_")
  Contrib1 <- Contrib1 %>% arrange(TableName)

  return(Contrib1)

}
