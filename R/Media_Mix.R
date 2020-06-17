#' Media_Mix
#'
#' @param mod_obj - model object
#' @param total_budget - total budget
#' @param incremental
#' @param interactive
#'

Media_Mix <- function(mod_obj, total_budget = NULL, incremental = 100000, interactive = FALSE) {

  ######### Need to generalize this path #########
  RC <- mod_obj$ABClist
  ABCs <- RC$ABCs[,-which(RC$ABCs[1,] < 1)]
  multiplier <- pull(filter(mod_obj$Mtable, FiscalPeriod == "FY18"), MSRP)
  #multiplier <- pull(filter(mod_obj$Mtable, FiscalPeriod == FiscalPeriod), MSRP)

  # To avoid scientific notation
  options("scipen" = 100, "digits" = 4)

  df_orig <- ABCs %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Channel") %>%
    select(Channel, `Adjusted A`, B, C, Spend, mROI)

  print("This is how the mROIs look with current Spend level")
  show(df_orig)

  df_opt <- df_orig %>%
    select(Channel, `Adjusted A`, B, C) %>%
    mutate(
      Min = 1000000,
      Max = 900000000
    )

  if(interactive == TRUE){
    message("Enter Min-Max Constraints in $MMs for each channel as: Min Spend,Max Spend; Ex: 1,900")
    for (i in seq(nrow(df_opt))) {
      min_max <- readline(paste0(df_opt$Channel[i], ": \t"))
      df_opt$Min[i] <- as.numeric(str_split_fixed(min_max, ",", 2)[1]) * 1000000
      df_opt$Max[i] <- as.numeric(str_split_fixed(min_max, ",", 2)[2]) * 1000000
    }
  }

  df_opt <- df_opt %>%
    mutate(
      Spend = Min,
      mROI = (mapply(mmmlegacy::Reach, `Adjusted A`, B, C, Spend + 10000) * multiplier - mapply(mmmlegacy::Reach, `Adjusted A`, B, C, Spend) * multiplier) / 10000
    )

  ######### Use DT to set these up! #########
  # DT::renderDataTable({DT::datatable(df_opt, editable = T)})
  # df_orig$Min <- 1*1000000
  # df_orig$Max <- 5*1000000
  rem_budget <- total_budget - sum(df_opt$Spend)

  print("Optimising...")

  while (rem_budget > incremental) {
    df_opt <- df_opt[order(-df_opt$mROI), ]
    if (df_opt$Spend[1] <= df_opt$Max[1] - incremental) {
      df_opt$Spend[1] <- df_opt$Spend[1] + incremental
    } else {
      df_opt$mROI <- 0
    }

    df_opt <- df_opt %>%
      mutate(mROI = (mapply(mmmlegacy::Reach, `Adjusted A`, B, C, Spend + 10000) * multiplier - mapply(mmmlegacy::Reach, `Adjusted A`, B, C, Spend) * multiplier) / 10000)

    rem_budget <- total_budget - sum(df_opt$Spend)
  }

  # Adding left-over budget to the higher mROI channel
  df_opt <- df_opt[order(-df_opt$mROI), ]
  df_opt$Spend[1] <- df_opt$Spend[1] + rem_budget
  rem_budget <- total_budget - sum(df_opt$Spend)
  # print(rem_budget)

  df_opt <- df_opt %>%
    mutate(mROI = (mapply(mmmlegacy::Reach, `Adjusted A`, B, C, Spend + 10000) * multiplier - mapply(mmmlegacy::Reach, `Adjusted A`, B, C, Spend) * multiplier) / 10000)

  print("Done")

  mod_obj$mmo = list(df_orig = df_orig,
                     df_opt = df_opt[order(df_opt$Channel), ])

  return(mod_obj)

}

