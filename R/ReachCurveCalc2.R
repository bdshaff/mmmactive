#' ReachCurveCalc2
#'
#' @param Values - Values
#' @param input - input
#' @param fit_curves - fit_curves
#'
#' @export

ReachCurveCalc2 <- function(Values, input, fit_curves){

  AllReach <- list()

  Reach <- compiler::cmpfun(Reach)

  # looking into the Input table (for each channel essentially)
  for (col in 2:ncol(input)) {

    # unwrapping the rows of the input table
    EF <- as.integer(input[4, col])
    RF <- as.integer(input[5, col])
    Period <- as.integer(input[6, col])
    Decay <- as.integer(input[7, col])
    Channel <- as.character(input[11, col])
    Decay <- Decay / 100
    gnObs <- 104
    t <- 1

    ################################################################################################
    ################################################################################################
    ################################################################################################

    alpha_beta_gamma = get_alpha_beta_gamma(channel_name = Channel, fit_curves = fit_curves)


    grps_column <- Values[1:104, col]
    grps_matrix_previous_year = matrix(rep(grps_column[1:52, 1][[1]], 101), nrow = 52)
    grps_matrix_current_year = matrix(rep(grps_column[53:104, 1][[1]], 101), nrow = 52)
    percents_matrix = matrix(rep(seq(from = 0, to = 5, by = 0.05),each = 52), nrow = 52)
    ones_matrix = matrix(1, ncol = 101, nrow = 52)

    total = as.data.frame(rbind(grps_matrix_previous_year * ones_matrix, grps_matrix_current_year * percents_matrix))
    names(total) = seq(from = 0, to = 5, by = 0.05)


    ################################################################################################
    ################################################################################################
    ################################################################################################

    start = Sys.time()

    split_total =
      total %>%
      mutate(week_num = 1:nrow(total)) %>%
      pivot_longer(-week_num, names_to = "percent", values_to = "grps") %>%
      arrange(percent) %>%
      relocate(percent, .before = week_num) %>%
      group_by(percent) %>%
      group_split()

    split_reach_tables = map(split_total, ~get_reach_tables(.x, E = EF, R = RF, window = Period, decay = Decay, alpha_beta_gamma))
    split_reach_tables_clean = keep(split_reach_tables, ~is.data.frame(.x))

    totaladresponse = map(split_reach_tables_clean, ~pull(.x, "reach"))

    end = Sys.time()
    print(end - start)

    ################################################################################################
    ################################################################################################
    ################################################################################################

    TotalAdResponse <- matrix(unlist(totaladresponse), nrow = 104, byrow = FALSE)
    VarReach <- colSums((TotalAdResponse)[53:nrow(TotalAdResponse), 1:ncol(TotalAdResponse)])
    VarReach <- VarReach - VarReach[[1]]


    spend <- as.integer(input[1, col])
    contribution <- as.integer(input[2, col])

    # Find contributions through % of VarReach
    VarReach <- as.data.frame(VarReach)
    VarReach1 <- rbind(0, VarReach)
    VarReach1 <- VarReach1[1:101, ]

    ContrPer <- VarReach1 / VarReach
    ContrPer <- as.data.frame(ContrPer[1:21, ])
    ContrPer1 <- VarReach / VarReach1
    ContrPer1 <- as.data.frame(ContrPer1[22:101, ])
    names(ContrPer) <- "Percent"
    names(ContrPer1) <- "Percent"

    ContrPer <- rbind(ContrPer, ContrPer1)

    ContrPer$Contribution <- NA
    ContrPer$Contribution[[21]] <- contribution

    for (x in 21:1) {
      ContrPer$Contribution[x - 1] <- ContrPer$Percent[[x]] * ContrPer$Contribution[[x]]
    }

    for (x in 22:nrow(ContrPer)) {
      ContrPer$Contribution[x] <- ContrPer$Percent[[x]] * ContrPer$Contribution[x - 1]
    }

    # Calculate Spend
    Spend <- t(Per * spend)
    Final <- cbind(Spend, VarReach, ContrPer$Contribution)
    names(Final) <- c("Spend", "Reach", "Contribution")
    test <- list(Final)
    print(paste(names(Values)[col], "done"))


    AllReach[[col]] <- test
  }

  AllReach <- data.frame(matrix(unlist(AllReach), nrow = 101, byrow = FALSE))
  # Name the reach curves
  names <- names(Values)[-1]
  names <- rep(names, each = 3)

  testing <- matrix(names, ncol = 3, byrow = TRUE)
  names1 <- as.data.frame(testing)
  names1$Spend <- paste(testing[, 1], "Spend", sep = " ")
  names1$Reach <- paste(testing[, 2], "Reach", sep = " ")
  names1$Contribution <- paste(testing[, 3], "Contribution", sep = " ")
  names1 <- select(names1, Spend:Contribution)
  names1 <- as.matrix(t(names1))
  names1 <- as.vector(names1)
  names(AllReach) <- names1
  return(AllReach)
}
