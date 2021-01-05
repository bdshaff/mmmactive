#' Response_Curves
#'
#' @param obj - mod_obj
#' @param Contrib - Contrib
#' @param year - year
#'
#' @export


Response_Curves = function(mod_obj, Contrib, year){

  Contrib = Ctable
  fmi_data = mod_obj$Ftable
  spend = mod_obj$Stable[mod_obj$Stable$FY == year,]
  multiplier = as.numeric(mod_obj$Mtable[mod_obj$Mtable$FY == str_c("FY",str_sub(year,3,4)), "MSRP"])
  fit_curves = mod_obj$fit_curves
  panel = mod_obj$nmp
  From = as.character(mod_obj$EndDate - 721)
  To = as.character(mod_obj$EndDate)

  Contrib1 = prep_Contrib1(Contrib, spend, year, panel)

  Input = prep_input(Contrib1)

  Data = prep_data(fmi_data, From, To, Allparams = Contrib1$LU, Contrib1, panel)

  AllReach <- ReachCurveCalc2(Data, Input, fit_curves)

  Percent <- c(0:100)
  AllReach1 <- cbind(Percent, AllReach)

  AllReach1 <-
    gather(AllReach1, "Category", "Value", 2:ncol(AllReach1)) %>%
    mutate(
      TableName = str_split_fixed(Category, " ", n = 2)[, 1],
      Category = str_split_fixed(Category, " ", n = 2)[, 2]
    ) %>%
    filter(!is.na(Value))

  AllReach2 <-
    left_join(select(Contrib1, Categories, TableName), AllReach1, c("TableName" = "TableName")) %>%
    spread(Category, Value) %>%
    group_by(Categories, Percent) %>%
    summarise(Contribution = sum(Contribution, na.rm = TRUE), Spend = mean(Spend, na.rm = TRUE), Reach = mean(Reach, na.rm = TRUE)) %>%
    gather("Category", "Value", 3:5) %>%
    arrange(desc(Category)) %>%
    filter(Categories != "TV COMP")

  # Start ABCs
  AllReach3 <- dcast(AllReach2, Percent ~ Categories + Category, fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "Value")
  AllReach3$Percent <- NULL


  totalmatrix <- AllReach3
  abcs <- list()
  names(totalmatrix) <- rep(c("Contribution", "Reach", "Spend"), times = ncol(totalmatrix) / 3)


  dummy <- tribble(
    ~term, ~estimate, ~std.error, ~statistic, ~p.value,
    "a", 0, 0, 0, 0,
    "b", 0, 0, 0, 0,
    "c", 0, 0, 0, 0
  )

  # Loop Reach Curves to find ABCs
  for (x in 1:(ncol(totalmatrix) / 3)) {
    # Find your ABCs
    print(x)
    data <- totalmatrix[, (1 + 3 * (x - 1)):(3 + 3 * (x - 1))]
    model <- try(onls(Contribution ~ a / (1 + b * ((Spend / 1000))^c),
                      start = list(a = 80, b = 1, c = -1),
                      data = data,
                      lower = c(a = 0.1, b = 0.1, c = -2)
    ))

    if (class(model) == "try-error") {
      abcs[[x]] <- dummy
      next
    }

    m <- try({
      m <- broom::tidy(model)
    })
    if (class(m) == "try-error") {
      abcs[[x]] <- dummy
      next
    }

    abcs[[x]] <- m
  }


  abcs <- abcs %>%
    bind_rows() %>%
    select(estimate)
  abcs <- data.frame(matrix(unlist(abcs), nrow = 3))
  names(abcs) <- unique(AllReach2$Categories)

  # Find real contributions
  realcontributions <-
    AllReach2 %>%
    filter(Category == "Contribution") %>%
    filter(Percent == 20)

  realcontributions <- t(realcontributions[, 4])

  realspend <-
    AllReach2 %>%
    filter(Category == "Spend") %>%
    filter(Percent == 20)

  realspend <- t(realspend[, 4])

  # Adjust the A
  FirstContr <- mapply(Reach, abcs[1, ], abcs[2, ], abcs[3, ], realspend[1, ])
  FirstContr <- as.data.frame(t(FirstContr))
  AdjA <- (realcontributions / FirstContr) * abcs[1, ]



  names(AdjA) <- names(abcs)
  names(realcontributions) <- names(abcs)
  names(realspend) <- names(abcs)
  ABCs <- rbind(abcs, AdjA)
  ABCs$Parameters <- c("A", "B", "C", "Adjusted A")
  ABCs <-
    ABCs %>%
    select(Parameters, 1:(ncol(ABCs) - 1))
  ABCs <- ABCs[, 2:ncol(ABCs)]

  if (is.null(dim(ABCs))) {
    ABCs <- as.data.frame(ABCs)
    names(ABCs) <- names(abcs)
  }

  FinalContr <- mapply(Reach, ABCs[4, ], ABCs[2, ], ABCs[3, ], realspend[1, ])
  FinalContr <- as.data.frame(t(FinalContr))
  names(FinalContr) <- names(ABCs)
  ABCs <- rbind(ABCs, FinalContr)
  realspend <- as.data.frame(realspend)
  names(realspend) <- names(ABCs)
  ABCs <- rbind(ABCs, realspend)
  colABCs <- c("A", "B", "C", "Adjusted A", "Contribution", "Spend")
  ABCs <- cbind(colABCs, ABCs)

  if (multiplier > 0) {
    # Revenue Calc
    Revenue <- ABCs[5, c(2:ncol(ABCs))] * multiplier
    ABCs <- rbind(ABCs[, c(2:ncol(ABCs))], Revenue)
    # ABCs = rbind(ABCs, Revenue)
    # ROI Calc
    ROIs <- ABCs[c(7), ] / ABCs[c(6), ]
    ABCs <- rbind(ABCs, ROIs)
    # mROI
    mROIs <- ((mapply(Reach, ABCs[4, ], ABCs[2, ], ABCs[3, ], ABCs[6, ] + 10000) * multiplier) - (mapply(Reach, ABCs[4, ], ABCs[2, ], ABCs[3, ], ABCs[6, ]) * multiplier)) / 10000
    ABCs <- rbind(ABCs, mROIs)
    # Labels
    colABCs <- c("A", "B", "C", "Adjusted A", "Contribution", "Spend", "Revenue", "ROI", "mROI")
    rownames(ABCs) <- colABCs
    # ABCs[,1] = colABCs
  } else {
    ABCs <- ABCs
  }


  return(list(
    ABCs = ABCs,
    Data = Data,
    Input = Input,
    Reach = AllReach3,
    Contribution = Contrib1
  ))

}
