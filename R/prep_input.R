#' prep_input
#'
#' @param Contrib1 - Contrib1
#'
#' @export


prep_input = function(Contrib1){

  #input is a table with contributions

  Input <- Contrib1[, c("TableName", "Spend", "Contributions", "Type", "E", "R", "P", "D", "p", "d", "Categories")]
  Input$P[c(is.na(Input$P))] <- Input$p[c(!is.na(Input$p))]
  Input$D[c(is.na(Input$D))] <- Input$d[c(!is.na(Input$d))]

  #making it a very strange format here
  Input <-
    rbind(
      Input$Type,
      Input$Spend,
      Input$Contributions,
      Input$E,
      Input$R,
      Input$P,
      Input$D,
      Input$Categories,
      Input$TableName
    ) %>% as.data.frame(.)


  Input <- rbind(Input[c(1:3), ], NA, Input[c(4:7), ], NA, NA, NA, Input[c(8:9), ])

  Col <- rbind("R/L/P", "100% Spend", "100% Contribution", "DMA", "EF", "RF", "Period", "Decay", "Alpha", "Beta", "Power", "Category", "Var Name")

  Input <- cbind(Col, Input)

  row.names(Input) <- NULL
  Input <- as.data.frame(Input)
  names(Input) <- as.character(unlist(Input[1, ]))
  Input <- Input[-1, ]
  row.names(Input) <- NULL

  Input <- apply(Input, 2, function(x) as.character(x)) %>% as.data.frame(stringsAsFactors = FALSE)

  return(Input)
}
