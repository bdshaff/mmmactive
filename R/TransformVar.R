#' TransformVar
#'
#' @param data_vector - vector
#' @param spec_row - row from spec table
#' @param fit_curves - fit curves
#' @param print - logical
#'
#' @import RcppRoll
#' @return vector - transformed data_vector
#' @export

TransformVar <- function(data_vector = NULL, spec_row = NULL, fit_curves = NULL, print = TRUE) {
  require(tidyverse)

  if (is.null(data_vector)) {
    warning("TransformVar Input data_vector is NULL")
  } else if (!is.numeric(data_vector)) {
    warning("TransformVar Input data_vector is not of type numeric. Casting to numeric. Please check data!")
    data_vector <- as.numeric(data_vector)
  }
  if (is.null(spec_row)) {
    warning("TransformVar Input spec_row is NULL")
  }
  if (is.null(fit_curves)) {
    warning("TransformVar Input fit_curves is NULL")
  }

  type <- toupper(unlist(str_split(spec_row$TransformType, "_")))
  data_vector_transform <- data_vector

  for (i in 1:length(type)) {
    if (print) {
      cat(type[i], " transform ", spec_row$Orig_Variable, "\n")
    }
    if (type[i] == "ADSTOCK") {
      data_vector_transform <- AdStockPD(data_vector_transform, spec_row$Decay, spec_row$Period)
    }
    if (type[i] == "ADR") {
      if (str_detect(spec_row$Trans_Variable, "tv")) {
        data_vector_transform <- mmmactive::AdResponse(data_vector_transform, fit_curves$tv, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      } else if (str_detect(spec_row$Trans_Variable, "digital")) {
        data_vector_transform <- mmmactive::AdResponse(data_vector_transform, fit_curves$displayt1, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      } else if (str_detect(spec_row$Trans_Variable, "addressable")) {
        data_vector_transform <- mmmactive::AdResponse(data_vector_transform, fit_curves$addressable, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      } else if (str_detect(spec_row$Trans_Variable, "streaming")) {
        data_vector_transform <- mmmactive::AdResponse(data_vector_transform, fit_curves$st, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      } else {
        warning("applying the TV fit curve")
        data_vector_transform <- mmmactive::AdResponse(data_vector_transform, fit_curves$tv, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      }
    }
    if (type[i] == "ADSTOCKV3") {
      data_vector_transform <- adstockv3(data_vector_transform, spec$Decay[i], spec$Peak[i], spec$Length[i])
    }
    if (type[i] == "LAG") {
      data_vector_transform <- dplyr::lag(data_vector_transform, spec_row$Lag, default = 0) +
        c(rep(data_vector_transform[1],spec_row$Lag), rep(0, length(data_vector_transform) - spec_row$Lag))

    }
    if (type[i] == "LOG") {
      data_vector_transform <- log(data_vector_transform * spec_row$Scale + 1)
    }
    if (type[i] == "POLY") {
      data_vector_transform <- myPoly(data_vector_transform, spec_row$Alpha)
    }
    if (type[i] == "MA") {
      data_vector_transform <- rollmean(data_vector_transform, spec_row$Window, align = "right", fill = NA)
      data_vector_transform[which(is.na(data_vector_transform))] <- data_vector_transform[which(is.na(data_vector_transform))]
    }
    if (type[i] == "STEIN") {
      data_vector_transform <- shrinker(data_vector_transform, bw = spec_row$Window, trim = spec_row$Trim)
    }
    if (type[i] == "CPT") {
      data_vector_transform <-
        cpt.meanvar(data_vector_transform, minseglen = 6, penalty = "CROPS", pen.value = c(0, 100), method = "PELT")
    }
    if (type[i] == "POWER") {
      data_vector_transform <- (data_vector_transform)^spec_row$Power
    }
    if (type[i] == "NONE") {
      data_vector_transform <- data_vector_transform
    }
  }
  return(data_vector_transform)
}
