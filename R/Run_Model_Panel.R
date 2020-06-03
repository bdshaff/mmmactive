#' Execute Model Fitting
#'
#' @param obj
#' @param Method
#'
#' @return
#' @export
#'
#' @description
#'
#' @examples
#'
Run_Model_Panel <- function(obj, Method = "Bayes") {
  Method <- "Bayes"
  big_number <- 100000 # for difuse priors
  spec <- obj$spec
  x <- obj$data

  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]
  IV <- spec$Trans_Variable[spec$Variable_Type != "Dependent"]
  spec <- spec[spec$Variable_Type != "Dependent", ]
  priors <- dplyr::select(spec, Orig_Variable, Trans_Variable, Prior_Mean, Prior_SD)

  if (!is.null(obj$cs)) {
    cs <- obj$cs
  }

  if (!is.null(obj$Time)) {
    ts <- obj$Time
  }

  # do some checking and make sure the model variables (DepVar and IV) are in data
  if (DepVar %in% names(x)) {
    y <- x[[DepVar]]
  } else {
    stop("The dependent variable", DepVar, "is not in the model dataset. ")
  }

  if ("Intercept" %in% (spec$Trans_Variable)) {
    x$Intercept <- 1
  }

  if (FALSE %in% (IV %in% names(x))) {
    stop(
      "Some of the independent variables ",
      setdiff(IV, names(x)[IV %in% names(x)]),
      " are not in the model x set. Please check _Variables.csv file. "
    )
  }

  eq <- ifelse(spec$VaryBy[1] == "None", spec$Trans_Variable[1], paste(spec$VaryBy[1], spec$Trans_Variable[1], sep = ":"))
  for (k in 2:nrow(spec)) {
    tmp <- ifelse(spec$VaryBy[k] == "None", spec$Trans_Variable[k], paste(spec$VaryBy[k], spec$Trans_Variable[k], sep = ":"))
    eq <- paste(eq, tmp, sep = "+")
  }

  eq <- paste(eq, " -1")
  eq <- paste(DepVar, " ~ ", eq)
  eq <- as.formula(eq)

  eq_lm <- paste(IV[!IV %in% "Intercept"], collapse = " + ")
  eq_lm <- paste(eq_lm, " + ", cs)
  eq_lm <- paste(DepVar, " ~ ", eq_lm)
  eq_lm <- as.formula(eq_lm)


  print("calling my_bayes()...")
  bayes_obj_inter <- my_bayes(formula = eq, data = x)
  mod_matrix <- model.matrix(eq, x)


  b <- bayes_obj_inter$coefficients
  b$Variables <- row.names(t(mod_matrix))
  priors <- b
  priors$Prior_Mean <- 0
  priors$Prior_SD <- big_number
  # first populate the priors with user defined priors in the Variables.csv
  for (i in 1:nrow(spec)) {
    v <- spec$Trans_Variable[i]
    priors$Prior_Mean[grep(v, priors$Variables)] <- spec$Prior_Mean[i]
    priors$Prior_SD[grep(v, priors$Variables)] <- spec$Prior_SD[i]
  }

  hb_var <- spec$Trans_Variable[spec$VaryBy != "None" & spec$Orig_Variable != "Intercept"]
  #  hb_var <- spec$Trans_Variable[spec$Orig_Variable != "Intercept"]

  if ("PriorSD_Adj" %in% names(spec)) {
    spec$PriorSD_Adj <- abs(spec$PriorSD_Adj)
  } else {
    spec$PriorSD_Adj <- 1
  }

  if (length(hb_var) >= 1) {
    for (j in 1:length(hb_var)) {
      # if override == "N", calculate emperical prior and populate the priors dataframe
      if (toupper(spec$Override[spec$Trans_Variable == hb_var[j]]) == "N") {
        tmp <- priors$Estimate[grep(hb_var[j], priors$Variables)]
        if (spec$Sign[spec$Trans_Variable == hb_var[j]] > 0) {
          if (is.nan(mean(tmp[tmp > 0]))) {
            cat("The code is not able to find a emperical prior for", hb_var[j], "\n")
            cat("A diffuse prior is used. If you want an informative prior, you can override.\n")
          } else {
            priors$Prior_Mean[grep(hb_var[j], priors$Variables)] <- mean(tmp[tmp > 0])
          }
        } else if (spec$Sign[spec$Trans_Variable == hb_var[j]] > 0) {
          if (is.nan(mean(tmp[tmp < 0]))) {
            cat("The code is not able to find a emperical prior for", hb_var[j], "\n")
            cat("A diffuse prior is used. If you want an informative prior, you can override.\n")
          } else {
            priors$Prior_Mean[grep(hb_var[j], priors$Variables)] <- mean(tmp[tmp < 0])
          }
        } else {
          priors$Prior_Mean[grep(hb_var[j], priors$Variables)] <- mean(tmp)
        }
        # for now let's set the prior_sd = prior_mean*PriorSD_Adj. The PriorSD_Adj defauls to 1 in the _Variables.csv

        priors$Prior_SD[grep(hb_var[j], priors$Variables)] <-
          priors$Prior_Mean[grep(hb_var[j], priors$Variables)] * spec$PriorSD_Adj[spec$Trans_Variable == hb_var[j]]
        if (sum(priors$Prior_SD[grep(hb_var[j], priors$Variables)]) == 0) {
          priors$Prior_SD[grep(hb_var[j], priors$Variables)] <- big_number
        }
      }
    }
  }

  order_of_var <- row.names(t(mod_matrix))

  a <- priors
  a$Variables <- factor(a$Variables, levels = order_of_var)
  a <- arrange(a, Variables)
  a$Variables <- as.character(a$Variables)
  priors <- a

  bayes_obj <- my_bayes(formula = eq, data = x, priors = priors)

  varyby <- unique(obj$spec$VaryBy)[unique(obj$spec$VaryBy) != "None"]
  if (length(varyby) > 0) {
    b <- bayes_obj$coefficients %>%
      separate(Variables, into = c(varyby, "Variables"), sep = "\\.", fill = "left")
    b[[varyby]] <- gsub(varyby, "", b[[varyby]])

    tmp <- data.frame(varyby = unique(obj$data[[varyby]]))
    names(tmp) <- varyby

    full_b <- expand.grid(
      varyby = tmp[[varyby]],
      Variables = obj$spec$Trans_Variable[obj$spec$Variable_Type != "Dependent"]
    )
    names(full_b)[1] <- varyby

    full_b$Variables <- as.character(full_b$Variables)
    full_b[[varyby]] <- as.character(full_b[[varyby]])
    full_b <- left_join(full_b, b)
    v <- unique(full_b$Variables[is.na(full_b$Estimate)])
    if (length(v) > 0) {
      for (j in 1:length(v)) {
        full_b$Estimate[full_b$Variable == v[j]] <- b$Estimate[b$Variables == v[j]]
        full_b$Error[full_b$Variable == v[j]] <- b$Error[b$Variables == v[j]]
      }
    }
    full_b$Tvalue <- full_b$Estimate / full_b$Error
  } else {
    varyby <- obj$CS
    tmp <- data.frame(varyby = unique(obj$data[[varyby]]))
    names(tmp) <- varyby
    full_b <- expand.grid(
      varyby = tmp[[varyby]],
      Variables = obj$spec$Trans_Variable[toupper(obj$spec$Variable_Type) != "DEPENDENT"]
    )
    full_b$varyby <- as.character(full_b$varyby)
    full_b$Variables <- as.character(full_b$Variables)
    names(full_b)[1] <- varyby

    full_b <- left_join(full_b, bayes_obj$coefficients)
  }
  bayes_obj$coefficients <- full_b
  obj$Model <- bayes_obj

  # calculate actual vs predicted
  obj$Model$act_pred <- act_pred(obj)

  if (0) {
    time <- rlang::sym(obj$Time)
    dv <- rlang::sym(DepVar)
    cs <- rlang::sym(obj$CS)
    cs <- as.formula(cs ~ .)


    obj$Model$plt_act_pred <-
      ggplot(obj$Model$act_pred, aes(x = time)) +
      geom_point(aes(y = dv, colour = "actual")) +
      geom_line(aes(y = predicted, colour = "predicted")) +
      facet_wrap(cs, scales = "free", ncol = 2)
  }


  obj$Model_interLM <- bayes_obj_inter
  obj$lmModel <- lm(eq_lm, data = x)

  # obj$Model$VIF <- data.frame(vif(obj$lmModel))
  # obj$Model$VIF <- tibble::rownames_to_column(obj$Model$VIF, var = "Variables")
  # obj$Model$Priors <- priors
  # names(obj$Model$VIF) <- c("VIF", "Variables")
  # obj$Model$VIF <- left_join(obj$Model$coefficients, obj$Model$VIF, by = "Variables")
  # obj$Model$VIF <- obj$Model$VIF[, c("VIF", "Variables", names(obj$Model$coefficients)[1])]
  # obj$Model$result_all <- full_join(obj$Model$VIF, obj$Model$coefficients)
  # obj$Model$result_all <- obj$Model$result_all[, c("Variables", names(obj$Model$coefficients)[1], "VIF", "Estimate", "Error", "Tvalue")]
  # names(obj$Model$result_all)[names(obj$Model$result_all) == "Variables"] <- "Trans_Variable"
  # obj$Model$result_all <- full_join(spec, obj$Model$result_all)

  obj$Model$VIF <- data.frame(vif(obj$lmModel))
  obj$Model$VIF <- tibble::rownames_to_column(obj$Model$VIF, var = "Variables")
  obj$Model$result_all <- full_join(obj$Model$VIF, obj$Model$coefficients)
  obj$Model$result_all <- obj$Model$result_all[, c("Variables", "GVIF", "Estimate", "Error", "Tvalue")]
  names(obj$Model$result_all)[names(obj$Model$result_all) == "Variables"] <- "Trans_Variable"
  obj$Model$result_all <- full_join(spec, obj$Model$result_all)

  obj$Model$DW <- durbinWatsonTest(obj$lmModel)
  obj$Model$result_all$R2 <- rep(obj$Model$R2, nrow(obj$Model$result_all))
  obj$Model$result_all$DW <- rep(obj$Model$DW$dw, nrow(obj$Model$result_all))
  return(obj)
}
