#' unwind
#'
#' @param model1 - model1
#' @param model2 - model2
#' @param  use_model2_panel - use_model2_panel
#' @param print_summary - print_summary
#' @param expandPeriodToLongest - logical
#' @param demo - logical
#'
#' @export
#'

unwind <- function(model1, model2, use_model2_panel = "ALL", print_summary = TRUE,
                   expandPeriodToLongest = FALSE, demo = FALSE) {
  Base <- "ModelKPI"
  options(scipen = 100, digits = 4)
  agg_period_ <- base::names(model1$contributionsTbl)[2]
  base::names(model1$contributionsTbl)[2] <- "agg_period"
  main_m_ini <- model1$contributionsTbl
  main_m_groups <- base::unique(main_m_ini[[1]])

  if (use_model2_panel != "ALL") {
    sub_m_ini <- model2$contributionsTblPerc %>% dplyr::filter(Group ==
                                                                 use_model2_panel)
    sub_m_groups <- use_model2_panel
  } else {
    sub_m_ini <- model2$contributionsTblPerc
    sub_m_groups <- base::unique(sub_m_ini[[1]])
  }

  main_m_list <- base::list()
  for (p in 1:base::length(main_m_groups)) {
    main_m_list[p] <- main_m_ini[main_m_ini$Group == main_m_groups[p], -1] %>% base::list()
  }

  sub_m_list <- base::list()
  for (p in 1:base::length(sub_m_groups)) {
    sub_m_list[p] <- sub_m_ini[
      sub_m_ini$Group == sub_m_groups[p],
      -1
    ] %>% list()
  }

  for (q in 1:base::length(main_m_groups)) {
    if (base::length(sub_m_groups) == base::length(main_m_groups)) {
      v <- q
    } else {
      v <- 1
    }

    main_m <- main_m_list[[q]]
    if (q == v) {
      sub_m <- sub_m_list[[v]]
      new_sub_m <- base::data.frame(base::matrix(NA,
                                                 nrow = base::nrow(main_m),
                                                 ncol = base::ncol(sub_m)
      ))
      names(new_sub_m) <- names(sub_m)
      new_sub_m[, 1:2] <- main_m[, 1:2]
      mrow <- main_m$Date %in% sub_m$Date
      srow <- sub_m$Date %in% main_m$Date
      new_sub_m[mrow, -(1:2)] <- sub_m[srow, -(1:2)]
      incomp_nsubm <- !stats::complete.cases(new_sub_m)
      comp_nsubm <- stats::complete.cases(new_sub_m)

      if (expandPeriodToLongest) {
        for (i in (1:base::length(incomp_nsubm))[incomp_nsubm]) {
          k <- base::which(base::abs(new_sub_m$Date[comp_nsubm] -
                                       new_sub_m$Date[i]) == base::min(base::abs(new_sub_m$Date[comp_nsubm] -
                                                                                   new_sub_m$Date[i])))
          new_sub_m[i, -(1:2)] <- new_sub_m[comp_nsubm, ][k, -(1:2)]
        }
      } else {
        new_sub_m <- new_sub_m[stats::complete.cases(new_sub_m), ]
        main_m <- main_m[main_m$Date %in% new_sub_m$Date, ]
      }

      sub_m <- new_sub_m
      sub_var_name <- base::names(sub_m[3])
    }
    new_sub_m <- new_sub_m[new_sub_m$Date %in% main_m$Date, ]
    main_m <- main_m[main_m$Date %in% new_sub_m$Date, ]
    new_m1 <- main_m[, 1:2]
    new_m2 <- main_m[, !(base::names(main_m) %in% sub_var_name)]
    main_var_for_sub <- main_m[, sub_var_name]

    for (m in base::names(sub_m)[-(1:2)]) {
      if (m != "BASE") {
        new_m1[, m] <- main_var_for_sub * sub_m[, m]
      }
    }

    new_m1[, sub_var_name] <- main_var_for_sub * sub_m[, "BASE"]

    aggDateMelt <- function(x) {
      reshape2::melt(x, id.vars = c("agg_period", "Date"))
    }
    base::suppressWarnings(contribution <- dplyr::bind_rows(aggDateMelt(new_m1), aggDateMelt(new_m2)))

    if (Base != "ModelKPI") {
      contribution$variable[contribution$variable == sub_var_name] <- "BASE"
    }

    contribution <- reshape2::dcast(contribution, agg_period + Date ~ variable, fun.aggregate = sum, value.var = "value")
    contribution <- aggDateMelt(contribution)
    base::names(contribution) <- base::c("agg_period", "Date", "Categories", "value")

    if (q == v & v == 1) {
      if (use_model2_panel != "ALL") {
        model2$variableTbl$Model <-
          model2$variableTbl$Model %>%
          base::paste0("(", use_model2_panel, ")", sep = "")
      }

      var_cat_tbl <-
        base::unique(dplyr::bind_rows(
          model1$variableTbl,
          model2$variableTbl
        ))

      var_cat_tbl <- var_cat_tbl %>% dplyr::filter(Categories != "FY")
      var_order <- base::unique(var_cat_tbl$Categories)
      var_order <- var_order[!stringr::str_detect(var_order, "BASE")]
      var_order <- base::c(
        "agg_period", var_order[1:2],
        use_model2_panel,
        "BASE", var_order[-(1:2)]
      )

      if (Base != "ModelKPI") {
        var_order <- var_order[var_order != sub_var_name]
      }
    }

    summary <- contribution %>%
      reshape2::dcast(agg_period ~ Categories, fun.aggregate = sum, value.var = "value") %>%
      .[, var_order[-c(2, 3)]]
    rename_and_agg2 <- function(x) {
      reshape2::dcast(x, Date + agg_period ~ Categories, fun.aggregate = sum, value.var = "value") %>% .[, var_order]
    }
    contribution <- rename_and_agg2(contribution)

    if (q == 1) {
      print(q)
      contribution_return <- base::cbind(Group = main_m_groups[q], contribution)
      summary_return <- base::cbind(Group = main_m_groups[q], summary)
    } else {
      print(q)
      contribution <- base::cbind(Group = main_m_groups[q], contribution)
      summary <- base::cbind(Group = main_m_groups[q], summary)
      contribution_return <- base::rbind(contribution_return, contribution)
      summary_return <- base::rbind(summary_return, summary)
    }
  }


  if (print_summary == TRUE) {
    base::print(var_cat_tbl)
  }

  if (base::length(main_m_groups) != 1) {
    tmp <- summary_return[, -1] %>%
      dplyr::group_by(agg_period) %>%
      dplyr::summarise_each(dplyr::funs(sum)) %>%
      base::cbind(
        Group = "TOTAL",
        .
      )
    if (print_summary == TRUE) {
      base::print(tmp)
    }
    tmp[, -(1:3)] <- tmp[, -(1:3)] / tmp[[3]]
    if (print_summary == TRUE) {
      base::names(tmp)[2] <- agg_period_
      base::print(tmp)
    }
  }
  modname2 <- base::names(model2$contributionsTbl)[4]
  modname1 <- base::names(model1$contributionsTbl)[4]
  if (length(model1) != 6 & demo == FALSE) {
    i_c <- decompR::incremental_contributions(model1, contribution_return, modname2)
    i_c$Date <- base::as.character(i_c$Date)
    tmp.ic <- model1$incrementalContributionsTbl
    tmp.ic$Date <- base::as.character(tmp.ic$Date)
    i_c <- dplyr::bind_rows(tmp.ic, i_c)
    #i_c[base::is.na(i_c)] <- 0
    i_c$Date <- base::as.Date(i_c$Date)
  } else {
    i_c1 <- decompR::incremental_contributions(NULL, model1, modname1)
    i_c2 <- decompR::incremental_contributions(model1, contribution_return, modname2)
    i_c1$Date <- base::as.character(i_c1$Date)
    i_c2$Date <- base::as.character(i_c2$Date)
    i_c <- dplyr::bind_rows(i_c1, i_c2)
    i_c[base::is.na(i_c)] <- 0
    i_c$Date <- base::as.Date(i_c$Date)
  }


  if (!(base::min(model1$contributionsTbl$Date) == base::min(model2$contributionsTbl$Date)) &&
      !(base::min(model2$contributionsTbl$Date) == base::min(contribution_return$Date))) {
    base::message("MODEL PERIODS DIFFER")
    base::message("______________________")
    base::message("Start periods:")
    base::message("Model1:       ", base::min(model1$contributionsTbl$Date))
    base::message("Model2:       ", base::min(model2$contributionsTblPerc$Date))
    base::message("This decomp:  ", base::min(contribution_return$Date))
    base::message("______________________")
    base::message("End periods:")
    base::message("Model1:       ", base::max(model1$contributionsTbl$Date))
    base::message("Model2:       ", base::max(model2$contributionsTblPerc$Date))
    base::message("This decomp:  ", base::max(contribution_return$Date))
    base::message("______________________")
    if (!expandPeriodToLongest & (base::max(model2$contributionsTblPerc$Date) <
                                  base::max(model1$contributionsTbl$Date) | base::min(model2$contributionsTblPerc$Date) >
                                  base::min(model1$contributionsTbl$Date))) {
      base::message("Set expandPeriodToLongest=TRUE to impute nearest values in submodel and extend the period to match the main model's.")
    }
  }

  base::names(contribution_return)[2] <- agg_period_
  base::names(summary_return)[2] <- agg_period_
  base::names(i_c)[2] <- agg_period_
  if (use_model2_panel != "ALL") {
    model2pct <- as.data.frame(model2$varContPct)
    KPI2 <- model2pct[2, 2]
    model2pct <- model2pct[model2pct$Group == use_model2_panel, ]
  }
  model1ttl <- as.data.frame(model1$varCont)
  subtotalmodel1 <- data.frame(matrix(NA, nrow = 1, ncol = (ncol(model1ttl) - 4)))
  model1ttl <- model1ttl[model1ttl$Categories == KPI2, ]

  for (col in 5:ncol(model1ttl)) {
    colyer <- colnames(model1ttl)[col]
    picolyear <- sum(model1ttl[col], na.rm = T)
    model2pct[[colyer]] <- model2pct[[colyer]] * picolyear
  }

  var_tblPG <- rbind(as.data.frame(model1$varCont), model2pct[stats::complete.cases(model2pct), ])

  return(list(
    contributionsTbl = contribution_return, summary = summary_return,
    variableTbl = var_cat_tbl, incrementalContributionsTbl = i_c,
    varCont = var_tblPG
  ))
}
