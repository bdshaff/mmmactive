#' Plot_Resids
#'
#' @param obj - model object
#' @param panel_name  - panel name
#'

Plot_Resids = function(obj, panel_name){

  x <- obj$data
  cs = mod_obj$cs
  ts = mod_obj$Time

  # Dependent Variable
  depvar <- obj$spec$Trans_Variable[obj$spec$Variable_Type=="Dependent"]
  x$KPI <- x[[depvar]]

  # Fitted Values
  x$fitted <- obj$Model$fitted_value

  # Residuals
  x$resid <- obj$Model$residuals

  if (panel_name == "all") {
    print("View all panel resids")
    DF =
      x %>%
      select(!!sym(cs), !!sym(ts), KPI, fitted, resid) %>%
      group_by(!!sym(cs), !!sym(ts)) %>%
      summarise(KPI = sum(KPI, na.rm = TRUE),
                fitted = sum(fitted, na.rm = TRUE),
                resid = sum(resid, na.rm = TRUE))

    plot1 <-
      ungroup(DF) %>%
      ggplot(aes(!!sym(ts))) +
      geom_line(aes(y = KPI, colour = "Total Sales")) +
      geom_line(aes(y = fitted, colour = "Fitted Total Sales")) +
      geom_line(aes(y = resid, colour = "Residual")) +
      facet_wrap(paste(cs, "~."),scales = "free") +
      theme(panel.background = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(angle = 35, size = 5),
            axis.text.x = element_text(angle = 30, size = 5))

    plotly::ggplotly(plot1)

  } else if (panel_name != FALSE) {

    print(paste("View",panel_name,"panel resids"))
    DF =
      x %>% filter(!!sym(cs) == panel_name) %>%
      select(!!sym(ts), KPI, fitted, resid) %>%
      group_by(!!sym(ts)) %>%
      summarise(KPI = sum(KPI, na.rm = TRUE),
                fitted = sum(fitted, na.rm = TRUE),
                resid = sum(resid, na.rm = TRUE))

    plot1 <-
      ungroup(DF) %>%
      ggplot(aes(!!sym(ts))) +
      geom_line(aes(y = KPI, colour = "Total Sales")) +
      geom_line(aes(y = fitted, colour = "Fitted Total Sales")) +
      geom_line(aes(y = resid, colour = "Residual")) +
      theme(panel.background = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(angle = 35, size = 5),
            axis.text.x = element_text(angle = 30, size = 5)) +
      labs(title = panel_name)

    plotly::ggplotly(plot1)

  } else {

    print("View all panel resids: panels aggregated")
    DF  =
      x %>%
      select(!!sym(ts), KPI, fitted, resid) %>%
      group_by(!!sym(ts)) %>%
      summarise(KPI = sum(KPI, na.rm = TRUE),
                fitted = sum(fitted, na.rm = TRUE),
                resid = sum(resid, na.rm = TRUE))

    plot1 <-
      ungroup(DF) %>%
      ggplot(aes(!!sym(ts))) +
      geom_line(aes(y = KPI, colour = "Total Sales")) +
      geom_line(aes(y = fitted, colour = "Fitted Total Sales")) +
      geom_line(aes(y = resid, colour = "Residual")) +
      theme(panel.background = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(angle = 35, size = 5),
            axis.text.x = element_text(angle = 30, size = 5)) +
      labs(title = "All panels aggregated")

    plotly::ggplotly(plot1)
  }
}
