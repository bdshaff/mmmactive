#' Fitted vs Actual and Residual Plot
#'
#' A Plot of fitted vs observed for a mod_obj
#'
#' @param mod_obj - model mod_object
#' @param panel_name  - panel name
#'
#' @return mod_obj
#'
#' @import ggplot2
#' @export

Plot_Resids <- function(mod_obj, panel_name) {

  # add code to check that a Model element exists
  # if not alert that a model needs to be fit first

  x <- mod_obj$data
  cs <- mod_obj$cs
  ts <- mod_obj$Time

  # Dependent Variable
  depvar <- mod_obj$spec$Trans_Variable[mod_obj$spec$Variable_Type == "Dependent"]
  x$KPI <- x[[depvar]]

  # Fitted Values
  x$fitted <- mod_obj$Model$fitted_value

  # Residuals
  x$resid <- mod_obj$Model$residuals

  if (panel_name == "all") {
    print("View all panel resids")
    DF <-
      x %>%
      dplyr::select(!!sym(cs), !!sym(ts), KPI, fitted, resid) %>%
      dplyr::group_by(!!sym(cs), !!sym(ts)) %>%
      dplyr::summarise(
        KPI = sum(KPI, na.rm = TRUE),
        fitted = sum(fitted, na.rm = TRUE),
        resid = sum(resid, na.rm = TRUE)
      )

    plot1 <-
      dplyr::ungroup(DF) %>%
      ggplot(aes(!!sym(ts))) +
      geom_line(aes(y = KPI, colour = "Total Sales")) +
      geom_line(aes(y = fitted, colour = "Fitted Total Sales")) +
      geom_line(aes(y = resid, colour = "Residual")) +
      facet_wrap(paste(cs, "~."), scales = "free") +
      theme(
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(angle = 35, size = 5),
        axis.text.x = element_text(angle = 30, size = 5)
      )

    plotly::ggplotly(plot1)
  } else if (panel_name != FALSE) {
    print(paste("View", panel_name, "panel resids"))
    DF <-
      x %>%
      filter(!!sym(cs) == panel_name) %>%
      select(!!sym(ts), KPI, fitted, resid) %>%
      group_by(!!sym(ts)) %>%
      summarise(
        KPI = sum(KPI, na.rm = TRUE),
        fitted = sum(fitted, na.rm = TRUE),
        resid = sum(resid, na.rm = TRUE)
      )

    plot1 <-
      ungroup(DF) %>%
      ggplot(aes(!!sym(ts))) +
      geom_line(aes(y = KPI, colour = "Total Sales")) +
      geom_line(aes(y = fitted, colour = "Fitted Total Sales")) +
      geom_line(aes(y = resid, colour = "Residual")) +
      theme(
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(angle = 35, size = 5),
        axis.text.x = element_text(angle = 30, size = 5)
      ) +
      labs(title = panel_name)

    plotly::ggplotly(plot1)
  } else {
    print("View all panel resids: panels aggregated")
    DF <-
      x %>%
      select(!!sym(ts), KPI, fitted, resid) %>%
      group_by(!!sym(ts)) %>%
      summarise(
        KPI = sum(KPI, na.rm = TRUE),
        fitted = sum(fitted, na.rm = TRUE),
        resid = sum(resid, na.rm = TRUE)
      )

    plot1 <-
      ungroup(DF) %>%
      ggplot(aes(!!sym(ts))) +
      geom_line(aes(y = KPI, colour = "Total Sales")) +
      geom_line(aes(y = fitted, colour = "Fitted Total Sales")) +
      geom_line(aes(y = resid, colour = "Residual")) +
      theme(
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(angle = 35, size = 5),
        axis.text.x = element_text(angle = 30, size = 5)
      ) +
      labs(title = "All panels aggregated")

    plotly::ggplotly(plot1)
  }
}
