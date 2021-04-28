#' Plot Transformed Variable
#'
#' Plot Transformed Variable
#'
#' @return ggplot
#'
#' @export

plot_transformed = function(mod_obj, var){
  orig_var = var
  trans_var = mod_obj$spec[mod_obj$spec$Orig_Variable == orig_var, "Trans_Variable"]

  mod_obj$data_input$weekly %>%
    mutate(month = lubridate::floor_date(week, unit = "month")) %>%
    group_by(month, !!sym(mod_obj$cs)) %>%
    summarise(!!sym(orig_var) := mean(!!sym(orig_var))) %>%
    ungroup() %>%
    inner_join(
      mod_obj$data %>%
        select(month, !!sym(mod_obj$cs), !!sym(trans_var)) %>%
        ungroup()
    ) %>%
    pivot_longer(-c(month, !!sym(mod_obj$cs))) %>%
    ggplot(aes(month, value, color = name)) +
    geom_line() +
    facet_wrap(as.formula(paste(mod_obj$cs,"~.")))
}
