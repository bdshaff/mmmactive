#' Load_ModelData
#'
#' This function load the monthly and weekly level KPI and FMI data.
#'
#' @param mod_obj - model mod_object
#' @param input_file_MSRPData - path to an .xlsx file with monthly and weekly data tabs.
#' @param NAMEPLATE - Nameplate (ex. Titan)
#'
#' @return mod_obj
#'

Load_ModelData <- function(mod_obj, input_file_ModelData) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }

  datasheets <- excel_sheets(input_file_ModelData)

  mod_obj$data_input <-
    map(datasheets, ~ read_xlsx(input_file_ModelData, sheet = .x)) %>%
    set_names(str_extract(datasheets, "weekly|monthly"))

  groups <- names(mod_obj$data_group_selector)

  if ("weekly" %in% str_extract(datasheets, "weekly|monthly")) {
    mod_obj$data_input$weekly <-
      mod_obj$data_input$weekly %>%
      mutate(week = ymd(week)) %>%
      group_by_at(vars(groups))

    for (g in groups) {
      keep_vec <- unlist(mod_obj$data_input$weekly[, g]) %in% mod_obj$data_group_selector[[g]]
      mod_obj$data_input$weekly <- mod_obj$data_input$weekly[keep_vec, ]
    }
  }

  if ("monthly" %in% str_extract(datasheets, "weekly|monthly")) {
    mod_obj$data_input$monthly <-
      mod_obj$data_input$monthly %>%
      mutate(month = ymd(month)) %>%
      group_by_at(vars(groups))

    for (g in groups) {
      keep_vec <- unlist(mod_obj$data_input$monthly[, g]) %in% mod_obj$data_group_selector[[g]]
      mod_obj$data_input$monthly <- mod_obj$data_input$monthly[keep_vec, ]
    }
  }

  return(mod_obj)
}
