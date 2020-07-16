#' Load_ModelData
#'
#' This function load the monthly and weekly level KPI and FMI data.
#'
#' @param mod_obj - model mod_object
#' @param input_file_ModelData - path to an .xlsx file with monthly and weekly data tabs.
#'
#' @return mod_obj
#' @export

Load_ModelData <- function(mod_obj, input_file_ModelData) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.load_data_ready(mod_obj)) {
    stop("mod_obj missing group selector needed to load data.")
  }

  datasheets <- readxl::excel_sheets(input_file_ModelData)

  mod_obj$data_input <-
    purrr::map(datasheets, ~ readxl::read_xlsx(input_file_ModelData, sheet = .x)) %>%
    rlang::set_names(stringr::str_extract(datasheets, "weekly|monthly"))

  groups <- names(mod_obj$data_group_selector)

  if ("weekly" %in% stringr::str_extract(datasheets, "weekly|monthly")) {
    mod_obj$data_input$weekly <-
      mod_obj$data_input$weekly %>%
      dplyr::mutate(week = ymd(week)) %>%
      dplyr::group_by_at(dplyr::vars(groups))

    for (g in groups) {
      keep_vec <- unlist(mod_obj$data_input$weekly[, g]) %in% mod_obj$data_group_selector[[g]]
      mod_obj$data_input$weekly <- mod_obj$data_input$weekly[keep_vec, ]
    }
  }

  if ("monthly" %in% str_extract(datasheets, "weekly|monthly")) {
    mod_obj$data_input$monthly <-
      mod_obj$data_input$monthly %>%
      dplyr::mutate(month = ymd(month)) %>%
      dplyr::group_by_at(dplyr::vars(groups))

    for (g in groups) {
      keep_vec <- unlist(mod_obj$data_input$monthly[, g]) %in% mod_obj$data_group_selector[[g]]
      mod_obj$data_input$monthly <- mod_obj$data_input$monthly[keep_vec, ]
    }
  }

  return(mod_obj)
}
