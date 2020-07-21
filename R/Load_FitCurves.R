#' Load_FitCurves
#'
#' Loads a list of fitcurves onto the mod_obj
#'
#' @param mod_obj - model mod_object
#' @param input_file_ModelFitCurves - file path
#'
#' @return mod_obj
#' @export

Load_FitCurves <- function(mod_obj, input_file_ModelFitCurves) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }

  datasheets <- readxl::excel_sheets(input_file_ModelFitCurves)

  mod_obj$fit_curves <-
    purrr::map(datasheets, ~ janitor::clean_names(readxl::read_xlsx(input_file_ModelFitCurves, sheet = .x))) %>%
    rlang::set_names(stringr::str_replace(stringr::str_extract(datasheets, "_[a-zA-Z0-9.-]*$"), "_", ""))

  return(mod_obj)
}
