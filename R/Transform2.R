#' Transform2
#'
#' Run data transformation and combine weekly and monthly level data accosting to the model specification.
#'
#' @param mod_obj - model mod_object
#' @param print - TRUE or FALSE
#'
#' @return mod_obj
#' @export

Transform2 <- function(mod_obj, print = TRUE) {
  if (!is.mod_obj(mod_obj)) {
    stop("Input must be of class mod_obj.")
  }
  if (!is.activated(mod_obj)) {
    stop("mod_obj must be activated.")
  }
  if (!is.transform_ready(mod_obj)) {
    stop("mod_obj is not transform ready.")
  }

  data_input <- mod_obj$data_input
  spec <- mod_obj$spec
  fit_curves <- mod_obj$fit_curves
  spec_trans <- spec %>% dplyr::filter(Transform == "Y")
  cross_section <- mod_obj$cs
  trans_variable <- mod_obj$spec$Trans_Variable
  kpi <- mod_obj$kpi
  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]


  if (class(data_input) == "list") {
    transformed_data <- purrr::map(names(data_input), ~ TransformTemp(
      data_tmp = data_input,
      spec_tmp = spec_trans,
      tmp = .x,
      fit_curves = fit_curves,
      cross_section = cross_section,
      print = print
    )) %>% purrr::set_names(names(data_input))

    mod_obj$data_transformed <- transformed_data
    mod_obj$data <- TransformTempJoin(data_input, transformed_data, trans_variable)
  }

  if (kpi %in% c("sales_div_tiv","ma","oao","pi","pc","aa","src","web")) {
      mod_obj$data <-
        mod_obj$data %>%
        dplyr::group_by(!!rlang::sym(cross_section)) %>%
        dplyr::mutate(!!DepVar := !!rlang::sym(DepVar) / mean(!!rlang::sym(DepVar), na.rm = TRUE))

      mod_obj$data_transformed$monthly <-
        mod_obj$data_transformed$monthly %>%
        dplyr::group_by(!!rlang::sym(cross_section)) %>%
        dplyr::mutate(!!DepVar := !!rlang::sym(DepVar) / mean(!!rlang::sym(DepVar), na.rm = TRUE))
    }

  return(mod_obj)

}
