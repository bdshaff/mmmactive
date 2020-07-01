#' Transform
#'
#' Run data transformation and combine weekly and monthly level data accosting to the model specification.
#'
#' @param mod_obj - model mod_object
#' @param print - TRUE or FALSE
#'
#' @return mod_obj
#'

Transform <- function(mod_obj = NULL, print = TRUE) {
  if (is.null(mod_obj)) {
    warning("Please supply model mod_object")
  }

  data_input <- mod_obj$data_input
  spec <- mod_obj$spec
  fit_curves <- mod_obj$fit_curves
  spec_trans <- spec %>% filter(Transform == "Y")
  cross_section <- mod_obj$cs
  trans_variable <- mod_obj$spec$Trans_Variable
  kpi <- mod_obj$kpi
  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]


  if (is.null(cross_section)) {
    print("no crossection")
    if (length(data_input) == 1) {
      data_input <- data_input[[1]]

      DFFS <-
        map(1:nrow(spec_trans), ~ TransfromVar(
          data_vector = data_input[[spec_trans$Orig_Variable[.x]]],
          spec_row = spec_trans[.x, ],
          fit_curves = fit_curves,
          print = print
        ))
      names(DFFS) <- spec_trans$Trans_Variable

      mod_obj$data_transformed <- list(data.frame(DFFS))
      mod_obj$data <- bind_cols(data_input, data.frame(DFFS))
      return(mod_obj)
    }
  } else {
    if (print) {
      cat(str_c("Crossection Variable: ", cross_section, "\n"))
    }

    if ("data.frame" %in% class(data_input)) {
      DFsplit <- data_input %>%
        group_by(!!sym(cross_section)) %>%
        group_split()

      DFFS <- map(DFsplit, ~ TransformSplit(.x,
        spec_split = spec_tmp,
        fit_curves = fit_curves,
        print = print
      )) %>% bind_rows()

      mod_obj$data_transformed <- DFFS
      mod_obj$data <- data_input %>% bind_cols(DFFS)
      return(mod_obj)
    } else if (class(data_input) == "list") {
      DFFS <- map(names(data_input), ~ TransformTemp(
        data_tmp = data_input,
        spec_tmp = spec_trans,
        tmp = .x,
        fit_curves = fit_curves,
        cross_section = cross_section,
        print = print
      )) %>% set_names(names(data_input))

      mod_obj$data_transformed <- DFFS
      mod_obj$data <- TransformTempJoin(data_input, DFFS, trans_variable)

      if(kpi == "sales"){
        mod_obj$data =
          mod_obj$data %>%
          group_by(!!sym(cross_section)) %>%
          mutate(!!DepVar := !!sym(DepVar)/mean(!!sym(DepVar), na.rm = TRUE))
      }

      return(mod_obj)
    }
  }
}
