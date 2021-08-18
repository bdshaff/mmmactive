#' Dummy Variables
#'
#' This adds dummy variables to data and modifies the spec
#'
#' @param mod_obj - model object
#'
#' @importFrom magrittr %>%
#' @return mod_obj
#' @export


Add_Dummy = function(mod_obj){
  cs <- mod_obj$cs
  ts <- mod_obj$Time

  data = mod_obj$data
  spec = mod_obj$spec

  dummy_spec = mod_obj$dummy_spec

  for(i in seq_along(rownames(dummy_spec))){

    dummy_spec_row = dummy_spec[i,]
    cat("Adding dummy:",paste(apply(dummy_spec_row, 2, as.character)),"\n")

    from = dummy_spec_row$from
    to = dummy_spec_row$to
    css = dummy_spec_row$css

    dum_name = janitor::make_clean_names(str_c(str_c("DUM_",from,"_",to),"_",css))

    data =
      data %>%
      mutate(
        !!dum_name := if_else(between(!!sym(ts), as.Date(from), as.Date(to)) & !!sym(cs) %in% str_split(css,",")[[1]], 1, 0)
      )

    spec =
      bind_rows(spec, data.frame(Orig_Variable = dum_name,
                                 Trans_Variable = dum_name,
                                 AggregateVariable = "Base",
                                 Variable_Type = "Dummy",
                                 Include = 1,
                                 VaryBy = "None",
                                 Prior_Mean = 0,
                                 Prior_SD = 1))

  }

  mod_obj$data = data
  mod_obj$spec = spec
  return(mod_obj)
}
