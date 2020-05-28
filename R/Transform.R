#' Transform
#'
#' @param obj - model object
#' @param print - TRUE or FALSE
#'

Transform = function(obj = NULL, print = TRUE){

  if(is.null(obj)){
    warning("Please supply model object")
  }

  data_input = obj$data_input
  spec = obj$spec
  fit_curves = obj$fit_curves
  spec_trans = spec %>% filter(Transform == "Y")
  cross_section = obj$cs
  trans_variable = obj$spec$Trans_Variable


  if(is.null(cross_section)){
    print("no crossection")
    if(length(data_input) == 1){

      data_input = data_input[[1]]

      DFFS =
        map(1:nrow(spec_trans), ~TransfromVar(data_vector = data_input[[spec_trans$Orig_Variable[.x]]],
                                              spec_row = spec_trans[.x,],
                                              fit_curves = fit_curves,
                                              print = print))
      names(DFFS) <- spec_trans$Trans_Variable

      obj$data_transformed = list(data.frame(DFFS))
      obj$data <- bind_cols(data_input, data.frame(DFFS))
      return(obj)
    }

  }else{

    if(print){
      cat(str_c("Crossection Variable: ", cross_section, "\n"))
    }

    if("data.frame" %in% class(data_input)){
      DFsplit = data_input %>% group_by(!!sym(cross_section)) %>% group_split()
      DFFS = map(DFsplit, ~TransformSplit(.x, spec_split = spec_tmp,
                                          fit_curves = fit_curves,
                                          print = print)) %>% bind_rows()
      obj$data_transformed = DFFS
      obj$data = data_input %>% bind_cols(DFFS)
      return(obj)

    }else if(class(data_input) == "list"){
      DFFS = map(names(data_input), ~TransformTemp(data_tmp = data_input,
                                                   spec_tmp = spec_trans,
                                                   tmp = .x,
                                                   fit_curves = fit_curves,
                                                   cross_section = cross_section,
                                                   print = print)) %>% set_names(names(data_input))
      obj$data_transformed = DFFS

      obj$data = TransformTempJoin(data_input, DFFS, trans_variable)

      return(obj)
    }
  }

}
