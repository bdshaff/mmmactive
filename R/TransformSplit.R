#' TransformSplit
#'
#' @param data_split
#' @param spec_split
#' @param fit_curves
#' @param print
#'

TransformSplit = function(data_split = NULL, spec_split = NULL, fit_curves = NULL, print = TRUE){
  require(tidyverse)

  if(is.null(data_split)){
    warning("Input data_split is NULL")
  }
  if(is.null(spec_split)){
    warning("Input spec_split is NULL")
  }
  if(is.null(fit_curves)){
    warning("Input fit_curves is NULL")
  }

  if(print){
    cat(str_c("Crossection Level: ", unique(data_split$vehicle), "\n"))
  }

  data_split_transform =
    map(1:nrow(spec_split), ~TransformVar(data_vector = data_split[[spec_split$Orig_Variable[.x]]],
                                          spec_row = spec_split[.x,],
                                          fit_curves = fit_curves,
                                          print = print))

  names(data_split_transform) <- spec_split$Trans_Variable
  data_split_transform_df = data.frame(data_split_transform)

  return(data_split_transform_df)

}
