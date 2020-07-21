#' TransformTemp
#'
#' @param data_tmp
#' @param spec_tmp
#' @param fit_curves
#' @param print
#'
#' @return data.frame
#' @export

TransformTemp <- function(data_tmp = NULL, spec_tmp = NULL, tmp = NULL, fit_curves = NULL, cross_section = NULL, print = TRUE) {
  require(tidyverse)

  if (is.null(data_tmp)) {
    warning("Input data_tmp is NULL")
  }
  if (is.null(spec_tmp)) {
    warning("Input spec_tmp is NULL")
  }
  if (is.null(tmp)) {
    warning("Input tmp is NULL")
  }
  if (is.null(fit_curves)) {
    warning("Input fit_curves is NULL")
  }

  spec_tmp <- spec_tmp %>% filter(Orig_Variable %in% names(data_tmp[[tmp]]))
  tmpDF <- data_tmp[[tmp]] %>% select(one_of(spec_tmp$Orig_Variable))

  tmpDFsplit <- tmpDF %>%
    group_by(!!sym(cross_section)) %>%
    group_split()

  tmpDFtransform <-
    map(tmpDFsplit, ~ TransformSplit(.x,
      spec_split = spec_tmp,
      fit_curves = fit_curves,
      print = print
    )) %>% bind_rows()

  tmpDF <- tmpDF %>% bind_cols(tmpDFtransform)

  return(tmpDF)
}
