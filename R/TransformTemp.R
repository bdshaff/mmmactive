#' TransformTemp
#'
#' @param data_tmp - weekly or monthly input data
#' @param spec_tmp - spec
#' @param fit_curves - fit curves
#' @param print - logical
#' @param tmp - time agg level
#' @param cross_section - cross_section level
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

  spec_tmp <- spec_tmp %>% dplyr::filter(Orig_Variable %in% names(data_tmp[[tmp]]))
  tmpDF <- data_tmp[[tmp]] %>% dplyr::select(tidyselect::one_of(spec_tmp$Orig_Variable))

  tmpDFsplit <- tmpDF %>%
    dplyr::group_by(!!rlang::sym(cross_section)) %>%
    dplyr::group_split()

  tmpDFtransform <-
    purrr::map(tmpDFsplit, ~ TransformSplit(.x,
      spec_split = spec_tmp,
      fit_curves = fit_curves,
      print = print
    )) %>% dplyr::bind_rows()

  tmpDF <- tmpDF %>% dplyr::bind_cols(tmpDFtransform)

  return(tmpDF)
}
