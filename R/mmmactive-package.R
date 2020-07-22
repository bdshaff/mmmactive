#' mmmactive: Implementation of MSMP framework for MMM workflows.
#'
#'
#' This package implement a set of functions for MMM projects following the MSMP (Marketing Science Modeling Platform) framework.
#' These functions are designed around a data structure of class `mod_obj` which is generally the first argument. The core functions of the
#' package are named after a key step of an MMM workflow.
#'
#' Getting familiar with the `mod_obj` : vignette("mod_obj_class_vignette", "mmmactive")
#'
#' \enumerate{
#'       \item \code{Load_Data} : vignette("LoadingData_vignette", "mmmactive")
#'       \item \code{Transform} : vignette("Transform_vignette", "mmmactive")
#'       \item \code{Run_Model_Panel} : vignette("Modeling_vignette", "mmmactive")
#'       \item \code{Decomp + Unnest}
#'       \item \code{Response}
#'       \item \code{Media Mix Optimization}
#' }
#'
#'
#' @author
#'
#' \itemize{
#'      \item Ben Denis Shaffer (Maintainer),
#'      \item Mansi Sheth,
#'      \item Aiden Hu
#'      \item Julia Liu
#'}
#'
#'
#'
#' @section
#' Vignettes:
#' mmmactive package: browseVignettes(package = "mmmactive")
#'
#' @import lubridate
#' @import stringr
#'
#' @docType package
#' @name mmmactive
NULL
