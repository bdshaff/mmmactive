#' Validate mod_obj
#'

validate_mod_obj <- function(mod_obj) {
  return("msmp" %in% class(mod_obj))
}
