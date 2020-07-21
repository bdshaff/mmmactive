#' Create mod_obj
#'
#' @importFrom magrittr %>%
#' @export

create_mod_obj <- function() {
  mod_obj <- list()
  class(mod_obj) <- c("list", "mod_obj")
  return(mod_obj)
}
