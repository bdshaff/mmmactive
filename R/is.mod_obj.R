#' Test mod_obj
#'
#' @param x - mod_obj
#'
#' @export

is.mod_obj = function(x){
  sum(attributes(x)$class == c("list", "mod_obj")) == 2
}
