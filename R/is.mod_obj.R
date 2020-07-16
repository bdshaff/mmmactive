#' Test mod_obj
#'
#' @export

is.mod_obj = function(x){
  sum(attributes(mod_obj)$class == c("list", "mod_obj")) == 2
}
