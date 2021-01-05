#' createABG
#'
#' @param lookup - lookup
#' @param type - type
#'
#' @export

createABG = function (lookup, type) {
  tmp <- lookup %>% filter(NTV == type) %>% mutate(Param = as.numeric(Param)) %>%
    arrange(Param)
  l = tmp$Reach
  finder <- function(x) {
    return(l[x])
  }
  return(finder)
}
