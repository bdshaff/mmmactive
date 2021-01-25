#' solve_abc
#'
#' @param reach
#'
#' @export


solve_abc = function(reach){

  dummy <- tribble(
    ~term, ~estimate, ~std.error, ~statistic, ~p.value,
    "a", 0, 0, 0, 0,
    "b", 0, 0, 0, 0,
    "c", 0, 0, 0, 0
  )

  model <- try(onls::onls(
    Contribution ~ a / (1 + b * ((Spend / 1000))^c),
    start = list(a = 80, b = 1, c = -1),
    data = reach,
    lower = c(a = 0.1, b = 0.1, c = -2)
  ))

  m <- try(broom::tidy(model))

  if(is_tibble(m)){
    return(m)
  }else if(class(m) == "try-error"){
    message("failed for solve for abc")
    return(dummy)
  }


}
