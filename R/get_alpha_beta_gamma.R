#' get_alpha_beta_gamma
#'
#' @param channel_name - channel_name
#' @param fit_curves - fit_curves
#'
#' @export

get_alpha_beta_gamma = function(channel_name, fit_curves){

  lookup =
    fit_curves$tv %>%
    mutate(NTV = c("A","B","C")) %>%
    pivot_longer(-NTV, values_to = "Reach", names_to = "Param") %>%
    mutate(Param = str_c(str_sub(Param, start = 2), "+")) %>%
    arrange(Param)

  lookup_dt1 =
    fit_curves$displayt1 %>%
    mutate(NTV = c("A","B","C")) %>%
    pivot_longer(-NTV, values_to = "Reach", names_to = "Param") %>%
    mutate(Param = str_c(str_sub(Param, start = 2), "+")) %>%
    arrange(Param)

  lookup_dt2 <-
    fit_curves$displayt2 %>%
    mutate(NTV = c("A","B","C")) %>%
    pivot_longer(-NTV, values_to = "Reach", names_to = "Param") %>%
    mutate(Param = str_c(str_sub(Param, start = 2), "+")) %>%
    arrange(Param)

  lookup_st <-
    fit_curves$st %>%
    mutate(NTV = c("A","B","C")) %>%
    pivot_longer(-NTV, values_to = "Reach", names_to = "Param") %>%
    mutate(Param = str_c(str_sub(Param, start = 2), "+")) %>%
    arrange(Param)

  lookup_ad <-
    fit_curves$addressable %>%
    mutate(NTV = c("A","B","C")) %>%
    pivot_longer(-NTV, values_to = "Reach", names_to = "Param") %>%
    mutate(Param = str_c(str_sub(Param, start = 2), "+")) %>%
    arrange(Param)


  if ((grepl("DIGITAL", channel_name) & grepl("T1", channel_name))) {
    Alpha <- createABG(lookup_dt1, "A")
    Beta <- createABG(lookup_dt1, "B")
    Gamma <- createABG(lookup_dt1, "C")
  } else if ((grepl("SOCIAL DISPLAY", channel_name) & grepl("T1", channel_name))) {
    Alpha <- createABG(lookup_dt1, "A")
    Beta <- createABG(lookup_dt1, "B")
    Gamma <- createABG(lookup_dt1, "C")
  } else if ((grepl("SOCIAL DISPLAY", channel_name) & grepl("T2", channel_name))) {
    Alpha <- createABG(lookup_dt2, "A")
    Beta <- createABG(lookup_dt2, "B")
    Gamma <- createABG(lookup_dt2, "C")
  } else if ((grepl("DIGITAL", channel_name) & grepl("T2", channel_name))) {
    Alpha <- createABG(lookup_dt2, "A")
    Beta <- createABG(lookup_dt2, "B")
    Gamma <- createABG(lookup_dt2, "C")
  } else if (grepl("STREAMING", channel_name)) {
    Alpha <- createABG(lookup_st, "A")
    Beta <- createABG(lookup_st, "B")
    Gamma <- createABG(lookup_st, "C")
  } else if (grepl("SOCIAL VIDEO", channel_name)) {
    Alpha <- createABG(lookup_st, "A")
    Beta <- createABG(lookup_st, "B")
    Gamma <- createABG(lookup_st, "C")
  } else if (grepl("SOCIAL", channel_name)) {
    Alpha <- createABG(lookup_st, "A")
    Beta <- createABG(lookup_st, "B")
    Gamma <- createABG(lookup_st, "C")
  } else if (grepl("ADDRESSABLE", channel_name)) {
    Alpha <- createABG(lookup_ad, "A")
    Beta <- createABG(lookup_ad, "B")
    Gamma <- createABG(lookup_ad, "C")
  } else if (!grepl("STREAMING", channel_name) & !grepl("ADDRESSABLE", channel_name) & !grepl("DIGITAL", channel_name) & !grepl("SOCIAL", channel_name)) {
    Alpha <- createABG(lookup, "A")
    Beta <- createABG(lookup, "B")
    Gamma <- createABG(lookup, "C")
  } else {
    Alpha <- createABG(lookup, "A")
    Beta <- createABG(lookup, "B")
    Gamma <- createABG(lookup, "C")
  }

  return(list(Alpha = Alpha,
              Beta = Beta,
              Gamma = Gamma))

}
