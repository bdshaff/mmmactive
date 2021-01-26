#' Response_Curves2
#'
#' @param mod_obj - mod_obj
#'
#' @export


Response_Curves2 = function(mod_obj){

  grps_data =
    mod_obj$data_input$weekly %>%
    ungroup() %>%
    group_by(week) %>%
    summarise_if(is.numeric, sum) %>%
    select(week, contains(unique(mod_obj$reach_curve_spec$Orig_Variable))) %>%
    pivot_longer(-week, names_to = "Orig_Variable", values_to = "GRPs") %>%
    full_join(mod_obj$reach_curve_spec %>% ungroup() %>% select(Orig_Variable, Model)) %>%
    pivot_wider(names_from = c(Model, Orig_Variable), values_from = GRPs) %>%
    filter(week <= mod_obj$EndDate, week >= mod_obj$EndDate - 721)

  AllReach = ReachCurveCalc3(mod_obj$reach_curve_spec, grps_data, mod_obj$fit_curves)

  abcs =
    map(AllReach, ~solve_abc(.x$reach)) %>%
    bind_rows() %>% select(estimate)

  abcs <- data.frame(matrix(unlist(abcs), nrow = 3))
  names(abcs) <- names(AllReach)

  spend_contrib =
    map(AllReach, ~.x$reach_curve_spec_row %>% ungroup %>% select(spend, contribution)) %>%
    bind_rows() %>%
    t() %>% data.frame()

  names(spend_contrib) <- names(AllReach)
  abcs = bind_rows(abcs, spend_contrib)
  rownames(abcs) = c("A","B","C","Spend","Real Contribution")

  abcs["Adjusted A",] = abcs["Real Contribution",] / mapply(Reach, abcs["A", ], abcs["B", ], abcs["C", ], abcs["Spend", ]) * abcs["A", ]
  abcs["Contribution",] = mapply(Reach, abcs["Adjusted A", ], abcs["B", ], abcs["C", ], abcs["Spend", ])

  msrp =
    mod_obj$data_input$monthly %>%
    ungroup() %>%
    mutate(FY = str_sub(month - months(3), 1, 4), .before = month) %>%
    filter(FY == mod_obj$reach_curve_spec$FY[1]) %>%
    summarise(mean(msrp)) %>%
    as.numeric()

  abcs["MSRP",] = msrp
  abcs["Revenue",] = abcs["MSRP",] * abcs["Contribution",]
  abcs["ROI",] = abcs["Revenue",]/abcs["Spend",]
  abcs["mROI",] = ((mapply(Reach, abcs["Adjusted A", ], abcs["B", ], abcs["C", ], abcs["Spend", ] + 1000) * msrp) - abcs["Revenue",]) / 10000

  as_tibble(abcs)

  mod_obj$response = list(abcs = abcs,
                          all_reach = AllReach)

  return(mod_obj)
}
