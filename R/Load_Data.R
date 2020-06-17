#' #' Load_Data
#' #'
#' #' @param obj - model object
#' #' @param input_file_ModelData - file parth
#' #'
#'
#' Load_Data = function(obj, input_file_ModelData){
#'   datasheets = excel_sheets(input_file_ModelData)
#'
#'   obj$data_input =
#'     map(datasheets, ~ read_xlsx(input_file_ModelData, sheet = .x)) %>%
#'     set_names(str_extract(datasheets, "weekly|monthly"))
#'
#'   groups = names(obj$data_group_selector)
#'
#'   if("weekly" %in% str_extract(datasheets, "weekly|monthly")){
#'
#'     obj$data_input$weekly =
#'       obj$data_input$weekly %>%
#'       mutate(week = ymd(week)) %>%
#'       group_by_at(vars(groups))
#'
#'     for(g in groups){
#'
#'       keep_vec = unlist(obj$data_input$weekly[,g]) %in% obj$data_group_selector[[g]]
#'       obj$data_input$weekly = obj$data_input$weekly[keep_vec,]
#'
#'     }
#'
#'   }
#'
#'   if("monthly" %in% str_extract(datasheets, "weekly|monthly")){
#'
#'     obj$data_input$monthly =
#'       obj$data_input$monthly %>%
#'       mutate(month = ymd(month)) %>%
#'       group_by_at(vars(groups))
#'
#'     for(g in groups){
#'
#'       keep_vec = unlist(obj$data_input$monthly[,g]) %in% obj$data_group_selector[[g]]
#'       obj$data_input$monthly = obj$data_input$monthly[keep_vec,]
#'
#'     }
#'
#'   }
#'
#'   return(obj)
#'
#' }
