create_mod_obj = function(){
  mod_obj = list()
  return(mod_obj)
}

add_group_selector = function(mod_obj, vehicles, regions){
  mod_obj$data_group_selector =
    list(vehicle = vehicles,
         region = regions)
  return(mod_obj)
}

activate_model_setup = function(mod_obj, input_file_ModelSetup){
  Model_setup = read_csv(input_file_ModelSetup, col_types = cols())
  mod_obj$ModelForm = Model_setup$Value[Model_setup$Parameter == "ModelForm"]
  mod_obj$Panel = Model_setup$Value[Model_setup$Parameter == "Panel"]
  mod_obj$Time = Model_setup$Value[Model_setup$Parameter == "Time"]
  mod_obj$BeginDate = mdy(Model_setup$Value[Model_setup$Parameter == "BeginDate"])
  mod_obj$EndDate = mdy(Model_setup$Value[Model_setup$Parameter == "EndDate"])
  mod_obj$SimStart = mdy(Model_setup$Value[Model_setup$Parameter == "SimStart"])
  mod_obj$SimEnd = mdy(Model_setup$Value[Model_setup$Parameter == "SimEnd"])
  mod_obj$mroi_step = as.numeric(Model_setup$Value[Model_setup$Parameter == "Mroi"])
  mod_obj$cs = Model_setup$Value[Model_setup$Parameter == "Crossection"]
  return(mod_obj)
}

activate_model_spec = function(mod_obj, input_file_ModelSpec){
  mod_obj$spec = read_csv(input_file_ModelSpec, col_types = cols()) %>% filter(Include == 1)
  return(mod_obj)
}

SetDateRange = function(mod_obj, na.rm = TRUE){
  mod_obj$data =
    mod_obj$data %>%
    filter(!!sym(mod_obj$Time) >= mod_obj$BeginDate,
           !!sym(mod_obj$Time) <= mod_obj$EndDate)

  if(na.rm){
    mod_obj$data[is.na(mod_obj$data)] = 0
  }

  return(mod_obj)
}

Load_Data = function(obj, input_file_ModelData){
  datasheets = excel_sheets(input_file_ModelData)

  obj$data_input =
    map(datasheets, ~ read_xlsx(input_file_ModelData, sheet = .x)) %>%
    set_names(str_extract(datasheets, "weekly|monthly"))

  groups = names(obj$data_group_selector)

  if("weekly" %in% str_extract(datasheets, "weekly|monthly")){

    obj$data_input$weekly =
      obj$data_input$weekly %>%
      mutate(week = ymd(week)) %>%
      group_by_at(vars(groups))

    for(g in groups){

      keep_vec = unlist(obj$data_input$weekly[,g]) %in% obj$data_group_selector[[g]]
      obj$data_input$weekly = obj$data_input$weekly[keep_vec,]

    }

  }

  if("monthly" %in% str_extract(datasheets, "weekly|monthly")){

    obj$data_input$monthly =
      obj$data_input$monthly %>%
      mutate(month = ymd(month)) %>%
      group_by_at(vars(groups))

    for(g in groups){

      keep_vec = unlist(obj$data_input$monthly[,g]) %in% obj$data_group_selector[[g]]
      obj$data_input$monthly = obj$data_input$monthly[keep_vec,]

    }

  }

  return(obj)

}

Load_FitCurves = function(obj, input_file_ModelFitCurves){

  datasheets = excel_sheets(input_file_ModelFitCurves)

  obj$fit_curves =
    map(datasheets, ~ clean_names(read_xlsx(input_file_ModelFitCurves, sheet = .x))) %>%
    set_names(str_replace(str_extract(datasheets, "_[a-zA-Z0-9.-]*$"), "_", ""))

  return(obj)

}

Decomp <- function(obj, incl_spent = FALSE) {

  x <- obj$data
  spec <- obj$spec
  b <- obj$Model$coefficients
  depvar <- obj$spec$Trans_Variable[obj$spec$Variable_Type == "Dependent"]
  decomp <- x[, c(obj$Time, depvar)]

  calc_var <- spec$Trans_Variable[spec$Variable_Type != "Dependent"]
  dcmp_var <- paste("d", calc_var, sep = "_")
  #print(dcmp_var)

  if(length(calc_var) <= 0) {
    stop("Please specify which variables you would like to decomp. ")
  }

  if(toupper(obj$ModelForm) == "LIN_LIN") {
    for(i in 1:length(calc_var)) {
      cat("Calculate decomposition of ", calc_var[i], "\n")
      decomp[[dcmp_var[i]]] <- b$Estimate[b$Variables == calc_var[i]] * x[[calc_var[i] ]]

    }

  } else if(toupper(obj$ModelForm) == "LOG_LOG") {
    stop("Sorry, haven't implemented yet. ")
    cat("The reference point is based on ", Reference, "\n")
    for(i in 1:length(calc_var)) {

    }

  }

  # include spent variable
  if(incl_spent) {
    for (i in 1:length(calc_var)) {
      spend_var <- spec$Spent_Variable[spec$Trans_Variable == calc_var[i]]
      if( !is.na(spend_var) ) {
        decomp[[spend_var]] <- x[[ spend_var ]]
      }
    }
  }

  obj$Decomposition <- decomp
  return(obj)
}

Transform = function(obj = NULL, print = TRUE){

  if(is.null(obj)){
    warning("Please supply model object")
  }

  data_input = obj$data_input
  spec = obj$spec
  fit_curves = obj$fit_curves
  spec_trans = spec %>% filter(Transform == "Y")
  cross_section = obj$cs
  trans_variable = obj$spec$Trans_Variable


  if(is.null(cross_section)){
    print("no crossection")
    if(length(data_input) == 1){

      data_input = data_input[[1]]

      DFFS =
        map(1:nrow(spec_trans), ~TransfromVar(data_vector = data_input[[spec_trans$Orig_Variable[.x]]],
                                              spec_row = spec_trans[.x,],
                                              fit_curves = fit_curves,
                                              print = print))
      names(DFFS) <- spec_trans$Trans_Variable

      obj$data_transformed = list(data.frame(DFFS))
      obj$data <- bind_cols(data_input, data.frame(DFFS))
      return(obj)
    }

  }else{

    if(print){
      cat(str_c("Crossection Variable: ", cross_section, "\n"))
    }

    if("data.frame" %in% class(data_input)){
      DFsplit = data_input %>% group_by(!!sym(cross_section)) %>% group_split()
      DFFS = map(DFsplit, ~TransformSplit(.x, spec_split = spec_tmp,
                                          fit_curves = fit_curves,
                                          print = print)) %>% bind_rows()
      obj$data_transformed = DFFS
      obj$data = data_input %>% bind_cols(DFFS)
      return(obj)

    }else if(class(data_input) == "list"){
      DFFS = map(names(data_input), ~TransformTemp(data_tmp = data_input,
                                                   spec_tmp = spec_trans,
                                                   tmp = .x,
                                                   fit_curves = fit_curves,
                                                   cross_section = cross_section,
                                                   print = print)) %>% set_names(names(data_input))
      obj$data_transformed = DFFS

      obj$data = TransformTempJoin(data_input, DFFS, trans_variable)

      return(obj)
    }
  }

}

TransformSplit = function(data_split = NULL, spec_split = NULL, fit_curves = NULL, print = TRUE){
  require(tidyverse)

  if(is.null(data_split)){
    warning("Input data_split is NULL")
  }
  if(is.null(spec_split)){
    warning("Input spec_split is NULL")
  }
  if(is.null(fit_curves)){
    warning("Input fit_curves is NULL")
  }

  if(print){
    cat(str_c("Crossection Level: ", unique(data_split$vehicle), "\n"))
  }

  data_split_transform =
    map(1:nrow(spec_split), ~TransfromVar(data_vector = data_split[[spec_split$Orig_Variable[.x]]],
                                          spec_row = spec_split[.x,],
                                          fit_curves = fit_curves,
                                          print = print))

  names(data_split_transform) <- spec_split$Trans_Variable
  data_split_transform_df = data.frame(data_split_transform)

  return(data_split_transform_df)

}

TransformTemp = function(data_tmp = NULL, spec_tmp = NULL, tmp = NULL, fit_curves = NULL, cross_section = NULL, print = TRUE){
  require(tidyverse)

  if(is.null(data_tmp)){
    warning("Input data_tmp is NULL")
  }
  if(is.null(spec_tmp)){
    warning("Input spec_tmp is NULL")
  }
  if(is.null(tmp)){
    warning("Input tmp is NULL")
  }
  if(is.null(fit_curves)){
    warning("Input fit_curves is NULL")
  }

  spec_tmp = spec_tmp %>% filter(Orig_Variable %in% names(data_tmp[[tmp]]))
  tmpDF = data_tmp[[tmp]] %>% select(one_of(spec_tmp$Orig_Variable))

  #tmpDFsplit = tmpDF %>% group_by(vehicle) %>% group_split()
  tmpDFsplit = tmpDF %>% group_by(!!sym(cross_section)) %>% group_split()

  tmpDFtransform =
    map(tmpDFsplit, ~TransformSplit(.x, spec_split = spec_tmp,
                                    fit_curves = fit_curves,
                                    print = print)) %>% bind_rows()

  tmpDF = tmpDF %>% bind_cols(tmpDFtransform)

  return(tmpDF)

}

TransformTempJoin = function(data_input, DFFS, trans_variable){

  DF = list()
  for(tmp in names(data_input)){
    DF[[tmp]] = data_input[[tmp]] %>% bind_cols(DFFS[[tmp]])
    cat(str_c("The ", tmp, " level data transformed and augmented \n"))
  }

  weeklyDF =
    DF$weekly %>%
    mutate(month = floor_date(week, unit = "month")) %>%
    group_by(vehicle, region, month) %>%
    select(one_of(trans_variable)) %>%
    summarise_all(.funs = function(x) mean(x, na.rm = TRUE)) %>%
    group_by(vehicle, region, month)


  monthlyDF =
    DF$monthly %>%
    mutate(month = floor_date(month, unit = "month")) %>%
    group_by(vehicle, region, month) %>%
    select(one_of(trans_variable)) %>%
    summarise_all(.funs = function(x) sum(x, na.rm = TRUE)) %>%
    group_by(vehicle, region, month)

  joinedDF =  monthlyDF %>% full_join(weeklyDF)

  cat(str_c(names(data_input), " data releveled and joined \n"))

  return(joinedDF)

}

TransfromVar = function(data_vector = NULL, spec_row = NULL, fit_curves = NULL, print = TRUE){
  require(tidyverse)

  if(is.null(data_vector)){
    warning("TransfromVar Input data_vector is NULL")
  }else if(!is.numeric(data_vector)){
    warning("TransfromVar Input data_vector is not of type numeric. Casting to numeric. Please check data!")
    data_vector = as.numeric(data_vector)

  }

  if(is.null(spec_row)){
    warning("TransfromVar Input spec_row is NULL")
  }
  if(is.null(fit_curves)){
    warning("TransfromVar Input fit_curves is NULL")
  }


  type = toupper(unlist(str_split(spec_row$TransformType, "_")))

  data_vector_transform = data_vector

  for(i in 1:length(type)){

    if(print){
      cat(type[i], " transform ", spec_row$Orig_Variable, "\n")
    }

    if(type[i] == "ADSTOCK"){
      data_vector_transform <- AdStockPD(data_vector_transform, spec_row$Decay, spec_row$Period)
    }

    if(type[i] == "ADR"){
      if(str_detect(spec_row$Trans_Variable,"tv")){
        data_vector_transform <- AdResponse(data_vector_transform, fit_curves$tv, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      }else if(str_detect(spec_row$Trans_Variable,"display")){
        data_vector_transform <- AdResponse(data_vector_transform, fit_curves$displayt1, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      }else if(str_detect(spec_row$Trans_Variable,"addressable")){
        data_vector_transform <- AdResponse(data_vector_transform, fit_curves$addressable, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      }else if(str_detect(spec_row$Trans_Variable,"streaming")){
        data_vector_transform <- AdResponse(data_vector_transform, fit_curves$st, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      }else{
        warning("applying the TV fit curve")
        data_vector_transform <- AdResponse(data_vector_transform, fit_curves$tv, c(spec_row$Effective, spec_row$Recency, spec_row$Period, spec_row$Decay))
      }
    }

    if(type[i] == "LAG"){
      data_vector_transform <- lag(data_vector_transform, spec_row$Lag, default = 0)
    }

    if(type[i] == "LOG"){
      data_vector_transform <- log(data_vector_transform*spec_row$Scale + 1)
    }

    if(type[i] == "POLY"){
      data_vector_transform <- myPoly(data_vector_transform, spec_row$Alpha)
    }

    if(type[i] == "MA"){
      data_vector_transform <- rollmean(data_vector_transform, spec_row$Window, align = "right", fill = NA)
      data_vector_transform[which(is.na(data_vector_transform))] <- data_vector_transform[which(is.na(data_vector_transform))]
    }

    if(type[i] == "STEIN"){
      data_vector_transform <- shrinker(data_vector_transform, bw = spec_row$Window, trim = spec_row$Trim)
    }

    if(type[i] == "CPT"){
      data_vector_transform <-
        cpt.meanvar(data_vector_transform, minseglen = 6, penalty = "CROPS", pen.value = c(0, 100), method = "PELT")
    }

    if(type[i] == "POWER"){
      data_vector_transform <- (data_vector_transform)^spec_row$Power
    }

    if(type[i] == "NONE"){
      data_vector_transform <- data_vector_transform
    }

  }

  return(data_vector_transform)

}

AdResponse <- function(afGRPsMat, afCoeffsMat, params){
  # Generate the Effective Cover of a vector of input GRPs

  # afGRPs = vector of GRP data
  # afCoeffsMat = matrix (10 cols by 3 rows) of coefficients for reach models from 1+ to 10+
  # nEffFreq = integer value of Effective Frequency Parameter
  # nRecFreq = integer value of Recency Frequency Parameter
  # nPeriod = integer value of Response Period Parameter
  # fDecay = decimal value of decay rate parameter

  nEffFreq <- params[1]
  nRecFreq <- params[2]
  nPeriod <- params[3]
  fDecay <- params[4]

  # Define output matrix size
  afGRPsMat = as.matrix(afGRPsMat)
  afAdResponse = matrix(1:nrow(afGRPsMat))
  fEffGRPs = RollingSum(afGRPsMat, fDecay, nPeriod)
  fTotalEffGRPs = AdStock(afGRPsMat, fDecay)

  a = afCoeffsMat[[1, nEffFreq]]
  b = afCoeffsMat[[2, nEffFreq]]
  c = afCoeffsMat[[3, nEffFreq]]

  if(nRecFreq == 0){
    afAdResponse[,1] = Reach(a, b, c, fTotalEffGRPs)
    return(afAdResponse);
  }

  if(nRecFreq == nEffFreq){
    afAdResponse[,1] = Reach(a, b, c, fEffGRPs)
    return(afAdResponse);
  }

  fTotalEffGRPs_rounded = round(fTotalEffGRPs[, 1], digits = 6)
  fEffGRPs_rounded = round(fEffGRPs[, 1], digits = 6)
  a_s = afCoeffsMat[1,]
  b_s = afCoeffsMat[2,]
  c_s = afCoeffsMat[3,]
  f = matrix(0, nrow = nrow(afGRPsMat), ncol=1)
  for (k in nRecFreq:(nEffFreq - 1)) {
    one = Reach(a_s[[k]], b_s[[k]], c_s[[k]], fEffGRPs)
    two = Reach(a_s[[k + 1]], b_s[[k + 1]], c_s[[k + 1]], fEffGRPs)
    three = Reach(a_s[[nEffFreq - k]], b_s[[nEffFreq - k]], c_s[[nEffFreq - k]], fTotalEffGRPs - fEffGRPs)
    f = f + (one-two)*three/100
  }


  for (i in 1:nrow(afGRPsMat)) {
    # Calculate the x+y(<x) Model
    if (fTotalEffGRPs_rounded[i] == fEffGRPs_rounded[i]) {
      # There is no extra history outside of the recency window
      afAdResponse[i ,1] = Reach(a, b, c, fEffGRPs[i, 1])
    } else {
      # There is extra history outside of the recency window to be considered
      afAdResponse[i, 1] = f[i, 1] + Reach(a, b, c, fEffGRPs[i, 1])
    }
  }

  # Return the calculated AdResponse
  return(as.vector(afAdResponse))
}

RollingSum = function(afGRPsMat, fDecay, nPeriod){
  # Create a rolling sum of an AdStock to a vector of data

  # afGRPsMat = matrix (vertical vector) of GRP data fDecay = single data point, decimal decay rate of media nPeriod
  # = integer value of number of observations to sum over

  weights = vector(length=nPeriod)
  decay = 1 - fDecay
  for(i in 1:nPeriod){
    weights[i] <- decay^(nPeriod - i)
  }

  afRollingSum = roll_sumr(afGRPsMat, weights=weights, normalize=F)
  afRollingSum[1:nPeriod-1] <- afGRPsMat[1:nPeriod-1]

  # Return the rolling sum of data
  return(afRollingSum)

  # Example use of Function test=RollingSum(afGRPs, 0.15, 4)
}

AdStock = function(afGRPsMat, fdecayRate){
  # Generate matrix of AdStocked/Decayed GRPs as a function of input GRPs and decay rate to a value
  # y(t)=y(t-1)*d + x(t)

  # afGRPs = matrix (vertical vector) of GRP Data
  # fdecayRate = decimal version of decay rate

  # Create output matrix base on size of input
  afAdStockedGRPsMat = matrix(1:nrow(afGRPsMat))

  # first observations are equal
  afAdStockedGRPsMat[1, 1] = afGRPsMat[1, 1]

  # loop through calculating AdStocked GRPs
  decay = 1 - fdecayRate
  value = afGRPsMat[1, 1]
  for (x in 2:nrow(afGRPsMat)) {
    value = value * decay + afGRPsMat[x, 1]
    afAdStockedGRPsMat[x, 1] = value
  }

  # Return AdStocked GRPs matrix
  return(afAdStockedGRPsMat)

  # Example use of AdStock Function test=AdStock(data.matric(GRPs[3]),0.15)

}

Reach <- function(fa, fb, fc, fGRPs){
  # Return reach data subject to fitted formula to a value r=a/(1+b*(GRPs/1000)^c)

  # fa = Alpha Coefficient in reach model
  # fb = Beta Coefficient in reach model
  # fc = Gamma Coefficient in reach model
  # fGRPs = single data point of GRPs at which to calculate reach

  fReach <- as.numeric(fGRPs > 0) * fa/(1 + fb * (fGRPs/1000)^fc)
  # Return calculated reach value
  return(fReach)

  # Example Use of Reach Function (solution=1.222065) test=Reach(0.79,-1,0.5,125)

}

AdStockPD <- function(data, i, p){
  rowSums(as.data.frame(embed(c(rep(NA, p), data), p + 1) %*% ((1 - i) ^ seq(0,p,1))),na.rm = F)->output
  output[is.na(output)] <- 0
  return(output)
}

Run_Model <- function(obj, Method = "Bayes"){

  spec <- obj$spec
  x <- obj$data

  DepVar <- spec$Trans_Variable[spec$Variable_Type == "Dependent"]
  IV <- spec$Trans_Variable[spec$Variable_Type != "Dependent"]
  spec <- spec[spec$Variable_Type != "Dependent", ]
  priors <- dplyr::select(spec, Orig_Variable, Trans_Variable, Prior_Mean, Prior_SD)

  # do some checking and make sure the model variables (DepVar and IV) are in data
  if(DepVar %in% names(x)) {
    y = x[[DepVar]]
  } else {
    stop("The dependent variable", DepVar, "is not in the model dataset. ")
  }

  if("Intercept" %in% (spec$Trans_Variable)) {
    x$Intercept = 1
  }

  if(FALSE %in% (IV %in% names(x))) {
    stop("Some of the independent variables ",
         setdiff(IV ,names(x)[IV %in% names(x)]),
         " are not in the model x set. Please check _Variables.csv file. ")
  }

  eq <- (paste(IV, collapse =" + "))
  eq <- paste(eq, " -1")
  eq <- paste(DepVar, " ~ ", eq)
  eq <- as.formula(eq)

  eq_lm <- paste(IV[!IV %in% "Intercept"], collapse =" + ")
  eq_lm <- as.formula(paste(DepVar, " ~ ", eq_lm))


  print("calling my_bayes()...")
  bayes_obj <- my_bayes(formula=eq, data=x, priors=priors)
  #  return(bayes_obj)
  obj$Model <- bayes_obj
  obj$Model$act_pred <- x[, c(obj$Time, DepVar)]
  obj$Model$act_pred$predicted <- obj$Model$fitted_value
  obj$Model$act_pred$residual <- obj$Model$residuals
  obj$Model$MAPE <- MAPE(obj$Model$act_pred[[DepVar]], obj$Model$act_pred$predicted)
  obj$lmModel <- lm(eq_lm, data=x)
  obj$Model$VIF <- data.frame(vif(obj$lmModel))
  obj$Model$VIF$variable <- row.names(obj$Model$VIF)
  names(obj$Model$VIF) <- c("VIF", "Variables")
  obj$Model$result_all <- full_join(obj$Model$VIF, obj$Model$coefficients)
  obj$Model$result_all <- obj$Model$result_all[, c("Variables", "VIF", "Estimate","Error", "Tvalue")]
  names(obj$Model$result_all)[names(obj$Model$result_all)=="Variables"] <- "Trans_Variable"
  obj$Model$result_all <- full_join(spec, obj$Model$result_all)

  obj$Model$DW <- durbinWatsonTest(obj$lmModel)
  obj$Model$result_all$R2 <- rep(obj$Model$R2, nrow(obj$Model$result_all))
  obj$Model$result_all$DW <- rep(obj$Model$DW$dw, nrow(obj$Model$result_all))
  #  obj$Model$DWtest <- (dwtest(obj$Model$formula, data=obj$data))

  return(obj)
}

my_bayes = function(formula, data, priors=NULL){
  big_number <- 10000           # for difuse priors

  DepVar <- all.vars(formula)[1]
  IV <- all.vars(formula)[-1]

  y <- data[[DepVar]]

  formula <- as.formula(formula)
  X <- model.matrix(formula, data=data)
  Xdf <- data.frame(X)
  IV <- names(Xdf)

  if(is.null(priors)) {
    print("priors is not specified. The priors will be set to difuse priors.")
    priors <- data.frame(IV, rep(0, length(IV)), rep(sqrt(big_number), length(IV)))
    names(priors) <- c("Trans_Variable", "Prior_Mean", "Prior_SD")
    priors$Prior_Mean <- as.numeric(priors$Prior_Mean)
    priors$Prior_SD <- as.numeric(priors$Prior_SD)
  }

  # setup diffuse priors and run the model to estimate the model error (sigma2)
  betabar <- rep(0, ncol(X))
  sig2 <- 1
  sd2 <- rep(big_number, ncol(X))
  B <- diag(sd2, ncol(X))
  A <- sig2 * diag(1/sd2, ncol(X))

  ivar <- ncol(X)
  U <- chol(A)         # cholesky root of precision matrix
  W <- rbind(X,U)      # combine the U matrix with design matrix
  nu <- c(y, (U %*% betabar))
  WpW <- t(W) %*% W
  IWpW <- solve(WpW, tol = 1e-25)
  btilde <- IWpW %*% t(W) %*% nu    # basicaly OLS

  yhat <- X %*% btilde
  e2 <- sum((yhat-y)^2)
  sig2_hat <- e2/(nrow(X) - ncol(X))    # this is the estimated sigma squared

  # use the estimate model error to run the final bayes model
  betabar <- priors$Prior_Mean
  sig2 <- sig2_hat
  sd2 <- priors$Prior_SD^2
  B <- diag(sd2, ncol(X))
  A <- sig2 * diag(1/sd2, ncol(X))

  ivar <- ncol(X)
  U <- chol(A)         # cholesky root of precision matrix
  W <- rbind(X,U)      # combine the U matrix with design matrix
  nu <- c(y, (U %*% betabar))
  WpW <- t(W) %*% W
  IWpW <- solve(WpW, tol = 1e-25)
  btilde <- IWpW %*% t(W) %*% nu
  var_cov <- sig2*solve(t(X) %*% X +A, tol = 1e-25)   # this is the error
  error <- sqrt(diag(var_cov))
  tvalue <- btilde/error
  fitted_value <- X %*% btilde
  residuals <- y - fitted_value
  SS_tot <- sum((y-mean(y))^2)
  #  SS_res <- sum((residuals)^2)
  SS_res <- t(residuals) %*% residuals
  sigma <- sqrt(SS_res/(nrow(X) - ncol(X)))
  R2 <- 1-(SS_res/SS_tot)

  coefficients <- data.frame(cbind(as.vector(btilde), as.vector(error)))
  names(coefficients) <- c("Estimate", "Error")
  coefficients$Tvalue <- coefficients$Estimate/coefficients$Error

  coefficients$Variables <- IV
  coefficients <- coefficients[, c("Variables", "Estimate", "Error", "Tvalue")]

  obj <- list(formula = formula, coefficients=coefficients,
              fitted_value = as.vector(fitted_value),
              residuals = as.vector(residuals),
              sigma = sigma,
              R2 = R2)

  class(obj) <- "mmmodelr"
  return(obj)
}

coef.mmmodelr <- function(x){
  return(x$coefficients)
}

MAPE = function(a, f) {
  mape = mean(abs(a-f)/a)
  return(mape)
}

MAPE_DF = function(df, a, f) {
  mape = mean(abs(df[[a]] - df[[f]])/df[[a]])
  return(mape)
}
