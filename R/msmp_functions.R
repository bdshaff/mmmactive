#' funcs
#'
#' @export

AdResponse <- function(afGRPsMat, afCoeffsMat, params) {
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
  fDecay <- params[4] / 100

  # Define output matrix size
  afGRPsMat <- as.matrix(afGRPsMat)
  afAdResponse <- matrix(1:nrow(afGRPsMat))
  fEffGRPs <- RollingSum(afGRPsMat, fDecay, nPeriod)
  fTotalEffGRPs <- AdStock(afGRPsMat, fDecay)

  a <- afCoeffsMat[[1, nEffFreq]]
  b <- afCoeffsMat[[2, nEffFreq]]
  c <- afCoeffsMat[[3, nEffFreq]]

  if (nRecFreq == 0) {
    afAdResponse[, 1] <- Reach(a, b, c, fTotalEffGRPs)
    return(afAdResponse)
  }

  if (nRecFreq == nEffFreq) {
    afAdResponse[, 1] <- Reach(a, b, c, fEffGRPs)
    return(afAdResponse)
  }

  fTotalEffGRPs_rounded <- round(fTotalEffGRPs[, 1], digits = 6)
  fEffGRPs_rounded <- round(fEffGRPs[, 1], digits = 6)
  a_s <- afCoeffsMat[1, ]
  b_s <- afCoeffsMat[2, ]
  c_s <- afCoeffsMat[3, ]
  f <- matrix(0, nrow = nrow(afGRPsMat), ncol = 1)
  for (k in nRecFreq:(nEffFreq - 1)) {
    one <- Reach(a_s[[k]], b_s[[k]], c_s[[k]], fEffGRPs)
    two <- Reach(a_s[[k + 1]], b_s[[k + 1]], c_s[[k + 1]], fEffGRPs)
    three <- Reach(a_s[[nEffFreq - k]], b_s[[nEffFreq - k]], c_s[[nEffFreq - k]], fTotalEffGRPs - fEffGRPs)
    f <- f + (one - two) * three / 100
  }


  for (i in 1:nrow(afGRPsMat)) {
    # Calculate the x+y(<x) Model
    if (fTotalEffGRPs_rounded[i] == fEffGRPs_rounded[i]) {
      # There is no extra history outside of the recency window
      afAdResponse[i, 1] <- Reach(a, b, c, fEffGRPs[i, 1])
    } else {
      # There is extra history outside of the recency window to be considered
      afAdResponse[i, 1] <- f[i, 1] + Reach(a, b, c, fEffGRPs[i, 1])
    }
  }

  # Return the calculated AdResponse
  return(as.vector(afAdResponse))
}

RollingSum <- function(afGRPsMat, fDecay, nPeriod) {
  # Create a rolling sum of an AdStock to a vector of data

  # afGRPsMat = matrix (vertical vector) of GRP data fDecay = single data point, decimal decay rate of media nPeriod
  # = integer value of number of observations to sum over

  weights <- vector(length = nPeriod)
  decay <- 1 - fDecay
  for (i in 1:nPeriod) {
    weights[i] <- decay^(nPeriod - i)
  }

  afRollingSum <- roll_sumr(afGRPsMat, weights = weights, normalize = F)
  afRollingSum[1:nPeriod - 1] <- afGRPsMat[1:nPeriod - 1]

  # Return the rolling sum of data
  return(afRollingSum)

  # Example use of Function test=RollingSum(afGRPs, 0.15, 4)
}

AdStock <- function(afGRPsMat, fdecayRate) {
  # Generate matrix of AdStocked/Decayed GRPs as a function of input GRPs and decay rate to a value
  # y(t)=y(t-1)*d + x(t)

  # afGRPs = matrix (vertical vector) of GRP Data
  # fdecayRate = decimal version of decay rate

  # Create output matrix base on size of input
  afAdStockedGRPsMat <- matrix(1:nrow(afGRPsMat))

  # first observations are equal
  afAdStockedGRPsMat[1, 1] <- afGRPsMat[1, 1]

  # loop through calculating AdStocked GRPs
  decay <- 1 - fdecayRate
  value <- afGRPsMat[1, 1]
  for (x in 2:nrow(afGRPsMat)) {
    value <- value * decay + afGRPsMat[x, 1]
    afAdStockedGRPsMat[x, 1] <- value
  }

  # Return AdStocked GRPs matrix
  return(afAdStockedGRPsMat)

  # Example use of AdStock Function test=AdStock(data.matric(GRPs[3]),0.15)
}

Reach <- function(fa, fb, fc, fGRPs) {
  # Return reach data subject to fitted formula to a value r=a/(1+b*(GRPs/1000)^c)

  # fa = Alpha Coefficient in reach model
  # fb = Beta Coefficient in reach model
  # fc = Gamma Coefficient in reach model
  # fGRPs = single data point of GRPs at which to calculate reach

  fReach <- as.numeric(fGRPs > 0) * fa / (1 + fb * (fGRPs / 1000)^fc)
  # Return calculated reach value
  return(fReach)

  # Example Use of Reach Function (solution=1.222065) test=Reach(0.79,-1,0.5,125)
}

AdStockPD <- function(data, i, p) {
  rowSums(as.data.frame(embed(c(rep(NA, p), data), p + 1) %*% ((1 - i)^seq(0, p, 1))), na.rm = F) -> output
  output[is.na(output)] <- 0
  return(output)
}


coef.mmmodelr <- function(x) {
  return(x$coefficients)
}

MAPE <- function(a, f) {
  mape <- mean(abs(a - f) / a)
  return(mape)
}

MAPE_DF <- function(df, a, f) {
  mape <- mean(abs(df[[a]] - df[[f]]) / df[[a]])
  return(mape)
}

adstockv3 <- function(afGRPs, fdecayRate, peak = 1, length = 600) {

  # sanity check
  if (fdecayRate < 0 | fdecayRate > 1) {
    cat("the specified decay rate is", fdecayRate, "\n")
    stop("please specify decay a number between [0,1]")
  }
  if (peak < 1 | peak >= length(afGRPs)) {
    cat("the specified length is", length, "\n")
    stop("please specify peak a number >= to 1. 1 meaning the peak strength is at the current week. ")
  }
  if (length <= peak) {
    stop("please specify length to be greater than peak")
  }

  # do this element by element
  afAdStockedGRPs <- 0
  for (i in 1:length(afGRPs)) {
    value <- afGRPs[i] # record the original value
    if (value > 0) {
      tmp1 <- afGRPs
      tmp2 <- afGRPs
      tmp2[i] <- 0
      tmp <- tmp1 - tmp2

      tmp3 <- lag(tmp, (peak - 1), default = 0)

      res <- Reduce(function(v, x) v * (1 - fdecayRate) + x, x = tmp3, accumulate = TRUE)

      if (peak > 1) {
        k <- i - 1
        for (j in 1:(peak - 1)) {
          if ((k + j) <= length(afGRPs)) { # making sure it won't pass the max length of the variable
            res[k + j] <- j / peak * value
          }
        }
      }
      if (length(res) >= (i + length)) {
        res[(i + length):length(res)] <- 0 # zero out all the carry-over afer the i+length
      }
      if (sum(res) != 0) {
        res <- res * sum(tmp) / sum(res)
      }
      afAdStockedGRPs <- afAdStockedGRPs + res
    }
  }
  afAdStockedGRPs2 <- Reduce(function(v, x) v * (1 - fdecayRate) + x, x = afGRPs, accumulate = TRUE)
  # normalize it
  if (sum(afAdStockedGRPs2) != 0) {
    afAdStockedGRPs2 <- afAdStockedGRPs2 * sum(afGRPs) / sum(afAdStockedGRPs2) # scale it so the sum of the total stays the same
  }
  return(afAdStockedGRPs)
}
