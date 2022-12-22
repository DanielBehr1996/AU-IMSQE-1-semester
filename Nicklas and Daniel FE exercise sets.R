# Nicklas and Daniel FE exercise sets 
## By Nicklas and Daniel
## 22.12.2022

# Exercise set 1

# 1) Consider the file DJITicker.xlsx which contains the Yahoo tickers associated to the Dow Jones Industrial Average (DJIA) index. Save it in csv and load it in R.
library(readxl)
# install.packages("quantmod")
library(quantmod)
DJITICKER <- read_excel("/Users/danielbehr/Desktop/AU/1. semester/Financial econometrics/Opgaver/Exercise sets/1/DJITicker.xlsx")
DJITICKER <- DJITICKER[-c(10,29),] # removes not working symbols
# Here we store the file as csv file. 
write.csv(DJITICKER, "mydata.csv")
# Loading csv file
DJITICKER <- read.csv("/Users/danielbehr/Desktop/AU/1. semester/Financial econometrics/Opgaver/Exercise sets/1/mydata.csv")

# Here we make list to store data
exercise_set_1_data <- list()

# Here we make for loop to import all data using DJITICKER's
for (sTicker in DJITICKER[,"Symbol"]) {
  
  exercise_set_1_data[[sTicker]] <- getSymbols(Symbols = sTicker,
                                               from = "2018-01-01",
                                               to = "2020-01-01",
                                               auto.assign = FALSE)
}

# 2) Your task is to download the time series of adjusted closing prices for all the assets in the
#DJIA index for the last year using the getSymbols function of the quantmod package of R.
#Compute the percentage logarithmic returns (i.e. yt = (log (pricet) − log (pricet−1)) × 100)
# for each asset and collect the resulting series in a list called lRet.

# Here we make list to store log returns
lRet <- list()

for (sTicker in DJITICKER[, "Symbol"]) {
  # We want to go to the list called exercise_set_1_data and find the sTickers and then 6 column for adj. prices
  lRet[[sTicker]] <- diff(log(na.omit(exercise_set_1_data[[sTicker]][, 6])))[-1] * 100
}


# 3) Create a 30×7 matrix called DescStat. The rows name are the tickers and the columns name
# are: “mean”, “median”, “variance”, “kurtosis”, “skewness”, “rho”, “rho2”. Fill each element
# of the matrix with the associated statistic where “rho”and “rho2” correspond to cor(yt, yt−1)
# and cor(y2t , y2t−1), respectively.

DescStat <- matrix(NA, 
                   nrow = nrow(DJITICKER), 
                   ncol = 7,
                   dimnames = list(DJITICKER[, "Symbol"], 
                                   c("mean", "median", "variance", "kurtosis", "skewness", "rho", "rho2")))

kurtosis <- function(vY) {
  mean((vY - mean(vY))^4)/(mean((vY - mean(vY))^2)^2)
}

skewness <- function(vY) {
  mean((vY - mean(vY))^3)/(mean((vY - mean(vY))^2)^(3/2))
}

for (sTicker in DJITICKER[, "Symbol"]) {
  DescStat[sTicker, "mean"] <- mean(lRet[[sTicker]])
  DescStat[sTicker, "median"] <- median(lRet[[sTicker]])
  DescStat[sTicker, "variance"] = var(lRet[[sTicker]])
  DescStat[sTicker, "kurtosis"] = kurtosis(lRet[[sTicker]])
  DescStat[sTicker, "skewness"] = skewness(lRet[[sTicker]])
  DescStat[sTicker, "rho"] = cor(lRet[[sTicker]],lRet[[sTicker]])
  # rho is lag of past values, so therefor use acf function (no plot)
  DescStat[sTicker, "rho"] = acf(lRet[[sTicker]], lag.max = 1, plot = FALSE)$acf[2, 1, 1]
  # rho^2 it's important to note that ^2 is for the variable
  DescStat[sTicker, "rho2"] = acf(lRet[[sTicker]]^2, lag.max = 1, plot = FALSE)$acf[2, 1, 1]
}

# 4) The Capital Asset Pricing Model (CAPM) has been introduced by Jack Treynor, William F.
# Sharpe, John Lintner and Jan Mossin independently and is used to asses the required rate of
# return for a financial investment given its risk level. Assuming that the risk free rate is zero,
# it simply takes the form of a linear regression as:

# formula

# Compute xt as the percentage logarithmic return of S&P 500. Download the series from Yahoo finance (the ticker is ˆGSPC, note the ˆ symbol).
mRet = do.call(cbind, lRet)
GSPC <- getSymbols("^GSPC", from = "2018-01-01", to = "2020-01-01", auto.assign = F)

vX <- as.numeric(diff(log(GSPC[,6])*100)[-1])

OLS_Estimator <- function(vY, vX) {
  
  dBeta   = cov(vY, vX)/var(vX)
  dAlpha  = mean(vY) - dBeta * mean(vX) 
  
  dSigma2 = var(vY - dBeta * vX - dAlpha)
  
  vOut = c("beta" = dBeta,
           "alpha" = dAlpha,
           "sigma2" = dSigma2)
  
  return(vOut)
}




mEst <- matrix(NA, 
                     nrow = nrow(DJITICKER),
                     ncol = 3,
                     dimnames = list(DJITICKER[,"Symbol"], 
                                     # Remeber the order from the function
                                     c("Beta", "Alpha", "dSigma")))


for (sTicker in DJITICKER[, "Symbol"]) {
  
  ## insert the output of OLS_Estimator to the row sTicker of mEst
  mEst[sTicker, ] = OLS_Estimator(as.numeric(lRet[[sTicker]]), vX)
  
}

# (5): Simulation






# Exercise set 2

# Here we build a SV estimator. This function can simulate a SV model given the right inputs
# iT is the number of observations in the simulated dataset.
# dOmega is a parameter in the SV model representing the mean of the latent log variance process.
# dPhi is a parameter in the SV model representing the autoregressive coefficient of the latent log variance process.
# dSigma2_eta is a parameter in the SV model representing the variance of the error term in the latent log variance process.

SV_estimator <- function(iT, dOmega, dPhi, dSigma2_eta) {
  
  # The vU variable is being assigned a vector of iT random normal values with mean 0 and standard deviation 1. The rnorm() function generates random normal values with the specified mean and standard deviation. The iT parameter specifies the number of values to generate.
  vU <- rnorm(iT)
  # vW <- numeric(iT) initializes a numeric vector vW with iT elements and assigns it the value NA. The vector is initialized to be of length iT, but all of its elements are set to NA, which is a placeholder value that represents "Not Available" or "Not Applicable". This line of code is setting up an empty vector that will be used later in the function.
  vW <- numeric(iT)
  # h_t not vW
  # Slide 18 lecture 6:
  # The mean of the distribution of vW[1] is set to dOmega/(1-dPhi) because this is the mean of the stationary distribution of the vW process. In the stochastic volatility model, vW is modeled as an autoregressive process of order 1 (AR(1)) with mean dOmega/(1-dPhi) and variance dSigma2_eta/(1-dPhi^2). The stationary distribution of an AR(1) process is a Normal distribution with mean dOmega/(1-dPhi) and variance dSigma2_eta/(1-dPhi^2). Therefore, the mean of the stationary distribution of vW is dOmega/(1-dPhi).
  # Look in slides for rnorm(1, alpha, beta) slide 19
  vW[1] <-rnorm(1, dOmega/(1-dPhi), sqrt(dSigma2_eta)/(1-dPhi^2)) 
  
  #  The first value of vW is initialized using the mean and standard deviation values that you specified. The subsequent values of vW are generated by the loop, which uses the previous value of vW and the specified values of dOmega, dPhi, and dSigma2_eta to generate the next value of vW. The mean and standard deviation values are only used for the first value of vW, since the subsequent values are generated using the model equations.
  for (t in 2:iT) {
    vW[t] <- dOmega + dPhi * vW[t-1] + sqrt(dSigma2_eta) * rnorm(1)
    
  }
  # sqrt(dSigma2_eta) * rnorm(1) because it's a Normal distribution rnorm() and it's dSigma^{2}_{eta} and therefore sqrt()
  
  # vSigma is exp(h_t/2) slide 18
  # Notes: In the stochastic volatility model, the variable vSigma represents the volatility of the time series data. In the model, the volatility is modeled as a function of the latent process vW. Specifically, the volatility is given by the expression exp(vW/2). This means that the value of vSigma at each time point is equal to the exponential of half of the value of vW at that time point. The reason for taking the exponential of half of vW is to ensure that vSigma is always positive, since the exponential function is always positive.
  vSigma <- exp(vW/2)
  # vY is y_t = exp(h_t/2) * u_t = vSigma * u_t
  vY <- vSigma * vU
  
  
  
  lOut <- list()
  lOut[["vY"]] <- vY 
  lOut[["vSigma"]] <- vSigma
  lOut[["dPhi"]] <- dPhi
  
  return(lOut)
}

# Here we give the initial inputs needed to simulate the SV model
dOmega = 0.0; dPhi = 0.9; dSigma2_eta = 0.25; iT = 1000; set.seed(2022)

# Here we simulate and store results
results <- SV_estimator(iT, dOmega, dPhi, dSigma2_eta)

# Here we plot the simulated returns
plot(results$vY, type = "l")
# Plot of volatility
plot(results$vSigma, type = "l")

# Here we get the GMM_estimator provided by Leopoldo. It uses the 24 moments 
source("/Users/danielbehr/Desktop/AU/1. semester/Financial econometrics/Opgaver/Solution from Teacher/2/GMMEstimation_SV.R")

# Here we estimate the simulated vY from a stochastic volatility model
fit <- GMM_Estimator(results$vY)


# Here we print the estimated parameters
fit$par


#Download the time series of the S&P500 index from Yahoo finance from 2005-01-01 to 2018-
#  01-01 and compute the percentage log returns. Replace the zero returns with their empirical
# mean. Estimate the SV model by GMM.
library(quantmod)
data <- getSymbols(Symbols = "^GSPC",
                   from = "2005-01-01",
                   to = "2018-01-01",
                   auto.assign = F)

# Here we create the logarithmic returns
lRet <- as.numeric(diff(log(data[,6]))[-1] * 100)


plot(lRet)
# Here we makes sure that there is no logarithmic returns that are 0, and if there are replaces them with the mean of lRet.
lRet[lRet == 0] <- mean(lRet)

# Here we plot the lRet to analyze it visualize it
plot(lRet)

# Here we estimate by GMM
fit_GMM2022 <- GMM_Estimator(lRet)

# Here we print the parameters
fit_GMM2022$par



# Here we do it for AAPL
AAPL <- getSymbols(Symbols = "AAPL",
                   from = "2019-01-01",
                   to = "2021-01-01",
                   auto.assign = F)
AAPL <- as.numeric(diff(log(AAPL[,6])*100)[-1])
AAPL[AAPL == 0] <- mean(AAPL)
plot(AAPL)
test <- GMM_Estimator(AAPL)
test$par

















