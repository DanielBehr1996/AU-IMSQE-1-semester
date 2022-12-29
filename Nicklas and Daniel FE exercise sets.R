# Nicklas and Daniel FE exercise sets 
## 29 dec 2022 (last updated)

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



# Exercise set 3 



#1) Write a code to perform filtering in a Gaussian Linear State Space model using the recursions in slide 22 of Lecture 7.


# Kalman filter and smoother for the state space:
# Y_t         = Z * alpha_t + eps_t, eps_t ~ N(0, S)
# alpha_{t+1} = T * alpha_t + H*eta_t, eta_t ~N(0, Q)
#
# Y_t is p x 1
# Z   is p x m
# S   is p x p
# T   is m x m
# H   is m x l
# Q   is l x l
# a1 is the initialization for a
# P1 is the initialization for P
#

# mY: This could be a matrix of the historical observations of a financial time series, such as daily stock prices or exchange rates. Each column of the matrix represents a time point, and each row represents a different financial asset.
#mZ: The design matrix mZ relates the observations to the state of the system. In the context of financial time series, the state could represent underlying factors that influence the time series, such as economic growth or market sentiment. mZ could include variables such as lagged values of the time series, macroeconomic indicators, or other factors that are believed to influence the time series.
# mS: The observation covariance matrix mS represents the uncertainty or noise in the observations. In financial time series, this could reflect measurement error, market noise, or other sources of uncertainty.
# mT: The state transition matrix mT describes how the state of the system evolves over time. In financial time series, this could represent how underlying factors change over time.
# mH: The matrix mH projects the state onto the observations. In financial time series, this could represent the relationship between the underlying factors and the time series.
# mQ: The state covariance matrix mQ represents the uncertainty or noise in the state. In financial time series, this could reflect the error in the estimate of the underlying factors, or the uncertainty in their evolution over time.
# a1: The initial state mean a1 is the mean of the distribution of the initial state of the system. In financial time series, this could represent the starting values of the underlying factors.
# P1: The initial state covariance matrix P1 represents the uncertainty in the initial state of the system.
# Smoothing: The logical flag Smoothing indicates whether to perform the Kalman smoother on the input time series. If TRUE, the function will return the smoothed states in addition to the filtered states. If FALSE, only the filtered states will be returned.
KF_R <- function(mY, mZ, mS, mT, mH, mQ, a1, P1, Smoothing = TRUE) {
  # n: The number of columns in mY, which represents the number of time points in the time series.
  # p: The number of rows in mY, which represents the number of financial assets in the time series.
  # r_int: The number of columns in mH, which represents the number of underlying factors that influence the time series.
  # m: The length of the vector a1, which represents the number of elements in the initial state mean.
  # v: A matrix of the prediction errors at each time point, with dimensions p x n.
  # F: An array of the variances of the prediction errors at each time point, with dimensions p x p x n.
  # K: An array of the Kalman gains at each time point, with dimensions m x p x n.
  # a_filt: A matrix of the filtered state means at each time point, with dimensions m x n.
  # a_pred: A matrix of the predicted state means at each time point, with dimensions m x n.
  # P_filt: An array of the filtered state variances at each time point, with dimensions m x m x n.
  # P_pred: An array of the predicted state variances at each time point, with dimensions m x m x n.
  # r: A matrix of the smoothed state residuals at each time point, with dimensions m x n.
  # N: An array of the smoothed state covariances at each time point, with dimensions m x m x n.
  # a_smoot: A matrix of the smoothed state means at each time point, with dimensions m x n.
  # V: An array of the smoothed state variances at each time point, with dimensions m x m x n.
  # L: An array of the smoothing gains at each time point, with dimensions m x m x n.
  # eps_smoot: A matrix of the smoothed prediction errors at each time point, with dimensions p x n.
  # eta_smoot: A matrix of the smoothed residuals at each time point, with dimensions r_int x n.
  # vLLK: A vector of the likelihood contributions at each time point.
  n = ncol(mY)
  p = nrow(mY)
  r_int = ncol(mH)
  m = length(a1)
  
  v = matrix(0, p,n);
  F = array(0, dim = c(p, p, n));
  K = array(0, dim = c(m, p, n));
  a_filt = matrix(0, m,n);
  a_pred = matrix(0, m,n);
  P_filt = array(0, dim = c(m, m, n));
  P_pred = array(0, dim = c(m,m,n));
  
  r = matrix(0, m,n);
  N = array(0, dim = c(m,m,n));
  a_smoot = matrix(0, m, n);
  V = array(0, dim = c(m, m, n));
  L = array(0, dim = c(m,m,n));
  
  eps_smoot = matrix(0, p,n);
  eta_smoot = matrix(0, r_int, n);
  vLLK = numeric(n);
  
  # //initialise
  # The variables v, a_filt, a_pred, P_filt, and P_pred are used to store the intermediate results of the Kalman filter. They are initialized at the first time point because the Kalman filter works iteratively, starting at the first time point and working forward in time. The initial values of these variables are given by the input parameters mY, a1, and P1.
  v[, 1] = mY[, 1];
  a_filt[, 1] = a1;
  a_pred[, 1] = a1;
  P_filt[,,1] = P1;
  P_pred[,,1] = P1;
  
  # This part can be found in equation for P_{t+1} last part of equation
  HQH = mH %*% mQ %*% t(mH);
  # The equation dC = -0.5 * (n * p * 1.0) * log(pi * 2.0) is used to calculate a constant term that is added to the log likelihood function. The log likelihood function is used to evaluate the fit of the Kalman filter to the data.
  dC = -0.5 * (n * p * 1.0) * log(pi * 2.0);
  
  # //filtering recursion
  # The reason mT does not occur in all equations is because it is only used in the equations that calculate the predicted state mean and predicted state variance, which are intermediate quantities that are used in the Kalman filter recursion. The filtered state mean and filtered state variance, which are the final estimates of the state at each time point, are calculated using the prediction errors and the Kalman gains, which do not depend on the state transition matrix.
  #In summary, mT is only used in certain equations because it is only relevant to the calculation of the predicted state mean and predicted state variance, which are intermediate quantities used in the Kalman filter recursion. The filtered state mean and filtered state variance, which are the final estimates of the state at each time point, are calculated using the prediction errors and the Kalman gains, which do not depend on the state transition matrix.
  for(t in 1:n) {
    #prediction error
    v[, t] = mY[, t] - mZ %*% a_pred[, t];
    # variance of the prediction error
    # This equation is F_t= ZP_tZ´ + sigma_t^2DD´ 
    # This would sigma_t^2DD´= mS
    # mS is a input variable, and hence is given
    F[,,t] = mZ %*% P_pred[,,t] %*% t(mZ) + mS;
    # filtered state mean E[alpha_t |Y_1:t]
    # This equation is a_{t+1} = Ta_t + k_t v_t where (k_t = TP_tZ´F^{-1}_t v_t)
    a_filt[, t] = a_pred[,t] + P_pred[,,t] %*% t(mZ) %*% solve(F[,,t]) %*% v[,t];
    # filtered state variance Var[alpha_t |Y_{1:t}]
    # This equation comes from P_{t+1} = TP_t L´_t + HQH
    # Not sure where the last P_pred[,,t] comes from
    P_filt[,,t] = P_pred[,,t] - P_pred[,,t] %*% t(mZ) %*% solve(F[,,t]) %*% mZ %*% P_pred[,,t];
    # kalman gain 
    # This is the equation for K_t = T P_t Z´ F_t^{-1}
    K[,,t]      = mT %*% P_pred[,,t] %*% t(mZ) %*% solve(F[,,t]);
    # likelihood contribution
    # In the Kalman filter algorithm, the vLLK variable represents the likelihood contribution at each time point. It is calculated as the sum of the log determinant of the variance of the prediction error (F[,,t]) and the dot product of the prediction error (v[,t]) and its inverse.
    # This comes from slide 23 from lecture 7
    vLLK[t] = (log(det(as.matrix(F[,,t]))) + c(v[,t] %*% solve(F[,,t]) * v[,t]));
    # This block of code is used to calculate the predicted state mean and variance for the next time point (t+1). It is only used when the current time point (t) is less than the total number of time points (n). This is because these quantities are only defined for time points t+1, t+2, ..., n-1. They are not defined for the last time point (n) because there is no time point after that.
    # The block of code within the if statement will only be executed if t is less than n. This means that it will be executed at all time points except the last one.
    if(t < n){
      # predicted state mean E[alpha_{t+1}|Y_{1:t}]
      a_pred[,t + 1] = mT %*% a_pred[,t] + K[,,t] %*% v[, t];
      # predicted state variance Var[alpha_{t+1}|Y_{1:t}]
      P_pred[,,t + 1] = mT %*% P_pred[,,t] %*% t((mT - K[,,t] %*% mZ)) + HQH;
    }
  }
  # //Smoothing recursion
  if(Smoothing){
    # In this context, for(t in n:2) means to loop through the sequence of integers from n to 2, in reverse order. This is equivalent to writing for(t in 2:n), but the loop is executed in the opposite order.
    # For example, if n is equal to 5, then the loop will iterate over the values 5, 4, 3, and 2. The statements inside the loop will be executed in that order.
    for(t in n:2) {
      # This is the equation for L_t = T - K_t Z
      L[,,t]= mT - K[,,t] %*% mZ;
      # 
      r[, t - 1] = t(mZ) %*% solve(F[,,t]) %*% v[, t] + t(L[,,t]) %*% r[, t];
      # 
      N[,,t - 1] = t(mZ) %*% solve(F[,,t]) %*% mZ + t(L[,,t]) %*% N[,,t] %*% L[,,t];
      # smoothed state mean E[alpha_t | Y_{1:n}]
      # Slide 27 lecture 7:
      a_smoot[, t] = a_pred[, t] + P_pred[,,t] %*% r[, t - 1];
      # smoothed state variance Var[alpha_t | Y_{1:n}]
      # Slide 28 lecture 7:
      V[,,t] = P_pred[,,t] - P_pred[,,t] %*% N[,,t - 1] %*% P_pred[,,t];
      # //error smoothing
      # 
      eps_smoot[, t] = mS %*% (solve(F[,,t]) %*% v[, t] - t(K[,,t]) %*% r[, t]);
      # 
      eta_smoot[, t] = mQ %*% t(mH) %*% r[, t];
    }
  }
  
  KF = list();
  
  KF[["v"]] = v;
  KF[["a_filt"]] = a_filt;
  KF[["a_pred"]] = a_pred;
  KF[["P_filt"]] = P_filt;
  KF[["P_pred"]] = P_pred;
  KF[["F"]] = F;
  KF[["K"]] = K;
  KF[["N"]] = N;
  KF[["a_smoot"]] = a_smoot;
  KF[["V"]] = V;
  KF[["L"]] = L;
  KF[["eps_smoot"]] = eps_smoot;
  KF[["eta_smoot"]] = eta_smoot;
  KF[["vLLK"]] = vLLK;
  KF[["dLLK"]] = -dC - 0.5 * sum(vLLK);
  
  return(KF);
  
}


KalmanFilter_AR1plusNoise <- function(vY, vPar, Smoothing = FALSE) {
  
  dPhi = vPar[1]
  dSigma = vPar[2]
  dEta = vPar[3]
  
  mY = matrix(vY, nrow = 1)
  mZ = matrix(1, 1, 1)
  mS = matrix(dSigma^2, 1, 1)
  mT = matrix(dPhi, 1, 1)
  mH = matrix(1, 1, 1)
  mQ = matrix(dEta^2, 1, 1)
  a1 = 0
  mP1 = matrix(dEta^2/(1-dPhi^2), 1, 1)
  
  KF_R(mY, mZ, mS, mT, mH, mQ, a1, mP1, Smoothing)
  
}


# 2) Write a code to simulate from the SV model reported in slide 31 of Lecture 7.
set.seed(2022)
SV_model_simulator_lecture_7_slide_31 <- function(iT, dRho, dSigma2_eta, dSigma) {
  # This being the errorterm for r_t:
  vZ <- rnorm(iT) 
  vEta <- rnorm(iT, mean = 0, sd = sqrt(dSigma2_eta))
  vW <- numeric(iT)
  
  vW[1] <- rnorm(1, 0, sqrt(dSigma2_eta/(1-dRho^2)))
  
  for (t in 2:iT) {
    vW[t] <- dRho * vW[t-1] + vEta[t]
  }
  
  # Here we calculate the equation for r_t = dSigma * exp(w_t/2)z_t
  vR <- dSigma * exp(vW/2)*vZ
  
  # Note that this part can create problems..
  lOut = list(vR = vR,
              vW = vW,
              dSigma = dSigma,
              dSigma2_eta = dSigma2_eta) # we want to compare dSigma across different estimation functions later
  
  return(lOut)
}

# 3) Simulate T = 1000 observations from the SV model with  = 1,  = 0.9, and 2 = 0.25.
# Set the seed to 123.

iT = 1000; dRho = 0.9; dSigma2_eta = 0.25; dSigma = 1


simulated_data <- SV_model_simulator_lecture_7_slide_31(iT, dRho, dSigma2_eta, dSigma)
# vR is returns

# vW is logartimic volatility.

# Further understanding:
# In the function SV_model_simulator_lecture_7_slide_31, vW is a vector of simulated values for the latent variable w. This variable is used to model the volatility of the returns, vR. The values of vW are generated by the recursion vW[t] = dRho * vW[t-1] + vEta[t] where vEta is a vector of normally distributed random errors with mean 0 and variance dSigma2_eta. The values of vR are then calculated as vR = dSigma * exp(vW/2)*vZ, where vZ is a vector of normally distributed random errors with mean 0 and variance 1.

# When you plot vW, you are plotting a time series of the values of vW at each time point. vW is a sequence of random variables representing the latent log volatility process in the univariate stochastic volatility model. It is usually not directly observable and must be inferred from the observed data (in this case, the returns vR). By plotting vW, you can get a sense of the underlying volatility process that generated the returns.


# 4) Write a function that maximize the quasi likelihood computed via the Kalman filter for
## the SV model of the previous point. The likelihood to maximize is defined in slide 23
## of Lecture 7.

# The "quasi likelihood" refers to the log-likelihood of the data

QML_SV <- function(vY) {
  
  ## Be sure that there are no zeros
  # This is being done for the returns
  if(any(vY==0)) {
    vY[vY==0] = mean(vY)
  }
  
  # f any of the returns are equal to zero, this will cause problems when taking the log of the returns. Specifically, the logarithm of zero is not defined, so the log of zero squared (which is what is done in the next line of code) will produce an error.
  # 1) transform the data
  vXi = log(vY^2)
  
  # Estimate sigma
  # where E[log(z_t^2)] = -1.270376
  # The value 1.270376 is the variance of a standard normal distribution, which is equal to $(\sqrt{2\pi})^2/2 \approx 1.270376$. The factor of 2 comes from the fact that vXi is the log of the squared returns, and so the variance of vXi is equal to twice the variance of the log returns.
  dSigma = exp((mean(vXi) + 1.270376)/2)
  
  # 2) Demean vXi
  # Demeaning the data is a common preprocessing step in statistical analysis, and is typically done to remove the effect of the mean from the data. This can make it easier to interpret the results of the analysis, and can help to stabilize the variance of the data.
  # It's worth noting that this particular transformation, vXi = log(vY^2), results in data that is centered around 0 by design. This means that the mean of vXi is already 0, and therefore demeaning vXi would not have any effect in this case. However, it is still included in the function for completeness.
  vXi = vXi - mean(vXi)
  
  iT = length(vXi)
  
  #just two parameters rho and sigma2_tau, we constrain sigma_eps^2 = pi^2/2
  # starting values
  # The first element of vPar is an initial estimate of the parameter dRho, which is the correlation between vXi at time t and vXi at time t-1. 
  # The second element of vPar is an initial estimate of the parameter dSigma2_eta, which is the variance of the process vXi.
  vPar = c(cor(vXi[-1], vXi[-iT]), var(vXi) * 0.1)
  
  # maximize the likelihood
  # The optim function is used to find the maximum likelihood estimates of the parameters dRho and dSigma2_eta. The function that is being optimized is defined in the first argument of the optim function. This function takes in two arguments, vPar and vXi, and returns the negative log likelihood (-dLLK). The optim function then tries to find the values of vPar that maximize the function being passed to it. The second argument of the optim function specifies the starting values for vPar, which are the sample correlation between vXi[-1] and vXi[-iT], and the sample variance of vXi times 0.1. The method argument specifies the optimization algorithm to be used, and the lower and upper arguments specify the lower and upper bounds for the optimization. The remaining arguments, vXi and iT, are passed to the function being optimized as additional arguments.
  optimizer = optim(vPar, function(vPar, vXi, iT) {
    
    dRho = vPar[1]
    dSigma2_eta = vPar[2]
    # The first element of vPar_tot, dRho, represents the value of the parameter rho that you are trying to estimate. The second element, the square root of pi squared divided by 2, represents the value of the parameter sigma_eps that you are trying to estimate. The third element, the square root of dSigma2_eta, represents the value of the parameter sigma2_eta that you are trying to estimate.
    # dSigma: sqrt(pi^2/2)
    vPar_tot = c(dRho, sqrt(pi^2/2), sqrt(dSigma2_eta))
    
    KF = KalmanFilter_AR1plusNoise(vXi, vPar_tot, Smoothing = FALSE)
    
    dLLK = KF$dLLK
    # In the case of maximum likelihood estimation, we can instead minimize the negative log likelihood, which is defined as the negative of the log of the likelihood function. The log function is monotonically increasing, so minimizing the negative log likelihood is equivalent to maximizing the likelihood. This is why the function in this code returns -dLLK, which is the negative of the log likelihood (dLLK)
    return(-dLLK)
    
  }, method = "L-BFGS-B", lower = c(0.001, 0.001), upper = c(0.99, 1.0),
  vXi = vXi, iT = iT)
  
  #extract estimated parameters
  vPar = optimizer$par
  dRho = vPar[1]
  dSigma2_eta = vPar[2]
  
  #compute the vector of total parameters
  vPar_tot = c(dRho, sqrt(pi^2/2), sqrt(dSigma2_eta))
  
  # filtering and smoothing
  # Overall, this line of code applies the Kalman filter to the data vY to obtain estimates of the latent variables in the model, as well as their associated variances and covariances. The smoothed estimates of the latent variables are also computed.
  Filter = KalmanFilter_AR1plusNoise(vY, vPar_tot, Smoothing = TRUE)
  
  # Making an object to store the results
  vModelPar = c(sigma = dSigma,
                rho = dRho,
                sigma2eta = dSigma2_eta)
  
  # Making a list to store the results
  # "Filter" is an object that stores the results of running the KalmanFilter_AR1plusNoise function on the input data "vY" with the parameters "vPar_tot" and with the smoothing option set to "TRUE".
  #"vModelPar" is a vector that stores the values of three parameters: "dSigma", "dRho", and "dSigma2_eta". These are the parameters of the stochastic volatility model that were estimated using the quasi maximum likelihood method.
  
  lOut = list(vPar = vModelPar,
              Filter = Filter)
  
  return(lOut)
  
}


# 5) Estimate the model on the simulated data. Can you recover the true parameters?

Estimated_vR <- QML_SV(simulated_data$vR)

Estimated_vR$vPar

# Check with the values given in the exercise set. They are fairly close.



# Real data
# Download the time series of the S&P500 index from Yahoo finance from 2005-01-01 to 2018-
# 01-01 and compute the percentage log returns. Replace the zero returns with their empirical
#mean. Estimate the SV model by QML. Use the mapping between the SV specification you
#estimated and the one of Lecture 6 in order to compare your estimates with those obtained
#from the GMM estimator in Exercise Set 2.

library(quantmod)
# Getting data from yahoo
sp500 <- getSymbols(Symbols = "^GSPC",
                    from = "2005-01-01",
                    to = "2018-01-01",
                    auto.assign = F)
# calculating the log return for the adjusted prices
# REMEMBER TO USE as.numeric (can cause problems with functions)
lRet <- as.numeric(diff(log(sp500[,6])*100))[-1]
# We don't want any 0's, so we replace them by the mean
lRet[lRet == 0] <- mean(lRet)
# estimating by QML
Estimated_real_data_QML <- QML_SV(lRet)
Estimated_real_data_QML$vPar
# Now we want to use this GMM_estimator
source("/Users/danielbehr/Desktop/AU/1. semester/Financial econometrics/Opgaver/Solution from Teacher/2/GMMEstimation_SV.R")
Estimated_real_data_GMM <- GMM_Estimator(lRet)
# t is possible that the difference in the estimated parameters between the two methods is due to the fact that they use different optimization algorithms and starting values for the optimization. The GMM method uses the Nelder-Mead optimization algorithm and estimates the parameters by minimizing the sum of squared residuals between the observed data and the model predictions. On the other hand, the QML method uses the L-BFGS-B optimization algorithm and maximizes the quasi-likelihood computed via the Kalman filter.

# map between the GMM and QML estimate
# We now use the equations we derived:
# sigma = exp(omega/(2*(1-rho)))
# The first value is omega second is phi of the Estimated_real_data_GMM
exp(Estimated_real_data_GMM$par[1]/(2*(1.0 - Estimated_real_data_GMM$par[2]))) # 0.6888401

# For the mapping to make sence, then it should be similar to:
# should be similar to sigma from the QML function
Estimated_real_data_QML$vPar[1] # 0.7453121


# Exercise set 4

# Computational part

# 1) Write a function to perform filtering of the volatility in the Stochastic Volatility (E[exp(alpha_t/2)|y_{1:t}])
# using the Bootstrap filter reported in slide 25 of Lecture 8.

# It's from slide 12 lecture 8:
#This is the SV model:
# y_t = exp(a_t/2) * epsilon_t
# a_t = w + phi * a_{t-1} + tau * eta_t 
# iN is the number of particles used in the Bootstrap filter. It is an optional argument that is set to 10000 by default.
BootstrapFilter <- function(vY, dOmega, dPhi, dTau, iN = 10000) {
  # iT is the lenght of returns
  iT = length(vY)
  
  ## matrix of particles
  mAlpha = matrix(NA, iT, iN)
  ## vector with filtered volatility values
  vVol = numeric(iT)
  ## importance weight at iteration t
  # This is the w's from slide 25
  vW = numeric(iN)
  
  ##initialize at time t = 1
  #sample from the unconditional dist as an initial proposal distribution
  # This is the first Alpha, but there is iN columns, therefore inset iN
  # mean and sd is as usual with SV models. 
  mAlpha[1, ] = rnorm(iN, dOmega/(1.0 - dPhi), sqrt(dTau^2/(1 - dPhi^2)))
  #compute the importance weights
  # n this case, the probability of vY[1] is being calculated given a mean of 0 and a standard deviation equal to the value of exp(mAlpha[1, ]/2.0) for each particle. 
  # we have exp(mAlpha[1, ]/2.0 from slide 19 lecture 8
  # vY[1] is used to compute the importance weight
  vW = dnorm(vY[1], 0, exp(mAlpha[1, ]/2.0))
  #normalized importance weights
  # Slide 18 lecture 8
  # sum(vW) is one number, and when we divide with vW (a vector) then we still have 10.000 numbers but they all get dividid with sum(vW)
  vW = vW/sum(vW)
  #approximate E[exp(alpha_1) | y_{1}]
  # slide 19 lecture 8 (exp hat{alpha/2})
  vVol[1] = sum(exp(mAlpha[1, ]/2) * vW)
  #resample
  # here we random sample from the object mAlpha to make the first row in mAlpha, and we use the importance weights as the probability
  # Sample: should sampling be with replacement?
  mAlpha[1, ] = sample(mAlpha[1, ], iN, replace = TRUE, prob = vW)
  
  for (t in 2:iT) {
    #sample from the proposal distribution p(alpha_t|alpha_{t-1})
    # slide 12 lecture 8
    mAlpha[t, ] = dOmega + dPhi * mAlpha[t - 1, ] + rnorm(iN, 0, dTau)
    #compute the importance weights
    vW = dnorm(vY[t], 0, exp(mAlpha[t, ]/2.0))
    #normalized importance weights
    vW = vW/sum(vW)
    #approximate E[exp(alpha_t) | y_{1:t}]
    vVol[t] = sum(exp(mAlpha[t, ]/2) * vW)
    #resample
    mAlpha[t, ] = sample(mAlpha[t, ], iN, replace = TRUE, prob = vW)
    
    ### NOTE THAT SINCE WE RESAMPLE AT EACH ITERATION WE AVOID THE STEP vW = 1
    ### CONSEQUENTLY WHEN WE COMPUTE THE IMPORTANCE WEIGHTS AT TIME t WE NEGLECT
    ### THE EFFECT OF THE IMPORTANCE WEIGHTS AT TIME t-1, SEE LECTURE 10
  }
  
  return(vVol)
  
}

# 2) Write a function like the one in point 1) but with resampling that occurs only when
# the Effective Sample Size (slide 26 of Lec. 8) is below the threshold gN. Let g be an
# argument of this function. Note that, when g = 1 resampling occurs at each iteration
# of the algorithm.

# Notes:
# The function BootstrapFilter_EES (EES stands for effective sample size) is a modified version of the BootstrapFilter function that uses a different resampling scheme. In the BootstrapFilter function, the particles are always resampled at each iteration, regardless of the quality of the sample. In contrast, the BootstrapFilter_EES function only resamples when the effective sample size (ESS) falls below a certain threshold. The ESS is a measure of the quality of the sample and is defined as the reciprocal of the sum of the squared normalized importance weights. A low ESS indicates that the sample is of poor quality, as many of the particles have similar weights, and the sample is not representative of the true distribution. In this case, resampling is necessary to correct the sample and improve the estimate of the filtered volatility
BootstrapFilter_EES <- function(vY, dOmega, dPhi, dTau, iN = 10000, dG = 0.75) {
  
  iT = length(vY)
  
  ## matrix of particles
  mAlpha = matrix(NA, iT, iN)
  ## vector with filtered volatility values
  vVol = numeric(iT)
  ## matrix importance weight at iteration t
  ## A more efficient version of the code can be written by
  ## keeping track only of two consecutive vectors of particles
  # This is NEW:
  mW_tilde = matrix(NA, iT, iN)
  ## vector of normalized importance weights
  vW = numeric(iN)
  
  ##initialize at time t = 1
  #sample from the unconditional dist as an initial proposal distribution
  # First row of Alpha is made for all iN coloumns
  mAlpha[1, ] = rnorm(iN, dOmega/(1.0 - dPhi), sqrt(dTau^2/(1 - dPhi^2)))
  #compute the importance weights
  mW_tilde[1, ] = dnorm(vY[1], 0, exp(mAlpha[1, ]/2.0))
  #normalized importance weights
  vW = mW_tilde[1, ]/sum(mW_tilde[1, ])
  #approximate E[exp(alpha_1) | y_{1}]
  # In the BootstrapFilter_EES function, vVol is a vector that stores the filtered volatility values at each time step. 
  vVol[1] = sum(exp(mAlpha[1, ]/2) * vW)
  #resample
  dESS = 1.0/sum(vW^2)
  # Now we only want to resample if the ESS falls below a certain threshold.
  if (dESS < dG * iN) {
    mAlpha[1, ] = sample(mAlpha[1, ], iN, replace = TRUE, prob = vW)
    mW_tilde[1, ] = 1.0
  }
  for (t in 2:iT) {
    #sample from the proposal distribution p(alpha_t|alpha_{t-1})
    mAlpha[t, ] = dOmega + dPhi * mAlpha[t - 1, ] + rnorm(iN, 0, dTau)
    #compute the importance weights
    mW_tilde[t, ] = mW_tilde[t-1, ] * dnorm(vY[t], 0, exp(mAlpha[t, ]/2.0))
    #normalized importance weights
    vW =  mW_tilde[t, ]/sum(mW_tilde[t, ])
    #approximate E[exp(alpha_t) | y_{1:t}]
    vVol[t] = sum(exp(mAlpha[t, ]/2) * vW)
    #resample
    dESS = 1.0/sum(vW^2)
    if (dESS < dG * iN) {
      mAlpha[t, ] = sample(mAlpha[t, ], iN, replace = TRUE, prob = vW)
      mW_tilde[t, ] = 1.0
    }
  }
  
  return(vVol)
  
}

# 3) Simulate T = 1000 observations from the SV model reported in slide 12 of Lecture 8
# with omega = 0, phi = 0.9, and tau = 0.5. Set the seed to 123.
set.seed(2022)
iT <- 1000
dOmega <- 0
dPhi <- 0.9
dTau <- 0.5

SV_model_simulator_lecture_8_slide_12 <- function(iT, dOmega, dPhi, dTau) {
  
  epsilon <- rnorm(iT)
  #eta <- rnorm(1) # causes problems with outliers
  vAlpha <- numeric(iT)
  # The distribution of alpha comes from lectue 6 slide 19
  vAlpha[1] <- rnorm(1, dOmega/(1-dPhi), sqrt(dTau^2/(1-dPhi^2)))
  
  if (iT > 1) {
  for (t in 2:iT) {
    
    vAlpha[t] <- dOmega + dPhi*vAlpha[t-1] + dTau * rnorm(1)
  }
}
 
  vY <- exp(vAlpha/2)*epsilon
  
  lOut <- list("vY" = vY,
               "vAlpha" = vAlpha,
               "vSigma" = exp(vAlpha/2)
               )
  return(lOut)
  
}

# Simulating results
Simulated_results <- SV_model_simulator_lecture_8_slide_12(iT, dOmega, dPhi, dTau) 



# Perform filtering of the volatility using the Bootstrap filter you derived in point 2) using
# N = 10000 particles and g = 1. Repeat also for g = 0.75 and g = 0.5. Is the quality of
# the estimate affected? Also play with N and see how the number of particles affect the
# precision of the estimate.

# first we choose 10.000 to be the number of particles
iN <- 10000
# We set the seed to make it possible to replicate results
set.seed(123)
# we use the "normal" BootstrapFilter and store the results
# vY = Simulated_results$vY
vY <- Simulated_results$vY
set.seed(123)
vVol_Boot = BootstrapFilter(vY, dOmega, dPhi, dTau, iN)
set.seed(123)
vVol_Boot_g_75 <- BootstrapFilter_EES(vY, dOmega, dPhi, dTau, iN = 10000, dG = 0.75)
set.seed(123)
vVol_Boot_g_50 <- BootstrapFilter_EES(vY, dOmega, dPhi, dTau, iN = 10000, dG = 0.50)
set.seed(123)
vVol_Boot_g_1 <- BootstrapFilter_EES(vY, dOmega, dPhi, dTau, iN = 10000, dG = 1.00)


plot.ts(Simulated_results$vY, type = "l", col = "black")+
lines(vVol_Boot_g_1, type = "l", col = "red") +
lines(vVol_Boot_g_50, type=  "l", col = "orange") +
lines(vVol_Boot_g_75, type = "l", col = "yellow")

# Here we play around with the number of filters
iN <- 5
vY <- Simulated_results$vY
set.seed(123)
vVol_Boot = BootstrapFilter(vY, dOmega, dPhi, dTau, iN)
set.seed(123)
vVol_Boot_g_75 <- BootstrapFilter_EES(vY, dOmega, dPhi, dTau, iN = 5, dG = 0.75)
set.seed(123)
vVol_Boot_g_50 <- BootstrapFilter_EES(vY, dOmega, dPhi, dTau, iN = 5, dG = 0.50)
set.seed(123)
vVol_Boot_g_1 <- BootstrapFilter_EES(vY, dOmega, dPhi, dTau, iN = 5, dG = 1.00)


plot.ts(Simulated_results$vY, type = "l", col = "black")+
  lines(vVol_Boot_g_1, type = "l", col = "red") +
  lines(vVol_Boot_g_50, type=  "l", col = "orange") +
  lines(vVol_Boot_g_75, type = "l", col = "yellow")




# 5) Estimate the parameters !,  and  using the QML estimator you derived in Exercise
# Set 3. Perform filtering via the Bootstrap filter using the estimated parameters. Is the
# precision of the filtered volatility affected?


Estimated_QML_SV_results <- QML_SV(vY)

## Map the parameters in our parametrization
# This is the mapping:
# omega = (1-rho) * log(sigma^2)
dOmega_hat = log(Estimated_QML_SV_results$vPar["sigma"]^2)*(1.0 - Estimated_QML_SV_results$vPar["rho"])
# This is the mapping
# phi = rho
dPhi_hat = Estimated_QML_SV_results$vPar["rho"]
# This is the mapping
# sigma_eta^2 = tau^2 note that we need to sqrt(sigma_eta^2) to get tau
dTau_hat = sqrt(Estimated_QML_SV_results$vPar["sigma2eta"])

# number of particles
iN = 10000

#Filtering with the true parameters and g = 0.75
set.seed(123)
vVol_Boot_TRUE = BootstrapFilter_EES(vY, dOmega, dPhi, dTau, iN, dG = 0.75)
#Filtering with the estimated parameters and g = 0.75
set.seed(123)
# Note that we now use the _hat values
vVol_Boot_Est = BootstrapFilter_EES(vY, dOmega_hat, dPhi_hat, dTau_hat, iN, dG = 0.75)

# Let's compare the results in a figure

plot.ts(lSim$vSigma)
lines(vVol_Boot_TRUE, col = "red")
lines(vVol_Boot_Est, col = "blue")
## Results are similar, and estimator error does not play a crucial role in our
## example. We are then satisfied with the results.
## Note that, this example shows us that we will never be able to recover
## the "true" volatility recursion.



###########################
# (2): Real Data          #
###########################

# 1) Download the time series of the S&P500 index from Yahoo finance from 2005-01-01 to
# 2018-01-01 and compute the percentage log returns. Replace the zero returns with their
# empirical mean.

library(quantmod)
sp500 <- getSymbols(Symbols = "^GSPC", 
                    from = "2005-01-01",
                    to = "2018-01-01",
                    auto.assign = F)

lRet_sp500 <- as.numeric(diff(log(sp500[,6])*100)[-1])

lRet_sp500[lRet_sp500 == 0] <- mean(lRet_sp500)

# 2) Estimate the SV model by QML.

Estimated_QML_for_sp500 <- QML_SV(lRet_sp500)

# lets see the parameters:
Estimated_QML_for_sp500$vPar

# 3) Perform filtering using the Bootstrap filter with g = 1 and N = 10000.

# Mapping
# omega = (1-rho) * log(sigma^2)
dOmega_hat = log(Estimated_QML_for_sp500$vPar["sigma"]^2)*(1.0 - Estimated_QML_for_sp500$vPar["rho"])
# phi = rho
dPhi_hat = Estimated_QML_for_sp500$vPar["rho"]
# sigma_eta^2 = tau^2 note that we need to sqrt(sigma_eta^2) to get tau
dTau_hat = Estimated_QML_for_sp500$vPar["sigma2eta"]

BootstrapFilter_sp500 <- BootstrapFilter_EES(lRet_sp500, dOmega_hat, dPhi_hat, dTau_hat, iN = 10000, dG = 1.0)

# 4) Estimate a GARCH(1,1) with Gaussian shocks using the rugarch package.
library(rugarch)

# first we need to define "spec" which is the details of the GARCH model
spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   distribution.model = "norm")

fitted_GARCH_11_using_rugarch_package <- ugarchfit(spec, lRet_sp500)



# 5) Compare in a figure the series of filtered volatility from the SV model and the one
# obtained by the GARCH model.

# A function to find sigma from the model
vSigma_GARCH <- as.numeric(sigma(fitted_GARCH_11_using_rugarch_package))


plot(vSigma_GARCH, type = "l", col = "red", main = "Comparison of sigma")
lines(BootstrapFilter_sp500, type = "l", col = "blue")
legend("topright", legend = c("GARCH", "Bootstrap SV"), col = c("red", "blue"),
       lty = 1)


# Exercise set 5


# 2) Estimation

# Create a function to evaluate the likelihood of the GAS model of the previous exercise.

# We can choose different kinds of likelihoods
## MODEL NAME: Loglikelihood for a GAS model with student t Disp.
## INPUTS
 # vY as returns
 # dOmega long-run mean of variance process
 # dAlpha represents how fast the variance gets back to longrun level
 # dBeta Autoregressive coefficient
 # dNu (written as a v in derivation) degress of freedom of the t dist. 
GAS_MODEL_student_t_LLK <- function(vY, dOmega, dAlpha, dBeta, dNu) {
  
  iT = length(vY)
  
  vPhi = numeric(iT)
  vPhi_tilde = numeric(iT)
  
  #initialize at its unconditional value
  # Slide 18 lecture 9
  # its the E[phi]
  vPhi_tilde[1] = dOmega/(1.0 - dBeta)
  # This is found in the derivations, and is to make it a exponential function
  vPhi[1] = exp(vPhi_tilde[1])
  
  # compute the log density at time 1
  # dt = The Student t Distribution
  # Notes: 
  # The first part, dt(vY[1]/vPhi[1], dNu, log = TRUE), calculates the density of a student t-distribution with mean 0, scale parameter vPhi[1], and degrees of freedom dNu, evaluated at the value vY[1]/vPhi[1]. The log = TRUE argument specifies that the result should be returned as the logarithm of the density.
  # The second part, log(vPhi[1]), calculates the logarithm of vPhi[1].
  # The final result of the equation is the difference between the two parts, i.e. the log density of the student t-distribution minus the logarithm of vPhi[1]. This result is stored in the variable dLLK.
  # - log(vPhi[1]) is in slide 26 lecture 9
  dLLK = dt(vY[1]/vPhi[1], dNu, log = TRUE) - log(vPhi[1])
  
  for (t in 2:iT) {
    
    #update
    # This equation from derivation
    # This is the original values and not z_{t-1}
    vPhi_tilde[t] = dOmega + dAlpha * (((dNu + 1.0) * (vY[t - 1]/vPhi[t - 1])^2)/(dNu + (vY[t - 1]/vPhi[t - 1])^2) - 1.0) +
      dBeta * vPhi_tilde[t - 1]
    #map
    # This is also in derivations
    vPhi[t] = exp(vPhi_tilde[t])
    #density at time t
    dLLK = dLLK + dt(vY[t]/vPhi[t], dNu, log = TRUE) - log(vPhi[t])
  }
  #output the results
  lOut = list(vPhi = vPhi,
              dLLK = dLLK)
  
  return(lOut)
  
}

# What can the function be used for:
# This function may be used to estimate the parameters of a GAS model with a student t distribution, or to compare the fit of different GAS models with different sets of parameters. It may also be used to assess the goodness of fit of the model to a given set of data, or to make forecasts based on the model.


# This estimation function will be used later in the emperical part:

## NAME: Estimation of GAS model with t dist
## INPUTS:
 # vY as vector of returns
Estimate_GAS_MODEL_t_dist <- function(vY) {
  
  ##Starting values
  vPar = c(omega = log(sd(vY)*sqrt(3/5)) * 0.9,
           alpha = 0.05,
           phi = 0.9,
           nu = 5)
  
  #optimizer
  optimizer = optim(vPar, function(vPar, vY) {
    
    # The negative log likelihood is used in the optimization algorithm because it is a common practice to minimize a negative value rather than maximize a positive value. This allows the optimizer to search for the values of the parameters that minimize the negative log likelihood, which is equivalent to maximizing the log likelihood.
    dnLLK = -GAS_MODEL_student_t_LLK(vY, vPar[1], vPar[2], vPar[3], vPar[4])$dLLK
    
    # We can have problems when we summarize log likelihoods, so we make sure if the value is close to finite = infinity, that it gets the value 1000
    if (!is.finite(dnLLK)) {
      dnLLK = 1000
    }
    
    return(dnLLK)
    # Here we make sure that vNu is above 2 by setting lower bound to 2.01
  }, lower = c(-3.0, 0.0001, 0.00001, 2.01), upper = c(3.0, 2.0, 0.9999, 50),
  method = "L-BFGS-B", vY = vY)
  
  # estimated parameters
  vPar = optimizer$par
  
  #compute phi
  vPhi = GAS_MODEL_student_t_LLK(vY, vPar[1], vPar[2], vPar[3], vPar[4])$vPhi
  
  #compute sigma
  # sigma is given in the exercise as (sigma^2 = phi * v/(v-2)) then we need to compute the sqrt()
  vSigma = vPhi * sqrt(vPar[4]/(vPar[4] - 2.0))
  # In a GAS model for financial returns, vSigma is a vector of standard deviations of the distribution of the returns at each time period. The standard deviation is a measure of the dispersion or spread of the distribution around the mean, and it is often used to quantify the risk or uncertainty associated with a given set of returns.
  
  #output the results
  lOut = list(vSigma = vSigma,
              vPar = vPar,
              vPhi = vPhi)
  
  return(lOut)
  
}



# 3) Real data

# Download the time series of the S&P500 index from Yahoo finance from 2005-01-01 to
# 2018-01-01 and compute the percentage log returns. Replace the zero returns with their
# empirical mean.

library(quantmod)

SP500 <- getSymbols(Symbols = "^GSPC",
                     from = "2005-01-01",
                     to = "2018-01-01",
                     auto.assign = F)

lRet <- as.numeric(diff(log(SP500[,6])*100)[-1])

lRet[lRet == 0] <- mean(lRet)



# Estimate the GAS model you developed in point 1 using the code of point 2.

GAS_model_estimated_results <- Estimate_GAS_MODEL_t_dist(lRet)
GAS_model_estimated_results$vPar


# Compare the volatility implied by GAS and the ones from the GARCH and SV models
# you have estimated in the "Real Data" part of Exercise Set 4.

plot(GAS_model_estimated_results$vSigma, type = "l", col = "black", main = "Compare the volatility")
lines(vSigma_GARCH, type = "l", col = "red")
lines(BootstrapFilter_sp500, type = "l", col = "blue")
legend("topright", legend = c("GAS model", "GARCH", "Bootstrap SV"), col = c("black", "red", "blue"),
       lty = 1)

set.seed(2022)
iT = 1000; dRho = 0.9; dSigma2_eta = 0.25; dSigma = 1





# Exercise set  6

# Computational part

# Write a function to estimate a Gaussian DCC model with constant location. For univariate
# GARCH models you can use the rugarch package. The function should returns
# the total likelihood of the model, the filtered correlations and variances and estimated
# parameters.

# Because we now look at multiple variables, then we need to define arrays as storing objects
#mEta: a matrix of size iT x iN, where iT is the number of time points and iN is the number of financial assets. mEta could represent the residuals of a model that describes the returns of the financial assets over time.
#dA: a scalar value representing the autoregressive parameter of the DCC model. This parameter controls the degree to which the correlations between the financial assets at time t depend on their correlations at time t-1.
#dB: a scalar value representing the moving average parameter of the DCC model. This parameter controls the degree to which the correlations between the financial assets at time t depend on the residuals of the model at time t-1.
#mQ: a matrix of size iN x iN, representing the unconditional correlations between the financial assets. This matrix could be estimated from the data using a method such as maximum likelihood estimation.
DCCFilter <- function(mEta, dA, dB, mQ) {
  
  iN = ncol(mEta)
  iT = nrow(mEta)
  
  # initialize the array for the correlations
  aCor = array(0, dim = c(iN, iN, iT))
  # initialize the array for the Q matrices
  aQ = array(0, dim = c(iN, iN, iT))
  
  ## initialization at the unconditional cor
  aCor[,, 1] = mQ
  aQ[,,1] = mQ
  
  #Compute the first likelihood contribution
  # The likelihood contribution for a single time point is calculated as the squared norm of the residuals (i.e., the squared Frobenius norm) multiplied by the inverse of the correlations at that time point, minus the squared norm of the residuals, plus the log of the determinant of the correlations at that time point.
  # This is in the bottom of slide 35 lecture 10
  # -0.5 is added in the end of function
  # Correlation component:
  dLLK = mEta[1, , drop = FALSE] %*% solve(aCor[,, 1]) %*% t(mEta[1, , drop = FALSE]) -
    mEta[1, , drop = FALSE]%*% t(mEta[1, , drop = FALSE]) + log(det(aCor[,, 1]))
  
  #main loop
  for (t in 2:iT) {
    #update the Q matrix
    # Slide 33 lecture 10
    aQ[,, t] = mQ * (1 - dA - dB) + dA * t(mEta[t - 1, , drop = FALSE]) %*% mEta[t - 1, , drop = FALSE] +
      dB * aQ[,,t - 1]
    
    ## Compute the correlation as Q_tilde^{-1/2} Q Q_tilde^{-1/2}
    # Slide 31 lecture 10 
    aCor[,, t] = diag(sqrt(1/diag(aQ[,, t]))) %*% aQ[,, t] %*% diag(sqrt(1/diag(aQ[,, t])))
    
    #augment the likelihood value
    dLLK = dLLK + mEta[t, , drop = FALSE] %*% solve(aCor[,, t]) %*% t(mEta[t, , drop = FALSE]) -
      mEta[t, , drop = FALSE] %*% t(mEta[t, , drop = FALSE]) + log(det(aCor[,, t]))
  }
  
  lOut = list()
  #remember to include the -1/2 term in the likelihood evaluation
  #see the equations in the corresponding lecture
  lOut[["dLLK"]] = -0.5 * dLLK
  lOut[["aCor"]] = aCor
  
  return(lOut)
}



Estimate_DCC <- function(mY) {
  
  ## estimate the marginal GARCH models
  require(rugarch) #load the rugarch package
  require(Rsolnp)
  
  #####################################################
  # The following part of GARCH estimation can be written
  # in a nicer way by using the multifit function in the rugarch
  # package. See help(multifit)
  #############################################################
  
  #Marginal garch specifications
  SpecGARCH = ugarchspec(mean.model = list(armaOrder = c(0, 0)))
  
  #list where marginal models are stored
  lFit_univariate = list()
  
  #estimate the univariate GARCH models
  for(n in 1:ncol(mY)) {
    lFit_univariate[[n]] = ugarchfit(SpecGARCH, mY[, n])
  }
  
  #Compute the residuals
  mEta = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(residuals(Fit, standardize = TRUE))
  }))
  
  #####################################################
  
  ## maximization of the DCC likelihood
  
  #initial parameters
  # In the context of the Estimate_DCC function, vPar is used as the initial values for the autoregressive and moving average parameters of the DCC model. These parameters are then estimated using a maximum likelihood estimation method.
  vPar = c(0.04, 0.9)
  
  #unconditional correlation
  mQ = cor(mEta)
  
  #maximize the DCC likelihood
  # solnp function, which is used to estimate the parameters of the DCC model
  optimizer = solnp(vPar, fun = function(vPar, mEta, mQ) {
    
    # Here we use the DCCfilter
    Filter = DCCFilter(mEta, vPar[1], vPar[2], mQ)
    # The reason why as.numeric is used here is because the solnp function, which is used to estimate the parameters of the DCC model, expects a numeric vector as input. By converting Filter$dLLK to a numeric vector using as.numeric, we can ensure that the input is in the correct format for the solnp function.
    dNLLK = -as.numeric(Filter$dLLK)
    return(dNLLK)
    
    # The ineqfun argument in the solnp function is used to specify a function that defines inequality constraints for the optimization problem. The function should take the optimization variables as input and return a vector of constraint violations.
  }, ineqfun = function(vPar, ...) {
    sum(vPar)
  }, ineqLB = 1e-4, ineqUB = 0.999, # The UB and LB for optimization variables are within a reasonable range
  LB = c(1e-4, 1e-4), UB = c(0.999, 0.999), # The UB and LB for The two upper and lower bounds in this case are used to specify the lower and upper bounds for the optimization variables vPar.
  mEta = mEta, mQ = mQ)
  
  #Extract the estimated parameters
  # pars comes from "solnp" package
  vPar = optimizer$pars
  
  #Filter the dynamic correlation using the estimated parameters
  Filter = DCCFilter(mEta, vPar[1], vPar[2], mQ)
  
  #extract univariate volatilities
  mSigma = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(sigma(Fit))
  }))
  
  #extract univariate estimated parameters
  mCoef = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(coef(Fit))
  }))
  
  #compute the likelihood of the volatility  part
  dLLK_V = do.call(sum, lapply(lFit_univariate, function(Fit) {
    as.numeric(likelihood(Fit))
  }))
  
  #compute the total likelihood
  ## this can be computed in the following way or
  ## or as in slides 35 of lecture 10
  aCor = Filter[["aCor"]]
  aCov = array(NA, dim = dim(aCor))
  # The third dimension of aCor represents the time index, so dim(aCor)[3] will return the number of time points in the aCor array. 
  # The second dimension of aCor represents the index of the financial assets, so dim(aCor)[2] will return the number of financial assets in the aCor array.
  iT = dim(aCor)[3]
  iN = dim(aCor)[2]
  # dkern is a variable that is initialized to zero. It appears to be used as a counter to keep track of the sum of the logarithms of the probability densities of the data points given the model.
  dkern = 0
  for (t in 1:iT) {
    aCov[,,t] = diag(mSigma[t, ]) %*% aCor[,,t] %*% diag(mSigma[t, ])
    dkern = dkern + as.numeric(determinant(aCov[,,t], logarithm = TRUE)$modulus + t(t(mY[t, ])) %*% solve(aCov[,,t]) %*% t(mY[t, ]))
  }
  dLLK = -0.5*(iT * iN * log(2*pi) + dkern)
  
  ## Compute z_t
  # z_t being equal to sum_t^{1/2} * y_t
  iT = nrow(mY)
  
  mZ = matrix(0, iT, ncol(mY))
  
  for (t in 1:iT) {
    mZ[t, ] = diag(1/mSigma[t, ]) %*% solve(chol(aCor[,,t])) %*% as.numeric(mY[t, ])
  }
  
  BIC = log(iT) * 8 - 2 * dLLK
  
  lOut = list()
  
  #output the results
  lOut[["dLLK"]] = dLLK
  lOut[["mCoef"]] = mCoef
  lOut[["vPar"]] = vPar
  lOut[["mSigma"]] = mSigma
  lOut[["aCor"]] = aCor
  lOut[["mEta"]] = mEta
  lOut[["mZ"]] = mZ
  lOut[["BIC"]] = BIC
  
  return(lOut)
  
}


DCCFilter_for_t_dist <- function(mEta, dA, dB, mQ, dNu) {
  
  # number of observations
  
  iT = nrow(mEta)
  
  # number of variables
  
  iN = ncol(mEta)
  
  # initialize the array for the correlations
  
  aCor = array(0, dim = c(iN, iN, iT))
  
  # initialize the array for the Q matrices
  
  aQ = array(0, dim = c(iN, iN, iT))
  
  # initialization at the unconditional cor
  
  aCor[,, 1] = mQ
  aQ[,,1] = mQ
  
  # initialize the likelihood value
  
  dLLK = 0
  
  # main loop
  
  for (t in 2:iT) {
    # update the Q matrix
    aQ[,, t] = mQ * (1 - dA - dB) + dA * t(mEta[t - 1, , drop = FALSE]) %% mEta[t - 1, , drop = FALSE] +
      dB * aQ[,,t - 1]
    ## Compute the correlation as Q_tilde^{-1/2} Q Q_tilde^{-1/2}
    aCor[,, t] = diag(sqrt(1/diag(aQ[,, t]))) %% aQ[,, t] %*% diag(sqrt(1/diag(aQ[,, t])))
    
    
    # compute the likelihood contribution from this time step
    dLLK_t = mEta[t, , drop = FALSE] %*% solve(aCor[,, t]) %*% t(mEta[t, , drop = FALSE]) + 
      lgamma(0.5*(dNu+iN)) - lgamma(0.5*dNu) - 0.5*iN*log(dNu-2) - 0.5*(dNu+iN)*log(1+1/(dNu-2)*t(mEta[t, , drop = FALSE]) %*% solve(aCor[,, t]) %*% mEta[t, , drop = FALSE])
    # augment the likelihood value
    dLLK = dLLK + dLLK_t
  }
  
  #create the output list
  
  lOut = list()
  
  #remember to include the -1/2 term in the likelihood evaluation
  
  # see the equations in the corresponding lecture
  
  lOut[["dLLK"]] = -0.5 * dLLK
  lOut[["aCor"]] = aCor
  
  return(lOut)
}

Estimate_DCC_for_t_dist <- function(mY, dNu) {
  
  ## estimate the marginal GARCH models
  require(rugarch) #load the rugarch package
  require(Rsolnp)
  
  #####################################################
  # The following part of GARCH estimation can be written
  # in a nicer way by using the multifit function in the rugarch
  # package. See help(multifit)
  #############################################################
  
  #Marginal garch specifications
  SpecGARCH = ugarchspec(mean.model = list(armaOrder = c(0, 0)))
  
  #list where marginal models are stored
  lFit_univariate = list()
  
  #estimate the univariate GARCH models
  for(n in 1:ncol(mY)) {
    lFit_univariate[[n]] = ugarchfit(SpecGARCH, mY[, n])
  }
  
  #Compute the residuals
  mEta = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(residuals(Fit, standardize = TRUE))
  }))
  
  #####################################################
  
  ## maximization of the DCC likelihood
  
  #initial parameters
  # In the context of the Estimate_DCC function, vPar is used as the initial values for the autoregressive and moving average parameters of the DCC model. These parameters are then estimated using a maximum likelihood estimation method.
  vPar = c(0.04, 0.9)
  
  #unconditional correlation
  mQ = cor(mEta)
  
  #maximize the DCC likelihood
  # solnp function, which is used to estimate the parameters of the DCC model
  optimizer = solnp(vPar, fun = function(vPar, mEta, mQ, dNu) {
    
    # Here we use the DCCfilter_for_t_dist
    Filter = DCCFilter_for_t_dist(mEta, vPar[1], vPar[2], mQ, dNu)
    # The reason why as.numeric is used here is because the solnp function, which is used to estimate the parameters of the DCC model, expects a numeric vector as input. By converting Filter$dLLK to a numeric vector using as.numeric, we can ensure that the input is in the correct format for the solnp function.
    dNLLK = -as.numeric(Filter$dLLK)
    return(dNLLK)
    
    # The ineqfun argument in the solnp function is used to specify a function that defines inequality constraints for the optimization problem. The function should take the optimization variables as input and return a vector of constraint violations.
  }, ineqfun = function(vPar, ...) {
    sum(vPar)
  }, ineqLB = 1e-4, ineqUB = 0.999, # The UB and LB for optimization variables are within a reasonable range
  LB = c(1e-4, 1e-4), UB = c(0.999, 0.999), # The UB and LB for The two upper and lower bounds in this case are used to specify the lower and upper bounds for the optimization variables vPar.
  mEta = mEta, mQ = mQ, dNu = dNu)
  
  #Extract the estimated parameters
  # pars comes from "solnp" package
  vPar = optimizer$pars
  
  #Filter the dynamic correlation using the estimated parameters
  Filter = DCCFilter_for_t_dist(mEta, vPar[1], vPar[2], mQ, dNu)
  
  #extract univariate volatilities
  mSigma = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(sigma(Fit))
  }))
  
  #extract univariate estimated parameters
  mCoef = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(coef(Fit))
  }))
  
  #compute the likelihood of the volatility  part
  dLLK_V = do.call(sum, lapply(lFit_univariate, function(Fit) {
    as.numeric(likelihood(Fit))
  }))
  
  #create the output list
  lOut = list()
  #include the estimated parameters in the output
  lOut[["vPar"]] = vPar
  #include the estimated correlations in the output
  lOut[["mCor"]] = Filter[["aCor"]][,, dim(Filter[["aCor"]])[3]]
  #include the estimated volatilities in the output
  lOut[["mSigma"]] = mSigma
  #include the estimated coefficients in the output
  lOut[["mCoef"]] = mCoef
  #include the likelihood in the output
  lOut[["dLLK"]] = -optimizer$fn - dLLK_V
  #return the output
  return(lOut)
}




# Write a function to estimate a multivatiate Student’s t DCC model with constant locations
# where variances are estimated by QML in a first step and the degrees of freedom
# parameter is estimated in a second step. For univariate GARCH models you can use
# the rugarch package. The function should returns the total likelihood of the model,
# the filtered correlations and variances and estimated parameters.
Estimate_DCC_t_dist <- function(mY) {
  
  # QML estimation of Sigma_t
  
  # Here we use "Estimate_DCC" from above
  Fit_QML = Estimate_DCC(mY)
  
  # extract standardized residuals
  mZ = Fit_QML$mZ
  
  iT = nrow(mZ)
  iP = ncol(mZ)
  
  # Estimate a Multivariate Student's t
  # on the standardized residuals
  # 5 is because we expect dNu = v = 5 
  # The value 5 in optimizer = optim(5, function(dNu, mZ, iT, iP) { is the initial value for the optimization algorithm to start from. In this case, it is the initial value for the parameter dNu which is being estimated.
  optimizer = optim(5, function(dNu, mZ, iT, iP) {
    
    # This is a kernel function, which measures the distance between two datapoints in the data
    dKern = 0.0
    for (t in 1:iT) {
      dKern = dKern + log(1.0 + c(t(mZ[t, ]) %*% (mZ[t, ]))/(dNu - 2.0))
    }
    dLLK = iT * (lgamma(0.5*(dNu + iP)) - lgamma(0.5*dNu) - 0.5 * iP * log(dNu - 2.0)) - 0.5 * (dNu + iP) * dKern
    
    return(-dLLK)
    # dNu needs to be above 2.00 and therefore has lowerbound 2.01. 
  }, method = "L-BFGS-B", lower = 2.01, upper = 50, mZ = mZ, iT = iT, iP = iP)
  
  dNu = optimizer$par
  
  lOut = list()
  lOut[["Fit_QML"]] = Fit_QML
  lOut[["dNu"]] = dNu
  
  return(lOut)
  
}

# Two step estimation of DCC model with Student's t
# distributed shocks



# Estimation part

# 1) Consider a couple of assets of your choice from the dji30ret dataset which is available
# in the rugarch package.

# In this package "rugarch" there is stored data called "dji30ret"
library(rugarch)
# Here we load the data from the package
data("dji30ret")
# We load 1000 observations from the two first variables in the data
# First variable is AA
# Second variable is AXP
mY = dji30ret[1:1000, 1:2]
vY = dji30ret[1:1000, 1]
# Estimate a bivariate Gaussian DCC model.

# because our returns now is a matrix with two columns, then we can pass it into:
# Estimate_DCC(vY) doesn't work because is not a matrix
fit_DCC_gaussian <- Estimate_DCC(mY)

#estimate Student's t DCC in two step with QML for the first step
Fit_DCC_t_dist = Estimate_DCC_t_dist(mY)

# Comparing the series
plot.ts(fit_DCC_gaussian$aCor[1,2,], col = "red")
lines(Fit_DCC_t_dist$Fit_QML$aCor[2,1,], col = "blue")
# here we look at the difference between the values to see the difference and compare them
p <- plot(fit_DCC_gaussian$aCor[1,2,] - Fit_DCC_t_dist$Fit_QML$aCor[2,1,], type = "l")
#p

# Real question: Compare the two DCC models using BIC. Which model is selected?
# Leopoldo question: Compare the two DCC and copula models using BIC. Which model is selected?
# This point is subtle because we haven't computed the Likelihood for the DCC model with
# Student's t shocks. Remeber that we have estimated the model by QML !
# We first compute the likelihood of the DCC t model


mY = as.matrix(mY)
# numer of returns for each variable
iT = nrow(mY)
# number of different variables of returns (e.i. AAPL, MMM and so on)
iP = ncol(mY)

mSigma_DCC_t = Fit_DCC_t_dist$Fit_QML$mSigma
aCor_DCC_t   = Fit_DCC_t_dist$Fit_QML$aCor
dNu = Fit_DCC_t_dist$dNu

dLLK_DCC_t = 0

for (t in 1:iT) {
  
  mSigma = diag(mSigma_DCC_t[t, ]) %*% aCor_DCC_t[,,t] %*% diag(mSigma_DCC_t[t, ])
  
  dLLK_DCC_t = dLLK_DCC_t +
    lgamma(0.5*(dNu + iP)) - lgamma(0.5*dNu) - 0.5 * iP * log(dNu - 2.0) - 0.5 * log(det(mSigma)) +
    -0.5 * (dNu + iP) * log(1.0 + c(mY[t, ] %*% solve(mSigma) %*% mY[t, ]) /(dNu - 2.0))
}

vBIC = c(
  BIC_DCC_t        = log(iT) * 9 - 2 * dLLK_DCC_t,
  BIC_DCC_Gauss    = fit_DCC_gaussian$BIC
)

sort(vBIC)

# The model selected by BIC is DCC_t




# Exercise set 7

# Computational part

PattonFilter <- function(mU, CopType, dOmega, dAlpha, dBeta, dNu) {
  
  #lambda function
  LambdaFun <- function(x) {
    (1 - exp(-x))/(1 + exp(-x))
  }
  
  ##transforming the PIT
  # The code you provided is a part of the function that transforms the probability integral transform (PIT) of the input variables (mU). The PIT is a transformation that maps the input variables from the uniform distribution to the copula distribution.
  # if CopulaType (copType) is a normal distribution then mU draws is made from a normal dist.
  if (CopType == "norm") {
    mU_Tr = qnorm(mU)
  }
  # if CopulaType (copType) is a t distribution then mU draws is made from a t dist.
  if (CopType == "t") {
    mU_Tr = qt(mU, dNu)
  }
  # mU_Tr is "transformed" mU
  
  iT = nrow(mU)
  
  #initialize the correlation dynamic
  vCor = numeric(iT)
  # this is rho: 
  vCor[1] = cor(mU_Tr[, 1], mU_Tr[, 2]) #empirical one
  
  #compute the first likelihood contribution
  if (CopType == "norm") {
    norm.cop <- normalCopula(vCor[1]) # part of the library(copula) package
    # slide 22 lecture 11
    dLLK = dCopula(mU[1, ], norm.cop, log = TRUE)
  }
  if (CopType == "t") {
    t.cop = tCopula(vCor[1], df = dNu)
    dLLK = dCopula(mU[1, ], t.cop, log = TRUE)
  }
  
  # main loop
  for (t in 2:iT) {
    
    #update the correlation using the Patton's (2006) recursion
    # slide 32 lecture 11
    vCor[t] = LambdaFun(dOmega + dBeta * vCor[t - 1] + dAlpha * mU_Tr[t - 1, 1] * mU_Tr[t - 1, 2])
    
    #compute the lileihood contribution at time t
    if (CopType == "norm") {
      norm.cop <- normalCopula(vCor[t])
      # now the dLLK is for all vCor[t]
      dLLK = dLLK + dCopula(mU[t, ], norm.cop, log = TRUE)
    }
    if (CopType == "t") {
      t.cop = tCopula(vCor[t], df = dNu)
      # now the dLLK is for all vCor[t]
      dLLK = dLLK + dCopula(mU[t, ], t.cop, log = TRUE)
    }
    
  }
  
  #output the result
  lOut = list()
  lOut[["dLLK"]] = dLLK
  lOut[["vCor"]] = vCor
  
  return(lOut)
}

## NAME: Estimate_patton
## Is used for estimation of Patton models

Estimate_Patton <- function(mY, CopType) {
  
  ## estimate the marginal GARCH models
  require(rugarch) #load the rugarch package
  require(copula)
  require(Rsolnp)
  
  #specify the univariate GARCH models
  SpecGARCH = ugarchspec(mean.model = list(armaOrder = c(0, 0)), distribution.model = "std")
  
  #Estimate the univariate GARCH models
  lFit_univariate = list()
  
  # we make ugarch for each asset we have in mY
  for(n in 1:ncol(mY)) {
    lFit_univariate[[n]] = ugarchfit(SpecGARCH, mY[, n])
  }
  
  # Extract the PIT
  mU = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(pit(Fit))
  }))
  
  ## robustify
  # This is just to robustify the code and avoid numerical under/overflows. If you do qnorm of something too close to 1 or 0 you get a results that for R might coincide with plus or minus infinity, respectively.
  mU[mU > 0.999] = 0.999
  mU[mU < 0.001] = 0.001
  
  ## maximization of the Copula likelihood in the t and norm copula cases
  # here we impose the constraint alpha + beta < 1 to avoid explosive patterns
  # in the correlation parameter
  if (CopType == "norm") {
    # approximated unc cor
    dCor_app = cor(mU[, 1], mU[, 2]) * 0.16
    # approximated unmapped unc cor
    dOmega_starting = log(dCor_app + 1) - log(1 - dCor_app) 
    # Starting values for optimization
    vPar = c(dOmega_starting, 0.04, 0.8)
    
    optimizer = solnp(vPar, fun = function(vPar, mU) {
      #dNu = NA because is normal dist.
      Filter = PattonFilter(mU, CopType = "norm", vPar[1], vPar[2], vPar[3], dNu = NA)
      # remember to "-" in front
      dNLLK = - as.numeric(Filter$dLLK)
      
      if (!is.finite(dNLLK)) {
        dNLLK = 1e4
      }
      
      if (!is.numeric(dNLLK)) {
        dNLLK = 1e4
      }
      
      return(dNLLK)
      
    }, 
    LB = c(-3, -0.999, 1e-4), UB = c(3, 0.999, 0.9999), 
    mU = mU)
  }
  
  if (CopType == "t") {
    ##unmap initial value
    # approximated unc cor
    dCor_app = cor(mU[, 1], mU[, 2]) * 0.16
    # approximated unmapped unc cor
    dOmega_starting = log(dCor_app + 1) - log(1 - dCor_app) 
    vPar = c(dOmega_starting, 0.04, 0.8, 5)
    
    optimizer = solnp(vPar, fun = function(vPar, mU) {
      # dNu is specified because is student t dist. 
      # dNu is set to 5
      Filter = PattonFilter(mU, CopType = "t", vPar[1], vPar[2], vPar[3], dNu = vPar[4])
      dNLLK = -as.numeric(Filter$dLLK)
      
      if (!is.finite(dNLLK)) {
        dNLLK = 1e4
      }
      
      if (!is.numeric(dNLLK)) {
        dNLLK = 1e4
      }
      
      return(dNLLK)
      
    },  
    # Note we have one more parameter dNu, which is bounded att 2.01 to 30.
    LB = c(-3, -0.999, 1e-4, 2.01), UB = c(3, 0.999, 0.9999, 30), 
    mU = mU)
  }
  
  vPar = optimizer$pars
  # In the code you provided, dLLK_C is being set to the negative of the last element (tail) of the values obtained from the optimization process, which is stored in the "optimizer" object.
  # This being the copula LLK
  dLLK_C = - tail(optimizer$values, 1)
  
  # compute the filtered correlation parameter
  if (CopType == "norm") {
    Filter = PattonFilter(mU, CopType = "norm", vPar[1], vPar[2], vPar[3], dNu = NA)
  }
  if (CopType == "t") {
    Filter = PattonFilter(mU, CopType = "t", vPar[1], vPar[2], vPar[3], dNu = vPar[4])
  }
  
  #extract univariate volatilities
  mSigma = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(sigma(Fit))
  }))
  
  #extract univariate estimated parameters
  mCoef = do.call(cbind, lapply(lFit_univariate, function(Fit) {
    as.numeric(coef(Fit))
  }))
  
  #compute the likelihood of the univariate models
  dLLK_V = do.call(sum, lapply(lFit_univariate, function(Fit) {
    as.numeric(likelihood(Fit))
  }))
  
  #compute the total likelihood of the model
  dLLK = dLLK_V + dLLK_C
  
  # there must be 2 marginals here could be AAPL and AA
  # Marginals is the number of columnns of mY
  # mY columns could be AAPL, AA, SP500 
  if (CopType == "norm") {
    iK = 9 # 3 pars for each marginal + 3 for the copula
  }
  if (CopType == "t") {
    iK = 10 # 3 pars for each marginal + 4 for the copula
  }
  
  iT = nrow(mY)
  # BIC formula
  BIC = log(iT) * iK - 2 * dLLK
  
  lOut = list()
  
  #output the results
  lOut[["dLLK"]] = dLLK
  lOut[["mCoef"]] = mCoef
  lOut[["vPar"]] = vPar
  lOut[["mSigma"]] = mSigma
  lOut[["vCor"]] = Filter[["vCor"]]
  lOut[["BIC"]] = BIC
  
  return(lOut)
  
}


# Estimation part 

# 1) Consider the same couple of assets you choose in the previous exercise set.

# In this package "rugarch" there is stored data called "dji30ret"
library(rugarch)
# Here we load the data from the package
data("dji30ret")
# We load 1000 observations from the two first variables in the data
# First variable is AA
# Second variable is AXP
mY = dji30ret[1:1000, 1:2]

# 2) Estimate a the Patton’s model with Gaussian copula.
#it is time consuming
Fit_Patton_norm = Estimate_Patton(mY, CopType = "norm")


# 3) Estimate a the Patton’s model with Student’s t copula.

#it is time consuming
Fit_Patton_t = Estimate_Patton(mY, CopType = "t")

# 4) Compare the filtered copula correlation parameter of the Gaussian and Student’s t copula models.

plot.ts(Fit_Patton_t$vCor, type = "l",  col = "black", main = "Compare Patton model with normal- and t disp.")
lines(Fit_Patton_norm$vCor, type = "l", col = "red")
# The two are very similar


# 5) Compare the two copula models using BIC. Which model is selected?

# Here we store the BIC from the different models and compare the values:
vBIC = c(
  BIC_Patton_Gauss = Fit_Patton_norm$BIC,
  BIC_Patton_t     = Fit_Patton_t$BIC
)

sort(vBIC)
# The chosen model is student t because of lower BIC score.







