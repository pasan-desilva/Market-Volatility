library(stats)
library(forecast)
library(datasets)
library(ggplot2)
library(tseries)
library(forecast)
library(readr)

berkshire <- read_csv("G:/BBsc Master Course/Semester 07/DA- 4420 Financial Econometrics/Assignments/Take Home Assignment/BRK-A.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y")))
summary (berkshire)

is.null(berkshire)
sapply(berkshire, mode)

#Identifying data from daily form
#Frequency = Average number of treading days in a year
berkshire_close <- ts(data = log(berkshire$Close), frequency = 252, start = c(2016,1))

#Graphical presentation of time series data
plot.ts(berkshire_close)

#Conducting Dickey Fuller Test for Staionarity
adf.test(berkshire_close)

#Checking for Structural Breaks
#Installing necessary packages
library(strucchange)
library(vars)

close_brake=Fstats(BRK_A~1, from = 0.01)
sctest(close_brake)

BRK_A <- ts(data = log(berkshire$`Adj Close`), frequency = 252, start = c(2016,1,1))
close_brakes <- breakpoints(BRK_A~1, h=0.1)
summary(close_brakes)

#Plotting the structural break impact on BRK-A price movement
plot(BRK_A)
lines(fitted(close_brakes, breaks = 7),col=4)
lines(confint(close_brakes,breaks=7))

#Creating Dummy Variables for the 7 Structural Break Episodes
berkshire$break1 <-1
berkshire$break1[1:217]<-0
berkshire$break2 <-1
berkshire$break2[1:357]<-0
berkshire$break3 <-1
berkshire$break3[1:482]<-0
berkshire$break4 <-1
berkshire$break4[1:652]<-0
berkshire$break5 <-1
berkshire$break5[1:778]<-0
berkshire$break6 <-1
berkshire$break6[1:926]<-0
berkshire$break7 <-1
berkshire$break7[1:1051]<-0

#Does the application of Structural breaks into the model make the time series stationary?
resid01 <- model01$residuals
adf.test(resid01)

#Checking the statistical significance of the Structural Breaks
model01 <- lm(BRK_A~break1+break2+break3+break4+break5+break6+break7, data=berkshire)
summary(model01)

#Conducting Augmented Dicker Fuller Test
adf.test(berkshire_close, alternative="explosive")

#Checking the Trend term and its significance
model02 <- dynlm(BRK_A ~ trend(BRK_A), data = BRK_A)
summary(model02)

#Detrending the model and checking for stationary processes
resid02 <- model02$residuals
adf.test(resid02)
adf.test(resid02, k=0)

#Changing the model for stationary using the differences; then Dickey Fuller Test
d.berkshire_close = diff(berkshire_close, differences = 1)
adf.test(d.berkshire_close)

#The Stationary model - Graphical Representation
plot.ts(d.berkshire_close)

#Utilization of ACF and PACF Functions
d.berkshire_close %>% ggtsdisplay()

#ACF and PACF using lagged values
acf(d.berkshire_close, lag=20)
pacf(d.berkshire_close, lag=20)

#Based on the Identification; 
##p=1 (strong evidence in the first lagged year)
##d=0 (The utilized model is already difference for staionarity)
##q=1-5(The most strong evidence for the lagged years)

#Constructing the ARIMA Model
arima01 <- Arima(d.berkshire_close, order = c(2,0,2))
arima02 <- Arima(d.berkshire_close, order = c(2,0,4))

#The results of ARIMA model
arima01
arima02

#Akaike (AIC) and Bayesian (BIC) Information Criterion for ARIMA models
#AIC
AIC(arima01,arima02)

#BIC
BIC(arima01,arima02)

#Ljung-Box Test for ARIMA Models 
#AIC = arima01
#BIC = arima02

checkresiduals(arima01)
checkresiduals(arima02)


#Hence ARIMA01 is selected as the best model based on higher P-Value

#Forecasting with ARIMA model
autoplot(forecast(arima02))

#Utilization of Auto ARIMA model
model_fit<-auto.arima(d.berkshire_close, seasonal = FALSE )
model_fit

autoplot(forecast(model_fit))

checkresiduals(model_fit)

#Measurement of Accuracy of both constructed ARIMA model, and the Auto ARIMA model.
#Taking 80% of data (2016-2019)
d.berkshire_close.train <- window(d.berkshire_close, end = c(2016,252))
arima.train01 <- Arima(d.berkshire_close.train, order = c(3,0,1))
arima.train02 <- Arima(d.berkshire_close.train, order = c(2,0,4), include.drift = TRUE)

#Taking the rest of 20% from the beginning of 2020
d.berkshire_close.test <- window(d.berkshire_close, end = c(2020,1))

#Checking Accuracy
accuracy(forecast(arima.train01, h = 252),d.berkshire_close.test)
accuracy(forecast(arima.train02, h = 252),d.berkshire_close.test)

#Loading Required Packages to build the VAR Model
library(vars)
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(tseries)
library(dynlm)
library("zoo")
library(urca)
library(mFilter)
library(tidyverse)
library(TSstudio)

#Loading the Dataset of Berkshire-Hathaway(A)
BRK_A <- read_csv("G:/BBsc Master Course/Semester 07/DA- 4420 Financial Econometrics/Assignments/Take Home Assignment/BRK-A.csv", 
                  col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                  Open = col_number(), High = col_number(), 
                  Low = col_number(), Close = col_number(), 
                  BRK = col_number(), Volume = col_number(), 
                  SP500 = col_number()))



#Initial Graph of two variables
ggplot(data = BRK_VAR)+geom_point(mapping = aes(x = SP500, y = BRK))

#Based on the residual relationship between X and Y variables, relatively linear relationship can be observed.
##(Beating the S&P index in the longer term with higher earnings and better P/E Ratio of under-performance)

BRK_VAR <- na.omit(BRK_A)
#Declaring Time series variables
BRK <- ts(BRK_VAR$BRK, start = c(2006,1,3), frequency = 252)
SP500 <- ts(BRK_VAR$SP500, start = c(2006,1,3), frequency = 252)

#Plotting the Series
#Combined:
autoplot(cbind(BRK,SP500))

#Individual:
par(mfrow = c(2, 1))
plot.ts(BRK, ylab = expression(paste("Closing Prices - BRK-A")))
plot.ts(SP500, ylab = expression(paste("Closing Prices - S&P 500")))

#At the moment, no structuring of the data is included; instead of letting the data to speak themselves.

#Running OLS
OLS1 <- lm(d.BRK ~ d.SP500)
summary(OLS1)
#In this scenario SP 500 affects BRK but not the other way around.

#Determine the persistence of the model
#BRK
acf(d.BRK, main = "ACF for BRK Closing prices")
pacf(d.BRK, main = "PACF for BRK Closing prices")

#S&P500
acf(d.SP500, main = "ACF for S&P 500 Closing prices")
pacf(d.SP500, main = "PACF for S&P 500 Closing prices")

#Checking for Unit Roots 
PP.test(BRK) #Non Significant - Non Stationary : Hence taking the First Difference
d.BRK =diff(BRK,differences = 1)
PP.test(d.BRK) 

PP.test(SP500) #Non Significant - Non Stationary : Hence taking the First Difference
d.SP500 =diff(SP500,differences = 1)
PP.test(d.BRK) 

#Testing for Co-integration
##Using Johansen Test
#Lag Selection Process = n-1
data1 <- read_csv("G:/BBsc Master Course/Semester 07/DA- 4420 Financial Econometrics/Assignments/Take Home Assignment/BRK-A.csv", 
                  col_types = cols(Date = col_date(format = "%m/%d/%Y")))
data_new <- na.omit(data1)
s <- ts(data_new$SP500, start = c(2006,1,3), frequency = 252)
b <- ts(data_new$BRK, start = c(2006,1,3), frequency = 252)
dataset <- cbind(s,b)
colnames(dataset) <- cbind("SP500", "BRK")
lagselectnew <- VARselect(dataset, lag.max = 10, type = "both")
lagselectnew$selection

#Since Lags = 8 - 1 -> 7

#Johansen Testing (Tracing)
ctest <- ca.jo(dataset, type = "trace", ecdet = "const", K=7)
summary(ctest)

#Johansen Testing (Maximum Eigen Value)
ctest1 <- ca.jo(dataset, type = "eigen", ecdet = "const", K=7)
summary(ctest)

#There are no Cointegrating relationships between the two variables.
##Thus, at the moment, no VECM is constructed.


#Finding the Optimal Autoregressive Lags
BRK_VAR.by <- cbind(d.BRK, d.SP500)
colnames(BRK_VAR.by) <- cbind("BRK_Clo", "SP500_Clo")
lagselect <- VARselect(BRK_VAR.by, lag.max = 10, type = "const")
lagselect$selection

#Building the VAR model
Model01 <- VAR(BRK_VAR.by, p=9, type = "const", season = NULL, exogen = NULL)
summary(Model01)

#Diagnosing the VAR
##Serial Correlation
serial1 <- serial.test(Model01, lags.pt = 9, type = "PT.asymptotic")
serial1

##Heteroscedasticity
arch1 <- arch.test(Model01, lags.multi = 9, multivariate.only = TRUE)
arch1

#Normal distribution of the residuals
norm1 <- normality.test(Model01, multivariate.only = TRUE)
norm1

#Testing for Structural breaks in the Residuals
stability1 <- stability(Model01, type = "OLS-CUSUM")
plot(stability1)

#Granger Causality Tests

##Both Directions : BRK$Granger -> SP500 and SP500$Granger -> BRK
GrangerBRK <- causality(Model01, cause = "BRK_Clo")
GrangerBRK

GrangerSP500 <- causality(Model01, cause = "SP500_Clo")
GrangerSP500

#Impulse Response Functions - How a Variable would behave n periods from now; if the variable gets shocked inside the system
##E.g: - What would happen to BRK_Clo if the SP_500 Clo are shocked
BRKirf <- irf(Model01, impulse = "SP500_Clo", response = "BRK_Clo", n.ahead = 20, boot = TRUE )
plot(BRKirf, ylab = "BRK", main = "Shock from SP500 Prices")

SP500irf <- irf(Model01, impulse = "BRK_Clo", response = "SP500_Clo", n.ahead = 20, boot = TRUE )
plot(SP500irf, ylab = "SP500", main = "Shock from BRK Prices")

#Variance Decomposition
FEVD1 <- fevd(Model01, n.ahead = 10)
plot(FEVD1)

#Structural VAR Forecasting 
forecast1 <- predict(Model01, n.ahead = 100, ci = 0.95)
fanchart(forecast1, names = "BRK_Clo")
fanchart(forecast1, names = "SP500_Clo")

#Structural Vector Autoregression - Checking for Contemporaneous Effects
#Re-modeling the data for ease of purpose
data1 <- read_csv("BRK-A.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))
data <- na.omit(data1)

#Converting into Time series Objects
s <- ts(data$SP500, start = c(2006,1,3), frequency = 252)
b <- ts(data$BRK, start = c(2006,1,3), frequency = 252)

#Interactive Plotting for each variable
ts_plot(s)
ts_plot(b)

#SVAR Restrictions
#Creating the Restriction Matrix (2X2)
amat <- diag(2)
amat

#Restrictions to be based on economic intuitions
amat[2,1] <- NA
amat

#Building the Model
s_var <- cbind(s,b)
colnames(s_var) <- cbind("SP500", "BRK")

#Checking for Contemporaneous Effects
a <- diag(1,2)
a[lower.tri(a)] <- NA
svar_est <- SVAR(Model01, Amat = a, max.iter = 100000) 


#Lag Order Selection
lagselectnew <- VARselect(s_var, lag.max = 10, type = "both")
lagselectnew$selection

#Estimating the Model
Model03 <- VAR(s_var, p=10, season = NULL, exogen = NULL, type = "const")
SVARModel01 <- SVAR(Model03, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod = c("scoring", "direct"))
SVARModel01

#Impulse Response Functions
SVAR_BRK <- irf(SVARModel01, impulse = "SP500", response = "BRK")
plot(SVAR_BRK)

SVAR_SP500  <- irf(SVARModel01, impulse = "SP500", response = "SP500")
plot(SVAR_SP500)

#Variance Decomposition
FEVD2 <- fevd(SVARModel01, n.ahead = 10)
plot(FEVD2)

#install.packages("fGarch")
#install.packages("rugarch")
#install.packages("aTSA")
library("fGarch")
library("rugarch")
library("aTSA")
library(readr)

#install.packages("fGarch")
#install.packages("rugarch")
#install.packages("aTSA")
library("fGarch")
library("rugarch")
library("aTSA")
library(readr)

#Loading Data to a Frame
data <- read_csv("G:/BBsc Master Course/Semester 07/DA- 4420 Financial Econometrics/Assignments/Homework 02/R-Code/BRK-A.csv")

#Assigning Adjusted Close Prices, for the frequency of annual trading days
BRK <-ts(data$Close, frequency = 252,  start = c(2016,01,4))

#Dickey-Fuller Test for BRK-A
adf.test(BRK)

#Adjusted-Prices Plot
plot.ts(BRK)

#Plotting the First Differences of the Adjusted Prices
returns <- diff(log(BRK), differences = 1)
plot.ts(returns)

#Augmented - Dickey-Fuller Test for BRK-A
adf.test(returns)

#ARCH heteroscedasticity test for residuals 
m1 <- arima(returns, order=c(2,0,4), include.mean=FALSE)
arch.test(m1, output=TRUE) 


#Auto Regressive Conditional Heteroskedastic Models (ARCH)
#ARCH(1)
model1 <- garchFit(~arma(2,4)+garch(1,0), returns, include.mean=FALSE, trace=FALSE)
model1
summary(model1)

#ARCH(2)
model1a <- garchFit(~arma(2,4)+garch(2,0), returns, include.mean=FALSE, trace=FALSE)
summary(model1a)

#ARCH(3)
model1b <- garchFit(~arma(2,4)+garch(3,0), returns, include.mean=FALSE, trace=FALSE)
summary(model1b)

#ARCH(4)
model1c <- garchFit(~arma(2,4)+garch(4,0), returns, include.mean=FALSE, trace=FALSE)
summary(model1c)

#ARCH(5)
model1d <- garchFit(~arma(2,4)+garch(5,0), returns, include.mean=FALSE, trace=FALSE)
summary(model1d)

#ARCH(6)
model1e <- garchFit(~arma(2,4)+garch(6,0), returns, include.mean=FALSE, trace=FALSE)
summary(model1e)#At ARCH(6) we get squared residuals that are white noise


#Generalized ARCH (GARCH) Modeling
#GARCH (1,1)
model2 <- garchFit(~arma(2,4)+garch(1,1), returns, include.mean=FALSE, trace=FALSE)
model2 #Both alpha1 and beta1 are statistically significant 
summary(model2)

#Prediction with Confidence Intervals (10 periods)
prediction <- predict(model2, n.ahead=10, plot=TRUE)

#Glosten-Jaganathan-Runkle GARCH (GJR-GARCH) Model
gjrgarch1 <- garchFit(formula = ~arma(2,4)+garch(1,1), leverage=T,returns,trace=F,include.mean=F)
summary(gjrgarch1)

#Exponential GARCH (EGARCH) model
egarch1 <- ugarchfit(ugarchspec(mean.model=list(armaOrder=c(2,4),include.mean=F),
                                variance.model=list(model="eGARCH",garchOrder=c(1,1))),returns)
egarch1  

#Application of additional enhancements of GARCH Modeling
#Threshold GARCH (T-GARCH) Model
fit.tgarch = garchFit(~arma(2,4)+garch(1,1),delta=1,leverage=T,data=returns,trace=F,include.mean=F)
fit.tgarch
summary(fit.tgarch)

#Taylor-Schwert GARCH (TS-GARCH) Model
fit.tsgarch = garchFit(~arma(2,4)+garch(1,1),delta=1,data=returns,trace=F,include.mean=F)
fit.tsgarch
summary(fit.tsgarch)
