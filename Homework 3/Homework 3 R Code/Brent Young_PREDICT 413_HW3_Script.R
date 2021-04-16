#R Code
#Load the data & create time series
library(fpp)
setwd("~/R/PREDICT 413/Homework 3")
EC<- read.csv("Electricity.csv")

ECtimeseries <- ts(EC$Usage, frequency = 12, start= c(1971,1))
ECtimeseries

#EDA
summary(EC)
str(EC)

#Timeplot on Full Data
par(mfrow=c(2,1)) 
plot(ECtimeseries, ylab= "Usage", xlab= "Year", main= " Monthly Average Residential Electricity Usage for Iowa City 1971 - 1979")

#Seasonal Subseries Plot 
monthplot(ECtimeseries,ylab="Usage",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: Electricity Usage")
axis(1,at=1:12,labels=month.abb,cex=0.8)
par(mfrow=c(1,1)) 

#Stl Decomposition 
fit_stl <- stl(ECtimeseries, s.window = "periodic")
plot(fit_stl, main = "STL Decomposition on Full Dataset")

#Split Data into Train/Test: 80/20
ECtimeseries_train <-window(ECtimeseries, frequency = 12, start=c(1971, 1), end=c(1977, 12))
ECtimeseries_train
ECtimeseries_test <-window(ECtimeseries, frequency = 12, start=c(1978, 1), end=c(1979, 10))
ECtimeseries_test

#Model Selection
#ETS Model
fit1_ets <- ets(ECtimeseries_train)
summary(fit1_ets)

#Auto.Arima Pre-work
tsdisplay(ECtimeseries_train)

#Unit Root Tests
adf.test(ECtimeseries_train, alternative = "stationary")
kpss.test(ECtimeseries_train)

#Number of Differencing Required for Seasonal Data
ns <- nsdiffs(ECtimeseries_train)
if(ns > 0) {
  xstar <- diff(ECtimeseries_train,lag=frequency(ECtimeseries_train),differences=ns)
} else {
  xstar <- ECtimeseries_train
}
ns

#Differencing 
kpss.test(diff(ECtimeseries_train))
tsdisplay(diff(ECtimeseries_train))

#Auto.Arima Model 
fit2_arima <- auto.arima(ECtimeseries_train, stepwise=FALSE, approximation=FALSE)
summary(fit2_arima)

tsdisplay(residuals(fit2_arima))
Box.test(residuals(fit2_arima), fitdf=3, lag=10, type="Ljung")
tsdiag(fit2_arima)

#Neutral Net 
fit3_nn <- nnetar(ECtimeseries_train)
summary(fit3_nn)

#Holt Winters Multiplicative with damp
fit4_hw <- hw(ECtimeseries_train, seasonal = "multiplicative", damped = TRUE)
summary(fit4_hw)

#Benchmark
fit5_meanf<-meanf(ECtimeseries_train)
fit6_naive<-naive(ECtimeseries_train)
fit7_rwfdrift<-rwf(ECtimeseries_train, drift=TRUE)
fit8_snaive<-snaive(ECtimeseries_train)

#Training Set Accuracy - Summary
summary(fit1_ets) # training set
summary(fit2_arima) # training set
summary(fit3_nn) # training set
summary(fit4_hw) # training set
summary(fit5_meanf) # training set
summary(fit6_naive) # training set
summary(fit7_rwfdrift) # training set
summary(fit8_snaive) # training set

#Training Set Accuracy - Goodness-of-fit
accuracy(fit1_ets) # training set
accuracy (fit2_arima) # training set
accuracy (fit3_nn) # training set
accuracy (fit4_hw) # training set
accuracy (fit5_meanf) # training set
accuracy (fit6_naive) # training set
accuracy (fit7_rwfdrift) # training set
accuracy (fit8_snaive) # training set

#Forecast on Test Set
par(mfrow=c(3,2)) 
ETS_MNM <-forecast(fit1_ets, h=length(ECtimeseries_test))
plot(ETS_MNM, ylab="Usage")
lines(ECtimeseries, col="red",ylab="Actual")
ETS_MNM

Auto.ARIMA <-forecast(fit2_arima, h=length(ECtimeseries_test))
plot(Auto.ARIMA, ylab="Usage")
lines(ECtimeseries, col="red",ylab="Actual")
Auto.ARIMA

NN <-forecast(fit3_nn, PI=TRUE, h=length(ECtimeseries_test))
plot(NN, ylab="Usage")
lines(ECtimeseries, col="red",ylab="Actual")
NN

HW <-forecast(fit4_hw, h=length(ECtimeseries_test))
plot(HW, ylab="Usage")
lines(ECtimeseries, col="red",ylab="Actual")
HW

SNAIVE <-snaive(ECtimeseries_train, h=length(ECtimeseries_test))
plot(SNAIVE)
lines(ECtimeseries, col="red",ylab="Actual", ylab="Usage")
par(mfrow=c(1,1)) 

print(accuracy(ETS_MNM, ECtimeseries_test)) #best model
print(accuracy(Auto.ARIMA, ECtimeseries_test))
print(accuracy(NN, ECtimeseries_test))
print(accuracy(HW, ECtimeseries_test))
print(accuracy(SNAIVE, ECtimeseries_test))

#Diagnostics

#Box-Ljung test A
Box.test(ETS_MNM$residuals, lag=25, type = "Ljung-Box")

par(mfrow=c(2,2)) 
acf(ETS_MNM$residuals, lag.max=25)
plot(ETS_MNM$residuals, ylab = "Residuals")
abline(h = 0, col = "red")
hist(ETS_MNM$residuals, main = "", xlab = "Residuals")
par(mfrow=c(1,1)) 

#Box-Ljung test B
Box.test(Auto.ARIMA$residuals, lag=25, type = "Ljung-Box")

par(mfrow=c(2,2)) 
acf(Auto.ARIMA $residuals, lag.max=25)
plot(Auto.ARIMA $residuals, ylab = "Residuals")
abline(h = 0, col = "red")
hist(Auto.ARIMA $residuals, main = "", xlab = "Residuals")
par(mfrow=c(1,1)) 

#Box-Ljung test C
par(mfrow=c(2,2)) 
plot(NN$residuals, ylab = "Residuals")
abline(h = 0, col = "red")
hist(NN$residuals, main = "", xlab = "Residuals")
par(mfrow=c(1,1)) 

#Box-Ljung test D
par(mfrow=c(2,2)) 
plot(HW$residuals, ylab = "Residuals")
abline(h = 0, col = "red")
hist(HW$residuals, main = "", xlab = "Residuals")
par(mfrow=c(1,1)) 

#Forecast of Next 5 Months 
par(mfrow=c(3,2)) 
fit3 <- ets (ECtimeseries, model ="MNM")
ETS_MNM <-forecast(fit3, h=5)
ETS_MNM 
plot(ETS_MNM, ylab="Usage")

fit4 <- Arima(ECtimeseries, order=c(1,0,0), seasonal=c(0,1,1), include.drift=TRUE)
Auto.ARIMA <-forecast(fit4, h=5)
plot(Auto.ARIMA, ylab="Usage")
Auto.ARIMA

fit5 <- nnetar(ECtimeseries, order=c(1,1,2))
NN <-forecast(fit5, PI=TRUE, h=5)
plot(NN, ylab="Usage")
NN

fit6 <- hw(ECtimeseries, seasonal = "multiplicative", damped = TRUE)
HW <-forecast(fit6, h=5)
plot(HW, ylab="Usage")
HW

fit7 <- snaive (ECtimeseries, h=5)
plot(fit7)
par(mfrow=c(1,1)) 
