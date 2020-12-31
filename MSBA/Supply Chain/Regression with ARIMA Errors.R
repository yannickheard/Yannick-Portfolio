library(fpp)


plot(insurance)
View(insurance)

# ARIMA Model
tsdisplay(insurance[,1])
adf.test(insurance[,1])

tsdisplay(diff(insurance[,1],12), lag=24)
adf.test(diff(insurance[,1],12))

tsdisplay(diff(insurance[,1]), lag=24)
adf.test(diff(insurance[,1]))

fit.AA <- auto.arima(insurance[,1])
summary(fit.AA)
tsdiag(fit.AA, gof.lag=10)

plot(forecast(fit.AA, h=18))
points(fitted(fit.AA), col="red", pch=19)

fit.SG <- Arima(insurance[,1], order=c(1,0,0), seasonal=c(0,1,0))
summary(fit.SG)
tsdiag(fit.SG, gof.lag=24)

plot(forecast(fit.SG, h=18))
points(fitted(fit.SG),col="red", pch=19)

# Linear Regression Model
attach(data.frame(insurance))
plot(insurance)
plot(Quotes ~ TV.advert)
cor(Quotes, TV.advert)

fit.LR <- lm(Quotes ~ TV.advert)
summary(fit.LR)
tsdisplay(residuals(fit.LR))

# Dynamic Regression 
ins.Q <- insurance[,"Quotes"]
ins.A <- insurance[,"TV.advert"]

ins.A1 <- Arima(ins.Q, xreg=ins.A, order=c(0,1,1))
summary(ins.A1)
tsdisplay(diff(arima.errors(ins.A1)))
tsdiag(ins.A1, gof.lag=10)

ins.A2 <- Arima(ins.Q, xreg=ins.A, order=c(2,1,0))
summary(ins.A2)
tsdisplay(diff(arima.errors(ins.A2)))
tsdiag(ins.A2, gof.lag=10)

ins.AA <- auto.arima(ins.Q, xreg=ins.A)
summary(ins.AA)
tsdiag(ins.AA, gof.lag=10)

plot(ins.Q)
points(fitted(fit.AA),col="red", pch=19)
points(fitted(ins.A2), col="blue", pch=19)
sqrt(mean((ins.Q-fitted(fit.AA))^2))
sqrt(mean((ins.Q-fitted(ins.A2))^2))


#Forecasting with ins.A2 model
ins.AP <- c(9,10,11,10,9,8,8,9,9,10,10,10)
ins.A2F <- forecast(ins.A2,xreg=ins.AP, h=12)
ins.A2F
plot(ins.A2F)
points(fitted(ins.A2), col="red", pch=19)


FV <- window(fitted(ins.A2), start=c(2004,1))
RE <- window(residuals(ins.A2), start=c(2004,1))
AE <- window(arima.errors(ins.A2), start=c(2004,1))
y <- window(insurance[,1], start=c(2004,1))
xr <- window(insurance[,2], start=c(2004,1))
round(cbind(y,xr,AE,FV,RE),3)

# Analysis of Advertising lagged effects

A.L1 <- ts(c(ins.A), start=c(2002,1), frequency=12)
A.L2 <- ts(c(NA,ins.A), start=c(2002,1), frequency=12)
A.L3 <- ts(c(NA,NA,ins.A), start=c(2002,1), frequency=12)
A.L4 <- ts(c(NA,NA,NA,ins.A), start=c(2002,1), frequency=12)
ins.Q
XA <- cbind(A.L1,A.L2,A.L3,A.L4)
XA
Quotes.R <- window(ins.Q, start=c(2002,4))
XA.R <- window(XA,start=c(2002,4), end=c(2005,4))
Quotes.R
XA.R

m.L1 <- auto.arima(Quotes.R, xreg=XA.R[,1], d=0)
summary(m.L1)
tsdiag(m.L1)
m.L2 <- auto.arima(Quotes.R, xreg=XA.R[,1:2], d=0)
summary(m.L2)
tsdiag(m.L2)
m.L3 <- auto.arima(Quotes.R, xreg=XA.R[,1:3], d=0)
summary(m.L3)
tsdiag(m.L3)
m.L4 <- auto.arima(Quotes.R, xreg=XA.R, d=0)
summary(m.L4)
tsdiag(m.L4)
