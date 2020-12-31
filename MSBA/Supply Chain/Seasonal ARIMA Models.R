library(fpp)
#
plot(austourists)
La <- BoxCox.lambda(austourists)
y.tr <- window(austourists, end=c(2008,4))
y.te <- window(austourists, start=c(2009,1))
m.ets <- ets(y.tr, model="ZZZ", restrict=FALSE, lambda=La)
summary(m.ets)
tsdiag(m.ets, gof.lag=8)




tsdisplay(BoxCox(y.tr,La))
tsdisplay(diff(BoxCox(y.tr,La), lag=4))
adf.test(diff(BoxCox(y.tr,La), lag=4))
m.1 <- Arima(y.tr, order=c(1,0,0), seasonal=c(0,1,1), lambda=La)
summary(m.1)
plot(y.tr)
lines(fitted(m.1d), col="blue")


m.1d <- Arima(y.tr, order=c(1,0,0), seasonal=c(0,1,1), include.drift=TRUE, lambda=La)
summary(m.1d)
tsdiag(m.1d, gof.lag=8)
plot(y.tr)
lines(fitted(m.1d), col="blue")


m.2 <- Arima(y.tr, order=c(1,0,0), seasonal=c(1,1,0), lambda=La)
summary(m.2)
tsdiag(m.2, gof.lag=8)
plot(y.tr)
lines(fitted(m.1), col="blue")

m.2d <- Arima(y.tr, order=c(1,0,0), seasonal=c(1,1,0), include.drift=TRUE, lambda=La)
summary(m.2d)
tsdiag(m.2d, gof.lag=8)

plot(forecast(m.ets, h=8))
points(y.te, col="red", pch=19)

plot(forecast(m.1d, h=8))
points(y.te, col="red", pch=19)

plot(y.te, ylim=c(30,60))
fc.ets <- forecast(m.ets, h=8)
fc.m1d <- forecast(m.1d, h=8)
points(forecast(m.ets, h=8)$mean, col="red", pch=20)
points(forecast(m.1d, h=8)$mean, col="blue", pch=20)
accuracy(fc.ets, y.te)
accuracy(fc.m1d, y.te)



m.aa <- auto.arima(y.tr, lambda=La)
summary(m.aa)
tsdiag(m.aa, gof.lag=8)



