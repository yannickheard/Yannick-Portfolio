library(hts)
library(fpp)

View(vn)
plot(vn)
vn

colnames(vn)
vn.train <- window(vn, end=2009.9)
vn.test <- window(vn, start=2010)
#
vn.SN.train <- vn.train[,1]+vn.train[,2]
vn.SN.test <- vn.test[,1]+vn.test[,2]

vn.MV.train <- vn.train[,3]+vn.train[,4]
vn.MV.test <- vn.test[,3]+vn.test[,4]

vn.BQ.train <- vn.train[,5]+vn.train[,6]
vn.BQ.test <- vn.test[,5]+vn.test[,6]

vn.CO.train <- vn.train[,7]+vn.train[,8]
vn.CO.test <- vn.test[,7]+vn.test[,8]

vn.TA.train <- vn.SN.train+vn.MV.train+vn.BQ.train+vn.CO.train
vn.TA.test <- vn.SN.test+vn.MV.test+vn.BQ.test +vn.CO.test

#L.SYD <- BoxCox.lambda(vn.train[,1])
m.SYD <- auto.arima(vn.train[,1])
m.NSW <- auto.arima(vn.train[,2])
m.SN <- auto.arima(vn.SN.train)
#
summary(m.SYD)
summary(m.NSW)
summary(m.SN)
#
f.SYD <-forecast(m.SYD,h=8)
f.NSW <- forecast(m.NSW, h=8)
f.SN <- forecast(m.SN, h=8)
#
plot(forecast(m.SYD,h=8), ylab="Sydney")
points(fitted(m.SYD), col="red")
points(vn.test[,1], col="black", pch=19)
plot(forecast(m.NSW,h=8), ylab="NSW")
points(fitted(m.NSW), col="red")
points(vn.test[,2], col="black", pch=19)
plot(forecast(m.SN,h=8), ylab="Sydney+NSW")
points(fitted(m.SN), col="red")
points(vn.SN.test, col="black", pch=19)
#
#
m.MEL <- auto.arima(vn.train[,3])
m.VIC <- auto.arima(vn.train[,4])
m.MV <- auto.arima(vn.MV.train)
#
summary(m.MEL)
summary(m.VIC)
summary(m.MV)
#
f.MEL <-forecast(m.MEL,h=8)
f.VIC <- forecast(m.VIC, h=8)
f.MV <- forecast(m.MV, h=8)
#
plot(forecast(m.MEL,h=8), ylab="MEL")
points(fitted(m.MEL), col="red")
points(vn.test[,3], col="black", pch=19)
plot(forecast(m.VIC,h=8), ylab="VIC")
points(fitted(m.VIC), col="red")
points(vn.test[,4], col="black", pch=19)
plot(forecast(m.MV,h=8), ylab="MEL+VIC")
points(fitted(m.MV), col="red")
points(vn.MV.test, col="black", pch=19)
#
plot(vn.train[,3], ylim=c(4000,15000))
lines(vn.train[,4], col="red")
#
m.BRI <- auto.arima(vn.train[,5])
m.QLD <- auto.arima(vn.train[,6])
m.BQ <- auto.arima(vn.BQ.train)
#
summary(m.BRI)
summary(m.QLD)
summary(m.BQ)


#
f.BRI <-forecast(m.BRI,h=8)
f.QLD <- forecast(m.QLD, h=8)
f.BQ <- forecast(m.BQ, h=8)
#
plot(forecast(m.BRI,h=8), ylab="BRI")
points(fitted(m.BRI), col="red")
points(vn.test[,5], col="black", pch=19)
plot(forecast(m.QLD,h=8), ylab="QLD")
points(fitted(m.QLD), col="red")
points(vn.test[,6], col="black", pch=19)
plot(forecast(m.BQ,h=8), ylab="BRI+QLD")
points(fitted(m.BQ), col="red")
points(vn.BQ.test, col="black", pch=19)
#
plot(vn.train[,5], ylim=c(5000,15000))
lines(vn.train[,6], col="red")
#
m.CAP <- auto.arima(vn.train[,7])
m.OTH <- auto.arima(vn.train[,8])
m.CO <- auto.arima(vn.CO.train)
#
summary(m.CAP)
summary(m.OTH)
summary(m.CO)
#
f.CAP <-forecast(m.CAP,h=8)
f.OTH <- forecast(m.OTH, h=8)
f.CO <- forecast(m.CO, h=8)
#
plot(forecast(m.CAP,h=8), ylab="CAP")
points(fitted(m.CAP), col="red")
points(vn.test[,7], col="black", pch=19)
plot(forecast(m.OTH,h=8), ylab="OTH")
points(fitted(m.OTH), col="red")
points(vn.test[,8], col="black", pch=19)
plot(forecast(m.CO,h=8), ylab="CAP+OTH")
points(fitted(m.CO), col="red")
points(vn.CO.test, col="black", pch=19)
#
plot(vn.train[,7], ylim=c(6000,14000))
lines(vn.train[,8], col="red")
#
tsdisplay(vn.TA.train)
tsdisplay(diff(vn.TA.train,4))
adf.test(diff(vn.TA.train,4),alternative="stationary")
m.G1 <- Arima(vn.TA.train, order=c(1,0,0), seasonal=c(0,1,1))
summary(m.G1)
tsdiag(m.G1)
m.TA <- auto.arima(vn.TA.train)
summary(m.TA)
tsdiag(m.TA)
m.TAES <- ets(vn.TA.train, model="ZZZ", restrict=FALSE)
summary(m.TAES)
tsdiag(m.TAES)
#
plot(forecast(m.TA, h=8), ylab="Total Australia")
points(fitted(m.TA), col="red")
points(vn.TA.test, col="black", pch=19)
#
#
h.vn <- hts(vn.train, nodes=list(4,c(2,2,2,2)))
summary(h.vn)
allts(h.vn)


fbu.ets <- forecast(h.vn, h=8, method = "bu", fmethod = "ets", restrict = FALSE)
plot(fbu.ets)

fbu.A <- forecast(h.vn, h=8, method = "bu", fmethod = "arima")
plot(fbu.A)

ftdha.A <- forecast(h.vn, h=8, method = "tdgsf", fmethod = "arima")
plot(ftdha.A)
allts(ftdha.A)

ftmo.A <- forecast(h.vn, h=8, method = "mo", level = 1, fmethod = "arima")
plot(ftmo.A)


print(fbu.A)

allts(fbu.A)

allts(h.vn)

