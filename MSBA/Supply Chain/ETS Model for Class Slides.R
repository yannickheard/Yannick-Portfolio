library(fpp)

plot(oil)



# Separate Training and Test data sets
oil.tr <- window(oil, start=1965, end=2000)
oil.te <- window(oil, start=2001)

# SES Model
m1.ses <- ets(oil.tr, model="ANN", alpha=0.1)
m2.ses <- ets(oil.tr, model="ANN", alpha=0.9)
summary(m1.ses)
summary(m2.ses)
plot(forecast(m2.ses, h=10, level=0), xlim=c(1965,2010), ylim=c(100,850), main="Forecasts from Simple Exponential Smoothing (SES)", ylab="SA Oil Prod. (MM Tons) ")
lines(fitted(m1.ses), col="orange", lwd=2, lty="dotted")
lines(forecast(m1.ses)$mean, col="orange", lwd=2)
lines(fitted(m2.ses), col="blue", lwd=2, lty="dotted")
lines(oil.te, col="red", lwd=2)
legend(x=1965, y=825, legend=c("Training Dataset",expression(paste("Fitted Values with ", alpha == 0.9, "   ")), expression(paste("Fitted Values with ", alpha == 0.1)), expression(paste("Forecast with ", alpha == 0.9)), expression(paste("Forecast with ", alpha == 0.1)),"Testing Dataset"), col=c("black","blue","orange","blue","orange", "red"), lty=c("solid","dotted","dotted","solid","solid", "solid"), lwd=c(1,2,2,2,2,2), cex=0.9)

State <- cbind(m2.ses$states,ts(c(NA,oil.tr), start=1964))
colnames(State) <- c("l_t", "y_t")
round(State,1)



# Holt's Model
str(livestock)
ls.tr <- window(livestock, end=1997)
ls.te <- window(livestock, start=1998)
#
m1.H <- ets(ls.tr, model="AAN", alpha = 0.9, beta = 0.8)
m2.H <- ets(ls.tr, model="AAN", alpha = 0.9, beta = 0.1)
plot(forecast(m2.H, h=10, level=0), xlim=c(1960,2010), ylim=c(200,600), main="Forecasts from Holt's Additive Trend Model", ylab="Livestock in Asia")
lines(fitted(m1.H), col="orange", lwd=2, lty = "dotted")
lines(fitted(m2.H), col="blue", lwd=2, lty="dotted")
lines(forecast(m1.H, h=10)$mean, col="orange", lwd=2)
lines(ls.te, col="red", lwd=2)
legend(x=1960, y=575, legend=c("Training Dataset",expression(paste("Fitted Values with ", alpha == 0.9," and ", beta==0.8,"     ")), expression(paste("Fitted Values with ", alpha == 0.1," and ", beta==0.1)), expression(paste("Forecast with ", alpha == 0.9," and ", beta==0.1)),expression(paste("Forecast with ", alpha == 0.9," and ", beta==0.9)), "Testing Dataset"), col=c("black","blue","orange", "blue","orange", "red"), lty=c("solid","dotted","dotted","solid","solid", "solid"), lwd=c(1,2,2,2,2,2), cex=0.9)

State.H <- cbind(m2.H$states,ts(c(NA,ls.tr), start=1960))
colnames(State.H) <- c("l_t","b_t", "y_t")
round(State.H,1)


plot(austourists, main="International Tourist Nights in Australia")


# Fit Holt-Winters Model to austourists
at.tr <- window(austourists, end=c(2008,4))
at.te <- window(austourists, start=c(2009,1))
m.HWA <- ets(at.tr, model="MAA", damped=FALSE, alpha=0.3, beta=0.1, gamma=0.4)
summary(m.HWA)
plot(forecast(m.HWA, h=8, level=0), xlim=c(1999,2011), ylim=c(20,60), main="Forecasts from Holt-Winters's Model with Additive Seasonality and Trend", ylab="International Visitor Nights")
lines(fitted(m.HWA), col="blue", lwd=2, lty="dotted")
lines(at.te, col="red", lwd=1)
legend(x=1999, y=59, legend=c("Training Dataset",expression(paste("Fitted Values with ", alpha == 0.3,"  ", beta==0.1," and ",gamma==0.4,"       ")), expression(paste("Forecast with ", alpha == 0.3,"   ", beta==0.1," and ",gamma==0.4,"     ")), "Testing Dataset"), col=c("black","blue", "blue","red"), lty=c("solid","dotted","solid","solid"), lwd=c(1,2,2,1), cex=0.9)

State.HWA <- cbind(m.HWA$states,ts(c(NA,ls.tr), start=1960))
colnames(State.HWA) <- c("l_t","b_t", "y_t")
round(State.HWA,1)
round(m.HWA$states,2)[1:5,]
round(at.tr,2)[1:12]

plot(m.HWA)


# Fit Unrestricted Optimal Model to austourists
fit.O <- ets(austourists, model="ZZZ", restrict =FALSE)
summary(fit.O)
plot(austourists, main="Optimized Forecast Model")
lines(fit.O$fitted, col="orange")


# Fit Holt-Winters Damped Model to austourists (Not Shown in Class Slides)
fit.HWDA <- ets(austourists, model="AAM", damped=TRUE, restrict =FALSE)
summary(fit.HWDA)
plot(austourists, main="Holt-Winters with Damped Trend Forecast")
lines(fit.HWDA$fitted, col="orange")


# Select Recent of History
vndata <- window(austourists, start=2005)
# Fit Holt-Winters Model to vndata
fit.HWV <- ets(vndata, model="AAM", damped=FALSE, restrict =FALSE)
summary(fit.HWV)
plot(austourists, main="Holt-Winters Forecast Based on Recent History")
lines(fit.HWV$fitted, col="orange")

# Fit Optimal Model to vndata
fit.OV <- ets(vndata, model="ZZZ", restrict =FALSE)
summary(fit.OV)
plot(austourists, main="Optimized Forecast Model Based on Recent History")
lines(fit.OV$fitted, col="orange")

# Forecast Based on Optimal Model with Restricted Data
fct.OV <- forecast(fit.OV,h=8)
summary(fct.OV)
plot(fct.OV)
lines(fit.OV$fitted, col="orange")

# Forecast Based on Optimal Model Using All History
fit.OA <- ets(austourists, model="ZZZ", restrict =FALSE)
fct.OA <- forecast(fit.OA,h=8)
summary(fct.OA)
plot(fct.OA)
lines(fit.OA$fitted, col="orange")

# Optimized Model with BoxCox Transformation
L <- BoxCox.lambda(austourists)
z <- BoxCox(austourists,L)
par(mfrow=c(2,1))
plot(austourists)
plot(z)
par(mfrow=c(1,1))

# Forecast Based on Optimal Model Using All History
fit.OABC <- ets(austourists, model="ZZZ", restrict =FALSE, lambda=L)
fct.OABC <- forecast(fit.OA,h=8)
summary(fct.OABC)
plot(fct.OABC)
lines(fit.OABC$fitted, col="orange")


