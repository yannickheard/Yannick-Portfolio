library(fpp)


# ACF and PACF non-seasonal
phi.1 = 0.8
theta.1=0.8
AR <- c(phi.1)
MA <- c(theta.1)
ACF.AR <- ARMAacf(ar=AR, lag.max=20, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR, lag.max=20, pacf=TRUE)
ACF.MA <- ARMAacf(ma=MA, lag.max=20, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ma=MA, lag.max=20, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(1,0,0)", " with ",phi[1]==0.8)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(1,0,0)", " with ",phi[1]==0.8)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF",  main=expression(paste("ARIMA(0,0,1)", " with ",theta[1]==0.8)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,1)", " with ",theta[1]==0.8)) )
abline(0,0)
set.seed(123)
yAR <- arima.sim(model=list(ar=AR), n=400)
yMA <- arima.sim(model=list(ma=MA), n=400)
par(mfrow=c(2,2))
Acf(yAR, main=expression(paste("ARIMA(1,0,0)", " with ",phi[1]==0.8)), lag.max=20)
Pacf(yAR, main=expression(paste("ARIMA(1,0,0)", " with ",phi[1]==0.8)), lag.max=20)
Acf(yMA, main=expression(paste("ARIMA(0,0,1)", " with ",theta[1]==0.8)), lag.max=20)
Pacf(yMA, main=expression(paste("ARIMA(0,0,1)", " with ",theta[1]==0.8)), lag.max=20)
par(mfrow=c(1,1))


# ACF and PACF non-seasonal Second Order
phi.1 = 0.5
phi.2 = 0.3
theta.1=0.5
theta.2 = 0.3
AR <- c(phi.1, phi.2)
MA <- c(theta.1, theta.2)
ACF.AR <- ARMAacf(ar=AR, lag.max=20, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR, lag.max=20, pacf=TRUE)
ACF.MA <- ARMAacf(ma=MA, lag.max=20, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ma=MA, lag.max=20, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(2,0,0)", " with ",phi[1]==0.5, " and ", phi[2]==0.3)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(2,0,0)", " with ",phi[1]==0.5, " and ", phi[2]==0.3)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF",  main=expression(paste("ARIMA(0,0,2)", " with ",theta[1]==0.5," and ", theta[2]==0.3)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,2)", " with ",theta[1]==0.5," and ", theta[2]==0.3)) )
abline(0,0)
set.seed(123)
yAR <- arima.sim(model=list(ar=AR), n=400)
yMA <- arima.sim(model=list(ma=MA), n=400)
par(mfrow=c(2,2))
Acf(yAR, main=expression(paste("ARIMA(2,0,0)", " with ",phi[1]==0.5, " and ", phi[2]==0.3)), lag.max=20)
Pacf(yAR, main=expression(paste("ARIMA(2,0,0)", " with ",phi[1]==0.5, " and ", phi[2]==0.3)), lag.max=20)
Acf(yMA, main=expression(paste("ARIMA(0,0,2)", " with ",theta[1]==0.5," and ", theta[2]==0.3)), lag.max=20)
Pacf(yMA, main=expression(paste("ARIMA(0,0,2)", " with ",theta[1]==0.5," and ", theta[2]==0.3)), lag.max=20)
par(mfrow=c(1,1))



# ACF and PACF non-seasonal Mixed
phi.1 = 0.8
theta.1=0.8
phi.2 = -0.8
theta.2=-0.8
AR1 <- c(phi.1)
MA1 <- c(theta.1)
AR2 <- c(phi.2)
MA2 <- c(theta.2)
ACF.AR <- ARMAacf(ar=AR1, ma=MA1, lag.max=20, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR1, ma=MA1, lag.max=20, pacf=TRUE)
ACF.MA <- ARMAacf(ar=AR2, ma=MA2, lag.max=20, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ar=AR2, ma=MA2, lag.max=20, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(1,0,1)", " with ",phi[1]==0.8, " and ", theta[1]==0.8)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(1,0,1)", " with ",phi[1]==0.8, " and ", theta[1]==0.8)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF",  main=expression(paste("ARIMA(1,0,1)", " with ",phi[1]==-0.8, " and ", theta[1]==-0.8)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(1,0,1)", " with ",phi[1]==-0.8, " and ", theta[1]==-0.8)) )
abline(0,0)
set.seed(123)
yAR <- arima.sim(model=list(ar=AR1, ma=MA1), n=400)
yMA <- arima.sim(model=list(ar=AR2, ma=MA2), n=400)
par(mfrow=c(2,2))
Acf(yAR, main=expression(paste("ARIMA(1,0,1)", " with ",phi[1]==0.8, " and ", theta[1]==0.8)), lag.max=20 )
Pacf(yAR, main=expression(paste("ARIMA(1,0,1)", " with ",phi[1]==0.8, " and ", theta[1]==0.8)), lag.max=20 )
Acf(yMA, main=expression(paste("ARIMA(1,0,1)", " with ",phi[1]==-0.8, " and ", theta[1]==-0.8)), lag.max=20)
Pacf(yMA, main=expression(paste("ARIMA(1,0,1)", " with ",phi[1]==-0.8, " and ", theta[1]==-0.8)), lag.max=20)
par(mfrow=c(1,1))





# Simulated Seasonal m=12 ACF and PACF
Phi = 0.8
Theta=0.8
AR <- c(0,0,0,0,0,0,0,0,0,0,0,Phi)
MA <- c(0,0,0,0,0,0,0,0,0,0,0,Theta)
ACF.AR <- ARMAacf(ar=AR, lag.max=48, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR, lag.max=48, pacf=TRUE)
ACF.MA <- ARMAacf(ma=MA, lag.max=48, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ma=MA, lag.max=48, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(0,0,0)(1,0,0)"[12], "   ",Phi[1]==0.8)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,0)(1,0,0)"[12], "   ",Phi[1]==0.8)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(0,0,0)(0,0,1)"[12], "   ",Theta[1]==0.8)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,0)(0,0,1)"[12], "   ",Theta[1]==0.8)) )
abline(0,0)
set.seed(123)
yAR <- arima.sim(model=list(ar=AR), n=400)
yMA <- arima.sim(model=list(ma=MA), n=400)
par(mfrow=c(2,2))
Acf(yAR, main=expression(paste("ARIMA(0,0,0)(1,0,0)"[12], "   ",Phi[1]==0.8)), lag.max=48)
Pacf(yAR, main=expression(paste("ARIMA(0,0,0)(1,0,0)"[12], "   ",Phi[1]==0.8)), lag.max=48)
Acf(yMA, main=expression(paste("ARIMA(0,0,0)(0,0,1)"[12], "   ",Theta[1]==0.8)), lag.max=48)
Pacf(yMA, main=expression(paste("ARIMA(0,0,0)(0,0,1)"[12], "   ",Theta[1]==0.8)), lag.max=48)
par(mfrow=c(1,1))



# Simulated Mixed Seasonal m=12 ACF and PACF
phi=0.3
Phi = 0.4
theta=0.3
Theta=0.4
AR <- c(phi,0,0,0,0,0,0,0,0,0,0,Phi)
MA <- c(theta,0,0,0,0,0,0,0,0,0,0,Theta)
ACF.AR <- ARMAacf(ar=AR, lag.max=48, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR, lag.max=48, pacf=TRUE)
ACF.MA <- ARMAacf(ma=MA, lag.max=48, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ma=MA, lag.max=48, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(1,0,0)(1,0,0)"[12], "   ",phi[1]==0.3," and ",Phi[1]==0.4)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(1,0,0)(1,0,0)"[12], "   ",phi[1]==0.3," and ",Phi[1]==0.4)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(0,0,1)(0,0,1)"[12], "   ",theta[1]==0.3," and ",Theta[1]==0.4)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,1)(0,0,1)"[12], "   ",theta[1]==0.3," and ",Theta[1]==0.4)) )
abline(0,0)
set.seed(123)
yAR <- arima.sim(model=list(ar=AR, ma=c(theta)), n=400)
yMA <- arima.sim(model=list(ma=MA, ar=c(phi)), n=400)
par(mfrow=c(2,2))
Acf(yAR, main=expression(paste("ARIMA(1,0,0)(1,0,0)"[12], "   ",phi[1]==0.4," and ",Phi[1]==0.5)), lag.max=48)
Pacf(yAR,main=expression(paste("ARIMA(1,0,0)(1,0,0)"[12], "   ",phi[1]==0.4," and ",Phi[1]==0.5)), lag.max=48)
Acf(yMA, main=expression(paste("ARIMA(0,0,1)(0,0,1)"[12], "   ",theta[1]==0.4," and ",Theta[1]==0.5)), lag.max=48)
Pacf(yMA, main=expression(paste("ARIMA(0,0,1)(0,0,1)"[12], "   ",theta[1]==0.4," and ",Theta[1]==0.5)), lag.max=48)
par(mfrow=c(1,1))

# Simulated Mixed AR-MA Seasonal m=12 ACF and PACF
phi=0.3
Phi = 0.4
theta=0.3
Theta=0.4
AR <- c(0,0,0,0,0,0,0,0,0,0,0,Phi)
ARns <- phi
MA <- c(0,0,0,0,0,0,0,0,0,0,0,Theta)
MAns <- theta
ACF.AR <- ARMAacf(ar=AR, ma=MAns,lag.max=48, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR, ma=MAns, lag.max=48, pacf=TRUE)
ACF.MA <- ARMAacf(ma=MA, ar=ARns, lag.max=48, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ma=MA, ar=ARns, lag.max=48, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(0,0,1)(1,0,0)"[12], "   ",theta[1]==0.3, " and ",Phi[1]==0.4)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,1)(1,0,0)"[12], "   ",theta[1]==0.3, " and ",Phi[1]==0.4)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(1,0,0)(0,0,1)"[12], "   ",phi[1]==0.3," and ",Theta[1]==0.4)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(1,0,0)(0,0,1)"[12], "   ",phi[1]==0.3," and ",Theta[1]==0.4)) )
abline(0,0)
set.seed(123)
yAR <- arima.sim(model=list(ar=AR, ma=c(theta)), n=400)
yMA <- arima.sim(model=list(ma=MA, ar=c(phi)), n=400)
par(mfrow=c(2,2))
Acf(yAR, main=expression(paste("ARIMA(0,0,1)(1,0,0)"[12], "   ",theta[1]==0.3, " and ",Phi[1]==0.4)), lag.max=48)
Pacf(yAR,main=expression(paste("ARIMA(0,0,1)(1,0,0)"[12], "   ",theta[1]==0.3, " and ",Phi[1]==0.4)), lag.max=48)
Acf(yMA, main=expression(paste("ARIMA(1,0,0)(0,0,1)"[12], "   ",phi[1]==0.3," and ",Theta[1]==0.4)), lag.max=48)
Pacf(yMA, main=expression(paste("ARIMA(1,0,0)(0,0,1)"[12], "   ",phi[1]==0.3," and ",Theta[1]==0.4)), lag.max=48)
par(mfrow=c(1,1))



# Simulated Seasonal m=4 ACF and PACF

Phi = 0.8
Theta=0.8
AR <- c(0,0,0,Phi)
MA <- c(0,0,0,Theta)
ACF.AR <- ARMAacf(ar=AR, lag.max=20, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR, lag.max=20, pacf=TRUE)
ACF.MA <- ARMAacf(ma=MA, lag.max=20, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ma=MA, lag.max=20, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(0,0,0)(1,0,0)"[4], " with ",Phi[1]==0.8)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,0)(1,0,0)"[4], " with ",Phi[1]==0.8)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF",  main=expression(paste("ARIMA(0,0,0)(0,0,1)"[4], " with ",Theta[1]==0.8)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,0)(0,0,1)"[4], " with ",Theta[1]==0.8)) )
abline(0,0)
set.seed(123)
yAR <- arima.sim(model=list(ar=AR), n=400)
yMA <- arima.sim(model=list(ma=MA), n=400)
par(mfrow=c(2,2))
Acf(yAR,  main=expression(paste("ARIMA(0,0,0)(1,0,0)"[4], " with ",Phi[1]==0.8)), lag.max=20)
Pacf(yAR,  main=expression(paste("ARIMA(0,0,0)(1,0,0)"[4], " with ",Phi[1]==0.8)), lag.max=20)
Acf(yMA,  main=expression(paste("ARIMA(0,0,0)(0,0,1)"[4], " with ",Theta[1]==0.8)), lag.max=20)
Pacf(yMA, main=expression(paste("ARIMA(0,0,0)(0,0,1)"[4], " with ",Theta[1]==0.8)), lag.max=20)
par(mfrow=c(1,1))




# Simulated Mixed Seasonal m=4 ACF and PACF

phi=0.3
Phi = 0.4
theta=0.3
Theta=0.4
AR <- c(phi,0,0,Phi)
MA <- c(theta,0,0,Theta)
ACF.AR <- ARMAacf(ar=AR, lag.max=20, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR, lag.max=20, pacf=TRUE)
ACF.MA <- ARMAacf(ma=MA, lag.max=20, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ma=MA, lag.max=20, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(1,0,0)(1,0,0)"[4], " with ",phi[1]==0.3," and ",Phi[1]==0.4)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(1,0,0)(1,0,0)"[4], " with ",phi[1]==0.3," and ",Phi[1]==0.4)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF",  main=expression(paste("ARIMA(0,0,1)(0,0,1)"[4], " with ",theta[1]==0.3," and ",Theta[1]==0.4)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,1)(0,0,1)"[4], " with ",theta[1]==0.3," and ",Theta[1]==0.4)) )
abline(0,0)

set.seed(123)
yAR <- arima.sim(model=list(ar=AR), n=400)
yMA <- arima.sim(model=list(ma=MA), n=400)
par(mfrow=c(2,2))
Acf(yAR, main=expression(paste("ARIMA(1,0,0)(1,0,0)"[4], " with ",phi[1]==0.3," and ",Phi[1]==0.4)), lag.max=20)
Pacf(yAR, main=expression(paste("ARIMA(1,0,0)(1,0,0)"[4], " with ",phi[1]==0.3," and ",Phi[1]==0.4)), lag.max=20)
Acf(yMA, main=expression(paste("ARIMA(0,0,1)(0,0,1)"[4], " with ",theta[1]==0.3," and ",Theta[1]==0.4)), lag.max=20)
Pacf(yMA, main=expression(paste("ARIMA(0,0,1)(0,0,1)"[4], " with ",theta[1]==0.3," and ",Theta[1]==0.4)), lag.max=20)
par(mfrow=c(1,1))




# Simulated Mixed AR-MA Seasonal m=4 ACF and PACF
phi=0.3
Phi = 0.4
theta=0.3
Theta=0.4
AR <- c(0,0,0,Phi)
ARns <- phi
MA <- c(0,0,0,Theta)
MAns <- theta
ACF.AR <- ARMAacf(ar=AR, ma=MAns,lag.max=20, pacf=FALSE)[-1]
PACF.AR <- ARMAacf(ar=AR, ma=MAns, lag.max=20, pacf=TRUE)
ACF.MA <- ARMAacf(ma=MA, ar=ARns, lag.max=20, pacf=FALSE)[-1]
PACF.MA <- ARMAacf(ma=MA, ar=ARns, lag.max=20, pacf=TRUE)
par(mfrow=c(2,2))
plot(ACF.AR, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(0,0,1)(1,0,0)"[4], "   ",theta[1]==0.3, " and ",Phi[1]==0.4)) )
abline(0,0)
plot(PACF.AR, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(0,0,1)(1,0,0)"[4], "   ",theta[1]==0.3, " and ",Phi[1]==0.4)) )
abline(0,0)
plot(ACF.MA, type="h", xlab="Lag", ylab="ACF", main=expression(paste("ARIMA(1,0,0)(0,0,1)"[4], "   ",phi[1]==0.3," and ",Theta[1]==0.4)) )
abline(0,0)
plot(PACF.MA, type="h", xlab="Lag", ylab="Partial ACF", main=expression(paste("ARIMA(1,0,0)(0,0,1)"[4], "   ",phi[1]==0.3," and ",Theta[1]==0.4)) )
abline(0,0)
set.seed(123)
yAR <- arima.sim(model=list(ar=AR, ma=c(theta)), n=400)
yMA <- arima.sim(model=list(ma=MA, ar=c(phi)), n=400)
par(mfrow=c(2,2))
Acf(yAR, main=expression(paste("ARIMA(0,0,1)(1,0,0)"[4], "   ",theta[1]==0.3, " and ",Phi[1]==0.4)), lag.max=20)
Pacf(yAR,main=expression(paste("ARIMA(0,0,1)(1,0,0)"[4], "   ",theta[1]==0.3, " and ",Phi[1]==0.4)), lag.max=20)
Acf(yMA, main=expression(paste("ARIMA(1,0,0)(0,0,1)"[4], "   ",phi[1]==0.3," and ",Theta[1]==0.4)), lag.max=20)
Pacf(yMA, main=expression(paste("ARIMA(1,0,0)(0,0,1)"[4], "   ",phi[1]==0.3," and ",Theta[1]==0.4)), lag.max=20)
par(mfrow=c(1,1))

