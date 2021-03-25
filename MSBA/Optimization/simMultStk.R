# Simulating multiple stock prices

s0     = c(100,100,100)
T = 1         #no of years to run simulation
mu =  c(.1073,.0737,.0627)      #expected returns per year
cov = matrix(c(0.02778,0.00387,0.00021,0.00387,0.01112,-0.00020,0.00021,-0.00020,0.00115),3,3)   #volatility per year
dt= 1/260         #no of periods per year
nStks=length(s0)

w=c(1/3,1/3,1/3)

N=10000
Periods=T/dt
priceMat=matrix(NA,Periods,nStks)

muInt=mu*dt
covInt=cov*dt

balance=vector()
balance2=vector()
val=vector()
for(i in 1:N){   
  returns=muInt+ matrix(rnorm(Periods*nStks,0,1),Periods,nStks) %*% chol(covInt)
  priceMat=s0*apply(returns+1,2,cumprod)
  balance[i]=sum(priceMat[Periods,]*w)
  
  b[1]=100*( 1+ returns[1,] %*% w);
  for(t in 2:Periods){
  #valueReBal(t)=+ valueReBal(t-1)*x(2)*(1+returns(t,2))+valueReBal(t-1)*x(3)*(1+returns(t,3));
    b[t]=b[t-1] * (w %*% (1+returns[t,]))
  }
  balance2[i]=b[Periods]
}

#plotting one realization of the stock prices
#matplot(priceMat,,"l")

