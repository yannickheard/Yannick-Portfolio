mu=0.1110941
sigma=0.2526696
p0=174

#time in yrs
T=1
#simulation resolution: in days
dt=1/260

nPeriods=T/dt

muInt=mu*dt
sigmaInt=sigma*sqrt(dt)
N=10000

simulatedPrices = matrix(0,nPeriods,N)
price = vector()
for (n in 1:N){
  
  #generate input for my simulation model
  r=rnorm(nPeriods,muInt,sigmaInt)
  
  #simulation model 
  #   price[1]=p0*(1+r[1])
  #   for (t in 2:nPeriods){
  #     price[t]=price[t-1]*(1+r[t])    
  #   }

  #simulation mode in vector form
  price=p0*cumprod(1+r)
  
  #record outputs
  simulatedPrices[,n]=price
  
}

#probability of finishing above 200 in 1 yr
mean(simulatedPrices[nPeriods,]>200)

