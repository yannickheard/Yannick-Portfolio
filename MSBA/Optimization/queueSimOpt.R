N=1000

#The avg service times from which we need to pick one
avgServiceTimes = seq(3,7,0.1)

#Corresponding Costs
cost =  5*avgServiceTimes^2 -  60*avgServiceTimes +200

AvgWaitTimes = rep(NA,length(avgServiceTimes))
PercentAnnoyed = rep(NA,length(avgServiceTimes))

#Random number generator's seed. Randomly picked and remembered for use in decision.
seed = sample(1000,1)

for (si in 1:length(avgServiceTimes) ){
  
  #reset seed everytime
  set.seed(seed)  
  
  #tau = sampled from an exponential with lambda=1/8
  tau = rexp(N-1,1/8) 
  #S = sampled from an exponential with mu = 1/avgServieTime
  S= rexp(N,1/avgServiceTimes[si])
  
  
  A = c(0,cumsum(tau));
  T = rep(NA,N)
  D = rep(NA,N)
  W = rep(NA,N)
  
  T[1] = 0
  D[1] = S[1]
  W[1] = 0
  
  for (i in 2:N){
    T[i] = max(D[i-1],A[i])
    D[i] = T[i] + S[i]
    W[i] = T[i] - A[i]
  }
  
  AvgWaitTimes[si] = mean(W)
  PercentAnnoyed[si] = mean(W>20)*100
}

obj = PercentAnnoyed+cost
plot(avgServiceTimes,obj,type="l")
