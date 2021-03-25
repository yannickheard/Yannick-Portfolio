N=5000

#tau = sampled from 1 to 15
tau = sample(15,N-1,replace=TRUE)
#S = sampled from (5,10) with prob 0.7 and 0.3 respectively
S= sample(c(5,10),N,prob=c(0.7,0.3),replace=TRUE)  

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

AvgWaitTimes = mean(W)
PercentAnnoyed = mean(W>20)*100


