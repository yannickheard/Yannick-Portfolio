price = 1
delta = 0.8

#Terminal time
T = 3

#possible values of "s" and "t"
sValues=10*2^(0:T)
tValues = seq(0,T)

sN=length(sValues)
tN=length(tValues)

#Value function matrix
V = matrix(NA,sN,tN)
rownames(V) = sValues # add rownames and colnames to the V matrix
colnames(V) = tValues

#Action matrix
U = V # copy V matrix with rownames and colnames, to U matrix

#walk backwards in time
for (t in rev(tValues)){
  #for each time value the loop through the possible "tons of fish"  
  for (s in sValues[1:(t+1)]){
    
    if(t==tValues[tN]){
      
      # Boundary condition
      V[paste(s), paste(t)]=0
      
    }else{
      
      # Bellman Equation
      dontFish=delta * V[paste(s * 2), paste(t+1)]
      fish=0.7*s + delta * V[paste(s), paste(t+1)]
      
      V[paste(s), paste(t)] = max(c(dontFish,fish))
      U[paste(s), paste(t)] = which.max(c(dontFish,fish))
    } #if
    
  } #for s
} #for t
