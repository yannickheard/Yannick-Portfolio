price = 1
M=100
delta = 0.9
T=15

sValues = seq(0,M)
tValues = seq(0,T)

sN=length(sValues)
tN=length(tValues)

V=matrix(NA,sN,tN)
U=matrix(NA,sN,tN)
rownames(V) = sValues # add rownames and colnames to the V matrix
colnames(V) = tValues
U = V # add rownames and colnames to U matrix

for (t in rev(tValues)){
  for (s in sValues){

    if(t==tValues[tN]){
      V[paste(s),paste(t)]=0
      U[paste(s),paste(t)]=0
    }
    else{
      X = seq(0,s)
      valueChoices= price*X - (X^2)/(1+s) + delta*V[paste(s-X),paste(t+1)]
      
      V[paste(s),paste(t)]=max(valueChoices)
      U[paste(s),paste(t)]=X[which.max(valueChoices)]
    }
  }
}

s=100
for (t in (1:15)){
  print(paste("Year:", t , " Init Ore:",s, " Mine:", U[paste(s),paste(t)]))
  s=s-(U[paste(s),paste(t)])
}
  
