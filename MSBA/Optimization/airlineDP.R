priceL = 180
priceH = 300
pL = c(0.3,0.7)
pH = c(0.6,0.4)

delta = 0.98

M=100
T=365

sValues = seq(0,M)
tValues = seq(0,T)

sN=length(sValues)
tN=length(tValues)

V=matrix(NA,sN,tN)
rownames(V)=sValues
colnames(V)=tValues
U=V

for (t in rev(tValues)){
  for (s in sValues){


    if(t==T){
      V[paste(s),paste(t)]=0
      U[paste(s),paste(t)]=0
    }
    else if(s==0){
      V[paste(s),paste(t)]=0
      U[paste(s),paste(t)]=0
    }
    else{

      valueL = pL[2]*priceL + delta* (pL[1]*V[paste(s),paste(t+1)] + pL[2]*V[paste(s-1),paste(t+1)])
      valueH = pH[2]*priceH + delta* (pH[1]*V[paste(s),paste(t+1)] + pH[2]*V[paste(s-1),paste(t+1)]) 

      V[paste(s),paste(t)]=max(c(valueL,valueH))
      U[paste(s),paste(t)]=which.max(c(valueL,valueH))
    }
    
  }
}

image(tValues,sValues,t(U))

