
library(quadprog)
m=c(0.1073,0.0737,0.0627)
s=c(0.1667,0.1055,0.0340)

rho=matrix(c(1,0.2199,0.0366,0.2199,1,-0.0545,0.0366,-0.0545,1),3,3,byrow=TRUE)

covMat=diag(s) %*% rho %*% diag(s)

RVals=seq(0.065,0.105,0.005)
StdDevs=rep(0,length(RVals))

for (i in 1:length(RVals)){

  Dmat=2*covMat
  dvec=rep(0,3)
  Amat=matrix(c(1,1,1,-1,-1,-1,m),3)
  bvec=c(1,-1,RVals[i])
  
  #no shorting
  Amat=cbind(Amat,diag(3))
  bvec=c(bvec,0,0,0)

  S=solve.QP(Dmat,dvec,Amat,bvec)
  StdDevs[i]=sqrt(S$value)
  
}


plot(StdDevs,RVals,"l")
