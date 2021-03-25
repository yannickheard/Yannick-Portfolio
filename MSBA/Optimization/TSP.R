library(lpSolveAPI)

M=100000
N=5
cVec=c(M,132,217,164,58,132,M,290,201,79,217,290,M,113,303,164,201,113,M,196,58,79,303,196,M)
cMat=matrix(cVec,5,5)

# x11, x12, x13 ...x15, x21,....x55, u1,u2... u5,

#index of xij=> (j-1)*5+i

TSP<-make.lp(0,30)

#set objective coefficients
set.objfn(TSP, c(cVec,rep(0,5)))

#set objective direction
lp.control(TSP,sense='min')

for(j in 1:5){
  add.constraint(TSP,c(1,1,1,1,1), "=",1,(j-1)*5+c(1,2,3,4,5))
}

for(i in 1:5){
  add.constraint(TSP,c(1,1,1,1,1), "=",1,(c(1,2,3,4,5)-1)*5+i)
}

for(i in 2:5){
  for(j in 2:5){
    if(i!=j){
      add.constraint(TSP,c(1,-1,N), "<=",N-1,c(25+i,25+j,(j-1)*5+i))
    }
  }  
}


set.type(TSP, c(1:25), "binary")
set.type(TSP, c(26:30), "integer")

write.lp(TSP,'TSP.lp',type='lp')

status=solve(TSP)
get.objective(TSP)
x=get.variables(TSP)







