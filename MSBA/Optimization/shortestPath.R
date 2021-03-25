library(lpSolveAPI)

maint=c(2,4,5,9,12)
tradeIn=c(7,6,2,1,0)

cost=matrix(0,6,6)
for(i in 1:5){
  for(j in (i+1):6){
    cost[i,j]=sum(maint[1:(j-i)])+12-tradeIn[j-i]
  } 
}

SPP<-make.lp(0,6*6)

#set objective coefficients
set.objfn(SPP, as.vector(t(cost)))

#set objective direction
lp.control(SPP,sense='min')

nodes=c(1:6)
rhs=c(1,rep(0,4),-1)
for (n in 1:6){
  coef=c(l[n,1:6]/l[n,1:6],-l[1:6,n]/l[1:6,n])
  ind=c((n-1)*6+c(1:6),(c(1:6)-1)*6+n)
  nz=is.finite(coef)
  add.constraint(SPP,coef[nz], "=",rhs[n],ind[nz])                  
}

ColNames = c()
RowNames = c()
for(i in 1:6){
  for(j in 1:6){
    ColNames = cbind(ColNames,paste("x",i,",",j, sep=""))
  }
  RowNames=cbind(RowNames,paste("node",i))
}

dimnames(SPP) <- list(RowNames, ColNames)
set.type(SPP, c(1:36), "binary")

            
#write to text file
write.lp(SPP,'SPP.lp',type='lp')


#solve the model, if this return 0 an optimal solution is found
status=solve(SPP)

#this return the proposed solution
get.objective(SPP)
x=get.variables(SPP)

#lhs of constraints
get.constraints(SPP)

