library(lpSolveAPI)


propCosts=c(12.50,12.55,12.70,12.80,12.85,12.95) 
demand=c(10000,15000,30000,35000,25000,10000)
M=sum(demand)
I0=5000

# x1...x6, y1... y6, I1... I6

pigSkinLP<-make.lp(0,18)

#set objective coefficients
set.objfn(pigSkinLP, c(propCosts,rep(50000,6),propCosts*0.05))

#set objective direction
lp.control(pigSkinLP,sense='min')

#Balance 
add.constraint(pigSkinLP,c(1,-1), "=",demand[1]-I0,c(1,1+12))
for(i in 2:6){
  add.constraint(pigSkinLP,c(1,1,-1), "=",demand[i],c(i-1+12,i,i+12))

}



for(i in 1:6){
  #Inv Const.
  add.constraint(pigSkinLP,c(1), "<=",20000,c(i+12))
  #yi<=xi
  add.constraint(pigSkinLP,c(1,-1), "<=",0,c(i+6,i))
  #xi<=Myi
  add.constraint(pigSkinLP,c(1,-M), "<=",0,c(i,i+6))
}

ColNames <- c("x1","x2","x3","x4","x5","x6","y1","y2","y3","y4","y5","y6","I1","I2","I3","I4","I5","I6")
dimnames(pigSkinLP)[2] <- list(ColNames)

set.type(pigSkinLP, c(7:12), "binary")

write.lp(pigSkinLP,'pigSkip.lp',type='lp')

status=solve(pigSkinLP)
get.objective(pigSkinLP)
x=get.variables(pigSkinLP)











