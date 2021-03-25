library(lpSolveAPI)

# y1.. y5, x1...x5

invLP<-make.lp(0,10)

#set objective coefficients
set.objfn(invLP, c(-25,-35,-28,-20,-40,10,18,11,9,10))

#set objective direction
lp.control(invLP,sense='max')

add.constraint(invLP,c(25,35,28,20,40,5,7,6,4,8), "<=",125)
add.constraint(invLP,c(1,1), "<=",1,c(2,5))
add.constraint(invLP,c(1,-1,-1), "<=",0,c(1,2,3))
add.constraint(invLP,c(-1,-1,-1), "<=",-2,c(3,4,5))

#yi - xi <=0
for(i in 1:5){
  add.constraint(invLP,c(1,-1), "<=",0,c(i,i+5))
}

#xi - yi*Max_i <=0
add.constraint(invLP,c(-5,1), "<=",0,c(1,6))
add.constraint(invLP,c(-4,1), "<=",0,c(2,7))
add.constraint(invLP,c(-5,1), "<=",0,c(3,8))
add.constraint(invLP,c(-7,1), "<=",0,c(4,9))
add.constraint(invLP,c(-3,1), "<=",0,c(5,10))

ColNames <- c("y1","y2","y3","y4","y5","x1","x2","x3","x4","x5")
dimnames(invLP)[2] <- list(ColNames)

set.type(invLP, c(6:10), "integer")
set.type(invLP, c(1:5), "binary")

write.lp(invLP,'inv.lp',type='lp')

solve(invLP)
get.objective(invLP)
x=get.variables(invLP)

#loop to do sensitivity analysis on budget

sVec=rep(0,51)
for(budget in 100:150){
  set.constr.value(invLP,budget,constraints = 1)
  solve(invLP)
  sVec[budget-99]=get.objective(invLP)
}



