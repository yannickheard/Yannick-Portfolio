library(lpSolveAPI)

cost=matrix(c(14,5,8,7,2,12,6,5,7,8,3,9,2,4,6,10),4,4,byrow=TRUE)

assignOpt<-make.lp(0,16)

#set objective coefficients
set.objfn(assignOpt, as.vector(t(cost)))

#set objective direction
lp.control(assignOpt,sense='min')

add.constraint(assignOpt,rep(1,4), "=",1,c(1:4))
add.constraint(assignOpt,rep(1,4), "=",1,c(5:8))
add.constraint(assignOpt,rep(1,4), "=",1,c(9:12))
add.constraint(assignOpt,rep(1,4), "=",1,c(13:16))


add.constraint(assignOpt,rep(1,4), "=",1,c(1,5,9,13))
add.constraint(assignOpt,rep(1,4), "=",1,c(1,5,9,13)+1)
add.constraint(assignOpt,rep(1,4), "=",1,c(1,5,9,13)+2)
add.constraint(assignOpt,rep(1,4), "=",1,c(1,5,9,13)+3)


ColNames <- c("x11","x12","x13","x14","x21","x22","x23","x24","x31","x32","x33","x34","x41","x42","x43","x44")
RowNames <- c("machine1","machine2","machine3","machine4","job1","job2","job3","job4")
dimnames(assignOpt) <- list(RowNames, ColNames)


set.type(lanProj, c(1:16), "binary")

#write to text file
write.lp(assignOpt,'assignOpt.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(assignOpt)

#this return the proposed solution
get.objective(assignOpt)
x=get.variables(assignOpt)

#lhs of constraints
get.constraints(assignOpt)

#sensitivity:
d=get.sensitivity.rhs(assignOpt)
c=get.sensitivity.obj(assignOpt)