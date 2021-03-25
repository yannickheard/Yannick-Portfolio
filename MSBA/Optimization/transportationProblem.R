library(lpSolveAPI)

cost=matrix(c(8,9,14,6,12,9,10,13,16,9,7,5),3,4)
supply=c(35,50,40)
demand=c(45,20,30,30)

tranOpt<-make.lp(0,12)

#set objective coefficients
set.objfn(tranOpt, as.vector(t(cost)))

#set objective direction
lp.control(tranOpt,sense='min')

add.constraint(tranOpt,rep(1,4), "<=",supply[1],c(1:4))
add.constraint(tranOpt,rep(1,4), "<=",supply[2],c(5:8))
add.constraint(tranOpt,rep(1,4), "<=",supply[3],c(9:12))

add.constraint(tranOpt,rep(1,3), ">=",demand[1],c(1,5,9))
add.constraint(tranOpt,rep(1,3), ">=",demand[2],c(1,5,9)+1)
add.constraint(tranOpt,rep(1,3), ">=",demand[3],c(1,5,9)+2)
add.constraint(tranOpt,rep(1,3), ">=",demand[4],c(1,5,9)+3)

ColNames <- c("x11","x12","x13","x14","x21","x22","x23","x24","x31","x32","x33","x34")
RowNames <- c("supply1","supply2","supply3","demand1","demand2","demand3","demand4")
dimnames(tranOpt) <- list(RowNames, ColNames)

#write to text file
write.lp(tranOpt,'transp.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(tranOpt)

#this return the proposed solution
get.objective(tranOpt)
x=get.variables(tranOpt)

#lhs of constraints
get.constraints(tranOpt)

#sensitivity:
d=get.sensitivity.rhs(tranOpt)
c=get.sensitivity.obj(tranOpt)