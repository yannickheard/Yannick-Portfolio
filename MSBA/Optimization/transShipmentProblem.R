library(lpSolveAPI)


#Var Arrangement (ab stands for a->b: eg: mn stands from Memphis to New York)
#ind:           1     2     3     4     5     6     7     8     9     10    11    12     13   14 
ColNames <- c("Xmn","Xmc","Xml","Xmb","Xdn","Xdc","Xdl","Xdb","Xnc","Xnl","Xnb","Xcn","Xcl","Xcb")

tShipOpt<-make.lp(0,14)

#set objective coefficients
set.objfn(tShipOpt, c(8,13,25,28,15,12,26,25,6,16,17,6,14,16))

#set objective direction
lp.control(tShipOpt,sense='min')

add.constraint(tShipOpt,rep(1,4), "<=",150,c(1:4))
add.constraint(tShipOpt,rep(1,4), "<=",200,c(5:8))

add.constraint(tShipOpt,c(1,1,1,-1,-1,-1), "=",0,c(1,5,12,9,10,11))
add.constraint(tShipOpt,c(1,1,1,-1,-1,-1), "=",0,c(2,6,9,12,13,14))

add.constraint(tShipOpt,rep(1,4), ">=",130,c(3,7,10,13))
add.constraint(tShipOpt,rep(1,4), ">=",130,c(4,8,11,14))

RowNames <- c("m","d","n","c","l","b")
dimnames(tShipOpt) <- list(RowNames, ColNames)

#write to text file
write.lp(tShipOpt,'tShipOpt.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(tShipOpt)

#this return the proposed solution
get.objective(tShipOpt)
x=get.variables(tShipOpt)

#lhs of constraints
get.constraints(tShipOpt)

#sensitivity:
d=get.sensitivity.rhs(tShipOpt)
c=get.sensitivity.obj(tShipOpt)