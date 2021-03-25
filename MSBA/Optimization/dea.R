library(lpSolveAPI)

#define the datasets
inp=matrix(c(5,8,7,14,15,12),3,2)
op=matrix(c(9,5,4,4,7,9,16,10,13),3,3)

#hospital we are testing fror efficiency
hosp=2

#create an LP model with 10 constraints and 12 decision variables
DEA<-make.lp(0,5)

#set objective coefficients
set.objfn(DEA, c(0,0,c(op[hosp,1],op[hosp,2],op[hosp,3])))

#set objective direction
lp.control(DEA,sense='max')

for (h in 1:3){
  add.constraint(DEA,c(inp[h,1],inp[h,2],-op[h,1],-op[h,2],-op[h,3]), ">=",0)
}
add.constraint(DEA,c(inp[hosp,1],inp[hosp,2]), "=",1,c(1,2))

#name cols and write to text file
ColNames <- c("x1","x2","y1","y2","y3")
dimnames(DEA)[2] <- list(ColNames)
write.lp(DEA,'dea.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(DEA)

#this return the proposed solution
z=get.objective(DEA)
x=get.variables(DEA)


#sensitivity:
d=get.sensitivity.rhs(DEA)
c=get.sensitivity.obj(DEA)


