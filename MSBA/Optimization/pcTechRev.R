library(lpSolveAPI)


#create an LP model with 0 constraints (we will add constraints later) and 2 decision variables
pcLP<-make.lp(0,2)

#set objective coefficients
set.objfn(pcLP, c(80,129))

#set objective direction
lp.control(pcLP,sense='max')

add.constraint(pcLP,c(5,6), "<=",10000)
add.constraint(pcLP,c(1,2), "<=",3000)
add.constraint(pcLP,c(1,0), "<=",600)
add.constraint(pcLP,c(0,1), "<=",1200)

varNames <- c("basic","xp")
constraintNames <- c("Assembly","Testing","Max basic","Max XP")
dimnames(pcLP) <- list(constraintNames, varNames)

#write to text file
write.lp(pcLP,'pc.lp',type='lp')


#solve the model, if this return 0 an optimal solution is found
solve(pcLP)

#this return the proposed solution
get.objective(pcLP)
x=get.variables(pcLP)

#lhs of constraints
get.constraints(pcLP)

#sensitivity:
d=get.sensitivity.rhs(pcLP)
c=get.sensitivity.obj(pcLP)