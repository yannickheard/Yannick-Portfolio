library(lpSolveAPI)

# Using dea.R we find that hospital 2 is in effient 
# This code looks for an "a" such that "a * hopital1 + (1-a) * hospital3" is always better than hospital 2
# By always, we mean that with lower inputs it can produce better outputs - which are the constraints. 
# Note that we dont need an obective here... we just need a "a" that allows us to demonstrate 2 is inefficient in comparison to others

#define the datasets
inp=matrix(c(5,8,7,14,15,12),3,2)
op=matrix(c(9,5,4,4,7,9,16,10,13),3,3)

#create an LP model with 10 constraints and 12 decision variables

DEAcomb<-make.lp(0,2)

#set objective coefficients
set.objfn(DEAcomb, rep(0,2))

#set objective direction
lp.control(DEAcomb,sense='max')

add.constraint(DEAcomb,c(1,1), "=",1)

add.constraint(DEAcomb,c(inp[1,1],inp[3,1]), "<=",inp[2,1])
add.constraint(DEAcomb,c(inp[1,2],inp[3,2]), "<=",inp[2,2])

add.constraint(DEAcomb,c(op[1,1],op[3,1]), ">=",op[2,1])
add.constraint(DEAcomb,c(op[1,2],op[3,2]), ">=",op[2,2])
add.constraint(DEAcomb,c(op[1,3],op[3,3]), ">=",op[2,3])


ColNames <- c("f1","f2")
dimnames(DEAcomb)[2] <- list(ColNames)

#write to text file
write.lp(DEAcomb,'DEAcomb.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(DEAcomb)

#this return the proposed solution
z=get.objective(DEAcomb)
x=get.variables(DEAcomb)

#sensitivity:
d=get.sensitivity.rhs(DEAcomb)
c=get.sensitivity.obj(DEAcomb)


