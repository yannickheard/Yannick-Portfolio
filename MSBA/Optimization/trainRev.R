library(lpSolveAPI)

#define the datasets

train<-data.frame(wagon=c('w1','w2','w3'), weightcapacity=c(10,8,12), spacecapacity=c(5000,4000,8000))
cargo<-data.frame(type=c('c1','c2','c3','c4'), available=c(18,10,5,20), volume=c(400,300,200,500),profit=c(2000,2500,5000,3500))

#create an LP model with 0 constraints (we will add constraints later) and 12 decision variables
trainLP<-make.lp(0,12)

#set objective coefficients
set.objfn(trainLP, rep(cargo$profit,nrow(train)))

#set objective direction
lp.control(trainLP,sense='max')

add.constraint(trainLP,rep(1,4), "<=",train$weightcapacity[1],indices=c(1:4))
add.constraint(trainLP,rep(1,4), "<=",train$weightcapacity[2],indices=c(5:8))
add.constraint(trainLP,rep(1,4), "<=",train$weightcapacity[3],indices=c(9:12))

add.constraint(trainLP,cargo$volume, "<=",train$spacecapacity[1],indices=c(1:4))
add.constraint(trainLP,cargo$volume, "<=",train$spacecapacity[2],indices=c(5:8))
add.constraint(trainLP,cargo$volume, "<=",train$spacecapacity[3],indices=c(9:12))

add.constraint(trainLP,rep(1,3), "<=",cargo$available[1],indices=c(1,5,9))
add.constraint(trainLP,rep(1,3), "<=",cargo$available[2],indices=c(1,5,9)+1)
add.constraint(trainLP,rep(1,3), "<=",cargo$available[3],indices=c(1,5,9)+2)
add.constraint(trainLP,rep(1,3), "<=",cargo$available[4],indices=c(1,5,9)+3)

constraintNames <- c("Weight x", "Weight y", "Weight z", "Volume x", "Volume y", "Volume z", "Availability 1", "Availability 2", "Availability 3", "Availability 4")
varNames <- c("x1","x2","x3","x4","y1","y2","y3","y4","z1","z2","z3","z4")
dimnames(trainLP) <- list(constraintNames, varNames)

#write to text file
write.lp(trainLP,'train.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(trainLP)

#this return the proposed solution
get.objective(trainLP)
x=get.variables(trainLP)

#lhs of constraints
get.constraints(trainLP)

#sensitivity:
d=get.sensitivity.rhs(trainLP)
c=get.sensitivity.obj(trainLP)


