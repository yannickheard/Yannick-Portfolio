library(lpSolveAPI)


cVec=c(M,132,217,164,58,132,M,290,201,79,217,290,M,113,303,164,201,113,M,196,58,79,303,196,M)
cMat=matrix(cVec,5,5)

# x11, x12, x21, x22, z1, z2, z3, z4, y1, y2, y3

#index of xij=> (j-1)*5+i

EuingLP<-make.lp(0,11)

ColNames <- c("x11","x12","x21","x22","z1","z2","z3","z4","y1","y2","y3")
dimnames(EuingLP)[2] <- list(ColNames)


#set objective coefficients
set.objfn(EuingLP, c(12,14,12,14,-0,-12500,-22500,-30000,0,0,0))

#set objective direction
lp.control(EuingLP,sense='max')

add.constraint(EuingLP,c(0.5,-0.5),">=",0,c(1,3))
add.constraint(EuingLP,c(0.4,-0.6),">=",0,c(2,4))

add.constraint(EuingLP,c(1,1,-0,-500,-1000,-1500),"<=",500,c(1,2,5,6,7,8))
add.constraint(EuingLP,c(1,1),"<=",1000,c(3,4))


add.constraint(EuingLP,c(1,-1),"<=",0,c(5,9))
add.constraint(EuingLP,c(1,-1,-1),"<=",0,c(6,9,10))
add.constraint(EuingLP,c(1,-1,-1),"<=",0,c(7,10,11))
add.constraint(EuingLP,c(1,-1),"<=",0,c(8,11))

add.constraint(EuingLP,c(1,1,1),"=",1,c(9,10,11))
add.constraint(EuingLP,c(1,1,1,1),"=",1,c(5:8))

set.type(EuingLP, c(9:11), "binary")

write.lp(EuingLP,'EuingLP.lp',type='lp')

status=solve(EuingLP)
get.objective(EuingLP)
x=get.variables(EuingLP)











