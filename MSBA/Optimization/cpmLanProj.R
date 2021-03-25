library(lpSolveAPI)

#define the cost matrix
l=matrix(0,16,16)
l[1,2]=10
l[2,3]=l[2,4]=6
l[3,5]=l[3,7]=6
l[4,6]=l[4,7]=12
l[5,6]=4
l[6,8]=l[6,9]=3
l[7,10]=6
l[8,12]=12
l[9,11]=3
l[10,13]=14
l[11,12]=4
l[12,14]=l[12,13]=3
l[13,15]=8
l[14,15]=12
l[15,16]=4

lanProj<-make.lp(0,16*16)

#set objective coefficients
set.objfn(lanProj, as.vector(t(l)))

#set objective direction
lp.control(lanProj,sense='max')

nodes=c(1:16)

rhs=c(1,rep(0,14),-1)

for (n in 1:16){

  # for each node, we are assebling "Outputs - Inputs = 0" (or +1 for start node and -1 for end node)
  #    on the LHS it is easier to sum over all nodes with only the connected nodes having non-zero coefficients
  #    so we do an is.finite(l_ij/l_ij)... this will return a 1 if i and j are connected and a 0 if not.
  
  coef=c(l[n,1:16]/l[n,1:16],-l[1:16,n]/l[1:16,n])
  ind=c((n-1)*16+c(1:16),(c(1:16)-1)*16+n)
  nz=is.finite(coef)
  add.constraint(lanProj,coef[nz], "=",rhs[n],ind[nz])               
}

#name the rows and columns and write the lp to a text file
ColNames = c()
RowNames = c()
for(i in 1:16){
  for(j in 1:16){
    ColNames = cbind(ColNames,paste("x",i,",",j, sep=""))
  }
  RowNames=cbind(RowNames,paste("node",i))
}
dimnames(lanProj) <- list(RowNames, ColNames)
set.type(lanProj, c(1:256), "binary")
write.lp(lanProj,'lanProj.lp',type='lp')


#solve the model, if this return 0 an optimal solution is found
status=solve(lanProj)

#this return the proposed solution
z=get.objective(lanProj)
x=get.variables(lanProj)



