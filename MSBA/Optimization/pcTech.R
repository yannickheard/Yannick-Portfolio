library('lpSolve')


#objective coefficients
c<-c(80,129)

#LHS of constraints
#
# define a 4x2 matrixn of zeros
A<-matrix(0,4,2)

# assembly constraint
A[1,]<-c(5,6)
# testing constraint
A[2,]<-c(1,2)
#Max constraints
A[3,]<-c(1,0)
A[4,]<-c(0,1)

#RHS of constraints
b<-c(10000,3000,600,1200)

#All constraints have a <=
dir<-rep("<=",4)

#solve the LP and assign the returned strcture to variable s
s=lp("max",c,A,dir,b,compute.sens = 1)


#status
cat("Status:",s$status,"\n")

#solution
cat("Solution:",s$solution,"\n")

#profit
cat("Profit:",s$objval,"\n")
