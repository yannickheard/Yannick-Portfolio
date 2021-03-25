library('lpSolve')

#variable arrangement
# X = (x1,x2,x3,x4, y1,...,y4, z1,...z4)


#objective coefficients
c<-rep(c(2000,2500,5000,3500),3)

#LHS of constraints
#
# define a 10x12 matrix of zeros
A=matrix(0,10,12)

#weight constraints
A[1,1:4]<-1
A[2,5:8]<-1
A[3,9:12]<-1
#volume constraints
A[4,1:4]<-c(400,300,200,500)
A[5,5:8]<-c(400,300,200,500)
A[6,9:12]<-c(400,300,200,500)
#availability constraints
A[7:10,1:4]<-diag(4)
A[7:10,5:8]<-diag(4)
A[7:10,9:12]<-diag(4)


#RHS of constraints
b<-c(10,8,12,5000,4000,8000,18,10,5,20)

#All constraints have a <=
dir<-rep("<=",10)

#solve the LP and assign the returned strcture to variable s
s=lp("max",c,A,dir,b,compute.sens = 1)


#status
cat("Status:",s$status,"\n")

#solution
cat("Solution:","\n")
print(t(matrix(s$solution,4,3)))

#profit
cat("Profit:",s$objval,"\n")

