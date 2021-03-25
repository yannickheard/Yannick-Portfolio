library("lpSolve")

#This script lists all the commands to find the solution for the short-term
#financing problems from the lecture

#The vatiable X stands to represent the decisiob vairables in the
#formualtion in this order:
  #X = [x1 x2 x3 x4 x5 y1 y2 y3 z1 z2 z3 z4 z5 z6]'

#we will use lp() to solve this

#Objective is to min -z6
c=c(rep(0,13),1)

#equality constraints
b=c(150,100,-200,200,-50,-300,100,100,100,100,100)
dir=c("=","=","=","=","=","=","<=","<=","<=","<=","<=")

A=matrix(0,11,14)

#x1+y1-z1:
A[1,1]=1;A[1,6]=1;A[1,9]=-1

#x2 +y2 -1.01x1 +1.003z1 - z2:
A[2,2]=1;A[2,7]=1;A[2,1]=-1.01;A[2,9]=1.003;A[2,10]=-1

#x3 +y3 -1.01x2 +1.003z2 - z3:
A[3,3]=1;A[3,8]=1;A[3,2]=-1.01;A[3,10]=1.003;A[3,11]=-1

#x4 - 1.02y1 -1.01x3 +1.003z3 - z4:
A[4,4]=1;A[4,6]=-1.02;A[4,3]=-1.01;A[4,11]=1.003;A[4,12]=-1

#x5 - 1.02y2 -1.01x4 +1.003z4 - z5:
A[5,5]=1;A[5,7]=-1.02;A[5,4]=-1.01;A[5,12]=1.003;A[5,13]=-1

#   - 1.02y3 -1.01x5 +1.003z5 - z6:
A[6,8]=-1.02;A[6,5]=-1.01;A[6,13]=1.003;A[6,14]=-1

A[7,1]=1
A[8,2]=1
A[9,3]=1
A[10,4]=1
A[11,5]=1

s = lp ("max",c,A,dir,b,compute.sens=1)