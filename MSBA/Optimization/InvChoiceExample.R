library("lpSolve")

# X arranged as [y1.. y5, x1...x5]
obj = c(-25,-35,-28,-20,-40,10,18,11,9,10)

# 14 constraints and 10 Varianbles in A
A=matrix(0,14,10)

#Cost constraint
A[1,1:10]=c(25,35,28,20,40,5,7,6,4,8)

#y1+y5<=1
A[2,1]=1;A[2,5]=1

#y1-y2-y3<=0
A[3,1]=1;A[3,2]=-1;A[3,3]=-1;

#-y3-y4-y5<=-2
A[4,3]=-1;A[4,4]=-1;A[4,5]=-1;

#yi - xi <=0
for(i in 5:9){
  A[i,i-4]=1; A[i,i+1]=-1;
}

#xi - yi*Max_i <=0
A[10,6]=1;A[10,1]=-5;
A[11,7]=1;A[11,2]=-4;
A[12,8]=1;A[12,3]=-5;
A[13,9]=1;A[13,4]=-7;
A[14,10]=1;A[14,5]=-3;

dir = c(rep("<=",14))
b = c(125,1,0,-2,rep(0,10))


S0 = lp("max",obj,A,dir,b,binary.vec=1:5,int.vec=6:10)



# Can you change the budget between 100 and 150 to see how it impacts your NPV?
sVec=rep(0,51)

for(budget in 100:150){
  b = c(budget,1,0,-2,rep(0,10))
  S = lp("max",obj,A,dir,b,binary.vec=1:5,int.vec=6:10)
  sVec[budget-99]=S$objval
}

plot(100:150,sVec)



