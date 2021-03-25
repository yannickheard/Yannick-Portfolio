library(lpSolve)

#Production costs each month
prodCosts=c(12.50,12.55,12.70,12.80,12.85,12.95) 

#demand each month
demand=c(10000,15000,30000,35000,25000,10000)


# 18 variables: X = (x1....x6,y1...y6, I1... I6)


#objective
obj = c(prodCosts,rep(50000,6),prodCosts*0.05)


#A matrix has 24 constarints and 18 variables
A = matrix(0,24,18);

#x1-I1=D1-5000
A[1,1]=1;A[1,13]=-1;  

#Ii-1+xi-Ii=Di
for(i in 2:6){
  A[i,i-1+12]=1;A[i,i]=1;A[i,i+12]=-1;  
}

for(i in 1:6){
  #Ii<=20000
  A[i+6,i+12]=1;  
  #xi-yiM<=0
  A[i+12,i]=1;A[i+12,i+6]=-30000;
  #yi-xi<=0
  A[i+18,i+6]=1;A[i+18,i]=-1;
}

b = c(demand[1]-5000,demand[2:6],rep(20000,6),rep(0,12));
dir = c(rep("=",6),rep("<=",18))

S = lp("min",obj,A,dir,b,binary.vec=7:12)