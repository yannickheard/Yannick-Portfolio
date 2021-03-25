


#The [10 10] is simply a guess and can be any number.
#
# Soution: x    = [9.31          5.03]
#          fval =  5456.54
#


annualDistance <- function(X){ 
  
  Xcor=c(5,10,0,12);
  Ycor=c(10,5,12,0);
  shipments=c(200,150,200,300);
  
  dist=sqrt((X[1]-Xcor)^2+(X[2]-Ycor)^2);
  
  totalDist=dist %*% shipments;
  return(totalDist)
  

}
S=optim(c(10,10),annualDistance,method="CG")