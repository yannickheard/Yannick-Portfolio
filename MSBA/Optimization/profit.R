#The 100 is simply a guess and can be any number.
#
# Soution: x=93.33 and fval=-9343.95
#
# you can plot this function using
#
# price=seq(80,120,.1);
# plot(price,-func(price));
#


func <- function(price){ 
  demand=(3777178*price^(-2.154))
  profit=demand*(price-50)
  return(-profit)
}
S=optim(100,func,method="CG")