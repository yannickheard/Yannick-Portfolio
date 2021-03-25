#no. of simulations
N=1000

#Door label list - you can even use: doors = c('A','B','C')
doors = c(1:3)

stay = rep(NA,N)
swit = rep(NA,N)

for (i in 1:N){
  
  #Generate Random Variables
  car = doors[ceiling(runif(1)*length(doors))]
  pick = doors[ceiling(runif(1)*length(doors))]
  
  #Run the Simulation - that is, play the game
  hostChoices  = setdiff(doors,union(pick,car))
  host = hostChoices[ceiling(runif(1)*length(hostChoices))]
  
  switchedDoor = setdiff(doors,union(pick,host))
  
  #remember output - that is, win or not for each strategy
  stay[i] = (pick==car) 
  swit[i] = (switchedDoor==car)
  
}

#summarize output
print(paste("Prob. of winning if we do not switch doors:", mean(stay)))
print(paste("Prob. of winning if we switch doors       :", mean(swit)))