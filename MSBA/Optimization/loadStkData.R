#the command below simply sets the working dir to the folder in which this code is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#then load the csv file from the same folder
stk=read.csv("ibm.csv")

#calc returns and discard first row
stk$priceChange=c(diff(stk$Adj.Close),0)
stk$return=stk$priceChange/stk$Adj.Close

#remove last row
stk=stk[-nrow(stk),]

#convert daily to yearly
mu=mean(stk$return)*250
sigma=sd(stk$return)*sqrt(250)