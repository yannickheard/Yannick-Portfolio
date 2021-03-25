setwd("~/Spring 2018/Marketing/Assignment 5")
mall <- read.csv("Mall_choice_data.csv")

library(mlogit)

mall1 <- mlogit.data(mall, choice = "choice", 
                     alt.var = "mode",
                     shape = "long")


m1 <- mlogit(choice ~  discount + targeting + distance | income + gender,
             mall1, reflevel="0")

summary(m1)





#Question 2

soda <- read.csv("Soda_choice_data.csv", header=T)
soda$Brand <- as.factor(soda$Brand)
soda.ms = soda[soda$ProductID!=0,]
soda0 = soda$MarketShare[soda$ProductID==0]
soda0 = matrix(soda0, length(soda0), 11)
soda.ms$logMktShrRatio = log(soda.ms$MarketShare/as.vector(t(soda0)))

lmsoda <- lm(ProductID~Brand + Sugar + Caffeine + Promotion, data = soda)

summary(lmsoda)
