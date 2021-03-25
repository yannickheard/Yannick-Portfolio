setwd("~/Spring 2018/Marketing/Project")
churn <- read.csv('data.csv')
churn$outcome <- as.numeric(churn$churn == "True")
library(MCMCpack)
library(rsq)
churnlog <- glm(outcome ~ account.length + international.plan + voice.mail.plan
                + number.vmail.messages + total.day.minutes + total.day.calls + total.day.charge + 
                  total.eve.minutes + total.eve.calls + total.eve.charge + total.night.minutes +
                  total.night.calls + total.night.charge + total.intl.minutes + total.intl.calls +
                  total.intl.charge + customer.service.calls 
                  , family=binomial(link="logit"), data = churn)

summary(churnlog)
AIC(churnlog)
BIC(churnlog)
logrsq <- rsq(churnlog, type = 'v')
logrsq

logvalues <- round(fitted(churnlog))
mean(logvalues-churn$outcome)



churnprob = glm(outcome ~ account.length + international.plan + voice.mail.plan
                + number.vmail.messages + total.day.minutes + total.day.calls + total.day.charge + 
                  total.eve.minutes + total.eve.calls + total.eve.charge + total.night.minutes +
                  total.night.calls + total.night.charge + total.intl.minutes + total.intl.calls +
                  total.intl.charge + customer.service.calls 
                , family = binomial(link="probit"), data = churn)

summary(churnprob)
AIC(churnprob)
BIC(churnprob)
probrsq <- rsq(churnprob, type = 'v')
probrsq


probvalues <- round(fitted(churnprob))
mean(probvalues-churn$outcome)


churnhier =  MCMCregress(outcome~account.length + international.plan + voice.mail.plan, random=~
                number.vmail.messages+ total.day.minutes + total.day.calls + total.day.charge + 
                total.eve.minutes + total.eve.calls + total.eve.charge + total.night.minutes +
                total.night.calls + total.night.charge + total.intl.minutes + total.intl.calls +
                total.intl.charge + customer.service.calls , group = 'state', 
              mcmc=6000, data=churn)

summary(churnhier)
linvalues <- round(fitted(churnhier))
mean(linvalues-churn$outcome)


churnlin = lm(outcome ~ account.length + international.plan + voice.mail.plan
                + number.vmail.messages + total.day.minutes + total.day.calls + total.day.charge + 
                  total.eve.minutes + total.eve.calls + total.eve.charge + total.night.minutes +
                  total.night.calls + total.night.charge + total.intl.minutes + total.intl.calls +
                  total.intl.charge + customer.service.calls 
                , data = churn)

linvalues <- round(fitted(churnlin))
mean(linvalues-churn$outcome)

summary(churnlin)
AIC(churnlin)
BIC(churnlin)
summary(churnlin)$r.squared 
