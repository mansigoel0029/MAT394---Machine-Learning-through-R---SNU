####QUESTION 1####

library(ISLR)
data(Default)
attach(Default)
set.seed(1)

#part(a)
fit.glm = glm(default~income+balance, data=Default,family="binomial")
summary(fit.glm)
 
#income : 4.985e-06
#balance: 2.274e-04

#part(b)
boot.fn=function(data,index)
  return(coef(glm(default~income+balance,data=data,family="binomial",subset=index)))

#part(c)
library(boot)
boot(Default ,boot.fn ,1000)

#income : 
#balance: 

#part (d)
#income: 4.985e-06 with glm summary,4.866284e-06 using bootstrap
#balance: 2.274e-04 with glm summary,2.298949e-04 using bootstrap

####QUESTION 2####
data(Weekly)
attach(Weekly)
set.seed(1)

#part(a)
fit.glm1 = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary(fit.glm1)

#part(b)
fit.glm2 = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = "binomial")
summary(fit.glm2)

#part(c)
predict.glm(fit.glm2, Weekly[1, ], type = "response") > 0.5 
#Prediction : UP, True Value : DOWN. Incorrectly classified. 

#part(d)
count=rep(0, dim(Weekly)[1])
for (i in 1:(dim(Weekly)[1])) {
  glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = binomial)
  preds = predict.glm(glm.fit, Weekly[i, ], type = "response") > 0.5
  actuals = Weekly[i, ]$Direction == "Up"
  if (preds!=actuals) 
    count[i]=1
}
sum(count)

#part(e)
#error = no.of 1s/total no.of observations
mean(count) #45% error rate 





