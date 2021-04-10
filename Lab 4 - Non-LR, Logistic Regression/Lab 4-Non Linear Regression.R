library(MASS)
data(Boston)
attach(Boston)

#SLR
fit1=lm(medv~lstat,data=Boston)
summary(fit1)
coef(fit1)

predict(fit1,data.frame(lstat=c(5,10,15)), 
        interval ="confidence")
predict(fit1,data.frame(lstat=c(5,10,15)), 
        interval ="prediction")

plot(lstat ,medv) 
abline(fit1,col='red')

#MLR
fit2=lm(medv~lstat+age,data=Boston) 
summary(fit2)

fit3=lm(medv~.,data=Boston) 
summary(fit3)

#Update function
lm.fit1=lm(medv~.,data=Boston)
summary(update(lm.fit1, .~.-age))
summary(update(lm.fit1, .~.+log(lstat)))
summary(update(lm.fit1, .~.+sqrt(lstat)))

#Interaction terms

#lstat:age includes an interaction term between lstat and age(lstat x age)
#lstat*age is a shorthand for lstat+age+lstat:age 
fit4=lm(medv~lstat*age,data=Boston)
summary(fit4)

#Polynomial Regression 
fit5=lm(medv~lstat+I(lstat^2),data=Boston)
summary(fit5)

#Comparing SLR and polynomial model
anova(fit1,fit5) #performs a hypothesis test with null hypothesis as
#the two models fit the data equally well, 
#and the alternative hypothesis is that the 2nd model is superior. 

summary(fit1)$r.sq
summary(fit5)$r.sq

#The poly() function can also be used to fit higher degree polynomials
fit6=lm(medv~poly(lstat,5),data=Boston) 
summary(fit6)


