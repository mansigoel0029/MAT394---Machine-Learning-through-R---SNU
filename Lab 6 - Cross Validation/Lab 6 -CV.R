# Leave-One-Out Cross-Validation
library(ISLR)
data("Auto")
attach(Auto)
Auto=na.omit(Auto)

# GLM and LM can be used to perform linear regression
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
#Confirming that answers are the same. 
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

#We need to use GLM function so that cv.glm() can be used.
#library boot is required for cv.glm()

library(boot)
model1=glm(mpg~horsepower,data=Auto)
cv.err1=cv.glm(Auto, model1)
cv.err1$delta

model=glm(mpg~poly(horsepower,2),data=Auto)
cv.err=cv.glm(Auto,model)

#LOOCV to select the best model
cv.err2=rep(0,5)
for (i in 1:5) {
  model2=glm(mpg~poly(horsepower,i),data=Auto)
  cv.err2[i]=cv.glm(Auto,model2)$delta[1]
}
cv.err2

# K fold cross validation to select the best model
cv.err3=rep(0,10)
for (i in 1:10) {
  model3=glm(mpg~poly(horsepower,i),data=Auto)
  cv.err3[i]=cv.glm(Auto, model3, K=10)$delta[1]
}
cv.err3
