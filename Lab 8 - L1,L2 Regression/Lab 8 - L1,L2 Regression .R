library(ISLR)
data("Hitters")
attach(Hitters)

sum(is.na(Hitters))
Hitters=na.omit(Hitters)

#Using glmnet() to predict Salary in Hitters.
#Pass x as a matrix and y as a vector. 

x=model.matrix(Salary~.,Hitters)[,-1] #creates matrix,transforms qualitative variables into dummy variables.
y=Hitters$Salary

library(glmnet)
grid=10^seq(10,-2,length=100) #100 lambdas between 10^10 and 10^-2
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) #alpha=0 for ridge regression 

#using coef() to view vector of ridge regression coefficients associated with each lambda
dim(coef(ridge.mod)) #20X100 - 20 rows for each predictor, 100 columns for each lambda.

ridge.mod$lambda[50] #lambda = 11498
coef(ridge.mod)[,50] # coeff estimates corresponding to lambda = 11498 
sqrt(sum(coef(ridge.mod)[-1,50]^2)) 

ridge.mod$lambda[60] #lambda = 705
coef(ridge.mod)[,60] # coeff estimates corresponding to lambda = 705
sqrt(sum(coef(ridge.mod)[-1,60]^2)) 

#Larger lambda, smaller coeff estimates, smaller l2 norm. 

#predict() used to obtain the ridge regression coefs for a new value of lambda.
predict(ridge.mod,s=50,type="coefficients")[1:20,] 

####RIDGE REGRESSION####
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)

#Lambda = 4
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2) #MSE = 142199 

#Lambda = 10^10
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2) #MSE = 224669.8

#Performing ridge regression with a very large lambda corresponds to 
#fitting a model with just the intercept term 
#i.e each test observation is predicted using the mean of the training points.

mean((mean(y[train])-y.test)^2) #MSE = 224669.9

#Lambda = 0 i.e OLS 
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2) #MSE = 167789.8
predict(ridge.mod,s=0,type="coefficients")

lm(y~x,subset=train)

#Using CV to select lambda

#using the built-in CV function, cv.glmnet(), default is 10 fold CV
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam #326

ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2) #MSE = 139856.6


#Refit ridge regression model on the full data set, 
#using the value of lambda chosen by CV, and examine the coefficient estimates.

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)

####LASSO REGRESSION####
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)  

#CV and test MSE
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam #9
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mean((lasso.pred-y.test)^2) #MSE = 143673.6

#Refit lasso regression model on the full data set, 
#using the value of lambda chosen by CV, and examine the coefficient estimates.

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam) 
lasso.coef

#8 of the 19 coefficient estimates are 0. 
#Lasso model with lambda chosen by CV contains only 11 variables.








