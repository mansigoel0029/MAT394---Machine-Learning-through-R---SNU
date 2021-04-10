####BOOTSTRAP - Estimating the Accuracy of a Statistic of Interest####
#Steps:
#1. Create a function that computes the statistic of interest. 
#2. Use the boot() function to perform the bootstrap 
#by repeatedly sampling observations from the data set with replacement.

library(ISLR)
data("Portfolio")
attach(Portfolio)
library(boot)
#Portfolio is a simlumated dataset containing 100 returns for 2 assets X and Y
#The data will be used to estimate the optimal fraction(alpha) to invest 
#in each asset to minimize risk of the combined portfolio. 
#Then we will use bootstrap to estimate the standard error of this estimate

####STEP 1####

#Function which takes input (X,Y) data and which observtions to estimate alpha. 
alpha.fn=function(data,index){
  X = data$X[index]   # data$X[1:100]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))}
#Returns estimate of alpha based on selected observations. 

alpha.fn(Portfolio,1:100)

#Randomly select 100 observations from the range 1 to 100, with replacement
set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=T))

#The boot() function automates the step in line 28 R number of times 
#to produce estimates of alpha 
#and then computes the standard deviation. 
boot(Portfolio,alpha.fn,R=1000)

#αˆ=0.5758 , SE(αˆ)=0.09366


####BOOTSRAP - Estimating the Accuracy of a Linear Regression Model####

library(ISLR)
data(Auto)
attach(Auto)

#Create bootstrap estimates for intercept and slope terms 
#by randomly sampling from among the observations with replacement. 
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))

boot.fn(Auto,1:392) 

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))

#Compute the standard errors of 1000 bootstrap estimates for 
#the intercept and slope terms.
boot(Auto ,boot.fn ,1000)
#bootstrap estimate for SE(β_0) = 0.84
#bootstrap estimate for SE(β_1) is 0.0074

#Comparing these results with the summary() function output
summary(lm(mpg~horsepower,data=Auto)) # intercept 0.717499, slope 0.006446

#The results are a little different because the actual trend in the data
# is NOT linear. It is quadratic. 

#Performing bootstrap for a quadratic model
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,
                    subset=index))

set.seed(1)
boot(Auto,boot.fn,1000) #2.03,0.0324,0.00011

#Comparing with the summary() function output
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto)) #1.80,0.0311,0.00012




