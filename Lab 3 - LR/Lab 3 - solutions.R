####QUESTION 1####

library(ISLR)
library(MASS)
data(Auto)
attach(Auto)
?Auto
#Check for NULL values and remove them
sum(is.na(Auto))
#Auto=na.omit(Auto)

#part (a)
model1 = lm(mpg~horsepower, data=Auto ) 
summary(model1)

#(a)(i)
#mpg=39.935861-0.157845(horsepower)

#(a)(ii)
#The relationship is negative 
#as we can see from the parameter estimate for horsepower(0.157845)
#This means that if a vehicle has higher horsepower,
#it will generally have a lower value for mpg.

#(a)(iii)
#0.6059 means that horsepower explains 60.59% of the variance in mpg.

#part (b)
plot(mpg ~ horsepower, Auto)
abline(model1,col='red')

#part (c)
predict(model1, data.frame(horsepower=c(98)), 
        interval = "prediction",level=0.95)
#A prediction interval is a range that likely contains 
#the value of the dependent variable for a single new observation 
#given specific values of the independent variables

predict(model1, data.frame(horsepower=c(98)), 
        interval = "confidence",level=0.95)
#A confidence interval of the prediction is a range that likely 
#contains the mean value of the dependent variable given specific values 
#of the independent variables.

####QUESTION 2####

#part (a)
Auto$name=NULL

fit = lm(mpg~., data=Auto)
summary(fit)

#part (b)
#weight, year,origin

#part (c)
#A coeff of 0.750773 means that when year increases by 1, the efficiency of a car
#increases by 77%

    