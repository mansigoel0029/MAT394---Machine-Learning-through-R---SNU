####SIMPLE LINEAR REGRESSION####

library(ISLR)
library(MASS)

data=read.csv("Advertising.csv", header=TRUE)
attach(data)
data$X=NULL

model1=lm(Sales~TV, data=data)
plot(TV, Sales)
abline(model1, col="red") #y=b_0+b_1x
summary(model1)  #Sales=7.032594+0.047537(TV), means an additional $1,000 spent 
#on TV advertising is associated with selling 47.5 more units of the product

model2=lm(Sales~Radio, data=data)
plot(Radio, Sales)
abline(model2, col="red")
summary(model2) #Sales=9.31164+0.20250(Radio)

model3=lm(Sales~Newspaper, data=data)
plot(Newspaper, Sales)
abline(model3, col="red")
summary(model3)  #Sales=12.35141+0.05469(Newspaper)

#AIC is a method for selecting a model. We select the model with a smaller AIC.
AIC(model1)
AIC(model2)
AIC(model3)

####MULTIPLE LINEAR REGRESSION####

model4=lm(Sales~TV+Radio+Newspaper, data=data)
#model4=lm(Sales~.,data=data)
summary(model4) #Sales=2.93+0.045(TV)+0.189(Radio)-0.001(Newspaper)
AIC(model4)
#Interpretation: for a given amount of TV and newspaper advertising, 
#spending an additional $1,000 on radio advertising leads to an increase 
#in sales by approximately 189 units

model5=lm(Sales~TV+Radio, data=data)
summary(model5) #Sales=2.92110+0.04575(TV)+0.18799(Radio)
AIC(model5) 



