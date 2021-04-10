####LOGISTIC REGRESSION####

#DATASET DESCRIPTION:
#Consists of percentage returns for the S&P 500 stock index over 1,250 days.
#Lag 1 through Lag 5: percentage returns for 5 previous trading days.
#Volume: The number of shares traded on the previous day.
#Today: The percentage return on the date in question.
#Direction: Whether the market was Up or Down on this date.

library(ISLR)
data("Smarket")
attach(Smarket)
names(Smarket)

#Dsiplays the dummy variables creates for a categorical variable 
contrasts(Direction)

#Fitting the model using the glm() function 
model=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)
summary(model)

#The smallest p-value here is associated with Lag1(0.145). 
#The negative coefficient for this predictor suggests that 
#if the market had a positive return yesterday, 
#then it is less likely to go up today. 
#However,p-value is still large and so there is 
#no evidence of a real association between Lag1 and Direction.

probs=predict(model,type ="response") #The type="response" option tells R to 
                                      #output probabilities of the form P(Y = 1|X)

probs[1:10]#This gives the first 10 values 

label_pred=rep("Down " ,1250) #Creates a vector of 1,250 Down elements 
label_pred[probs>0.5]="Up" #Transforms to Up all elements for which prob>0.5

c=table(Direction, label_pred) #Creates confusion matrix

accuracy=(c[1,1]+c[2,2])/1250 #Accuracy of the model
print(accuracy) #Model correctly predicted the movement of the market 
                #52.2 % of the time.

#Looking at the error, we get 100-52.2=47.8% is the training error
#which is still very high. In order to better assess the accuracy of the model 
#we can fit the model using part of the data, 
#and then examine how well it predicts the held out data. 
#This will yield a more realistic error rate.

####TRAIN-TEST SPLIT####

train=(Year<2005) #Training set 
Smarket.2005=Smarket[!train ,] #Testing set 
dim(Smarket.2005) #Cardinality of the testing set
Direction.2005=Direction[!train]

#Building the model
model1=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
            data=Smarket,family=binomial,subset=train)

#Predicting probs of the market going up for points in our test set
probs_test=predict(model1,Smarket.2005,type="response")

#Making predictions for the test set and comparing them with actual values 
pred_test=rep("Down",252)
pred_test[probs_test>.5]="Up"
table(pred_test,Direction.2005)

mean(pred_test==Direction.2005)
mean(pred_test!=Direction.2005)

#Using statistically significant variables
model2=glm(Direction~Lag1+Lag2,
           data=Smarket,family=binomial,subset=train )

probs_test=predict(model2,Smarket.2005, type="response")

pred_test=rep("Down",252)
pred_test[probs_test >.5]="Up"
table(pred_test,Direction.2005)

mean(pred_test==Direction.2005) #55%
mean(pred_test!=Direction.2005) #44%

#To predict direction of market for specific values for Lag1,Lag2
predict(model2,newdata=data.frame(Lag1=1.5,Lag2=1.1),type="response")


