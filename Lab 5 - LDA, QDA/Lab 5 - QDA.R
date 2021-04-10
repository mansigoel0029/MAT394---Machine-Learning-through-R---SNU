####QUADRATIC DISCRIMINANT ANALYSIS####

#Consists of percentage returns for the S&P 500 stock index over 1,250 days.
#Lag 1 through Lag 5: percentage returns for 5 previous trading days.
#Volume: The number of shares traded on the previous day.
#Today: The percentage return on the date in question.
#Direction: Whether the market was Up or Down on this date.

library(ISLR) #qda() function
library(MASS) #for the dataset

data(Smarket)
attach(Smarket) 
names(Smarket) 

#Displays the dummy variables creates for a categorical variable 
contrasts(Direction) 

#Splitting the data
train=(Year<2005) #Training set 
Smarket.2005=Smarket[!train,] #Testing set 
Direction.2005=Direction[!train] #Extracting test observations

#Fitting LDA model
qda_model=qda(Direction~Lag1+Lag2,data=Smarket,subset=train) 
summary(qda_model) 

qda.pred=predict(qda_model, Smarket.2005) 
names(qda.pred)
#Class:contains LDA's predictions about movement of market
#Posterior:It's kth column contains probabilities that the corresponding
#observation belongs to the kth class
#x:contains the linear discriminants

qda.class=qda.pred$class  
table(qda.class,Direction.2005) 
mean(qda.class==Direction.2005) #60% accuracy 

