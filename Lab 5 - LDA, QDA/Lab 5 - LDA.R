####LINEAR DISCRIMINANT ANALYSIS####

#Consists of percentage returns for the S&P 500 stock index over 1,250 days.
#Lag 1 through Lag 5: percentage returns for 5 previous trading days.
#Volume: The number of shares traded on the previous day.
#Today: The percentage return on the date in question.
#Direction: Whether the market was Up or Down on this date.

library(ISLR) #lda() function
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
lda_model=lda(Direction~Lag1+Lag2,data=Smarket,subset=train) 
lda_model

lda.pred=predict(lda_model, Smarket.2005) 
names(lda.pred)
#Class:contains LDA's predictions about movement of market
#Posterior:It's kth column contains probabilities that the corresponding
#observation belongs to the kth class
#x:contains the linear discriminants

lda.class=lda.pred$class  
table(lda.class,Direction.2005) 
mean(lda.class==Direction.2005) #56% accuracy 

#plot(Smarket.2005[,2:3],main="LDA decision boundary",xlab="Lag1",ylab="Lag2")
#points(Smarket.2005[,2:3],col=ifelse(Smarket.2005[,9]=='Down','green4','red3'))
#contour(S)


