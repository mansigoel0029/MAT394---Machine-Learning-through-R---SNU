library(ISLR)
data(Hitters)
attach(Hitters)

#Checking for NULL values and removing them 
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)

####BEST SUBSET SELECTION####

#The regsubsets() function performs best subset selection
library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full) #Outputs the best set of variables for each model size.(RSS)
#Asterisk indicates that variable is included in the corresponding model.

#nvmax can be used to return as many variables as are desired. 
regfit.full=regsubsets(Salary~.,data=Hitters ,nvmax=19) 
reg.summary=summary(regfit.full)

names(reg.summary)#The summary() function also returns R2, RSS,BIC. 
#We can examine these to try to select the best overall model.

reg.summary$rsq
reg.summary$bic

#Plotting BIC
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC", type="l")
which.min(reg.summary$bic) ##gives the x axis value corresponding to the max BIC value
points(6,reg.summary$bic[6],col="red") #puts a point on the graph for that value

plot(regfit.full,scale="bic") #used to display the selected variables for the best model with a given number of predictors 
#ranked according to the BIC.

#Lowest BIC -150 approx
# Best Model : 6 variables-AtBat, Hits, Walks, CRBI, DivisionW, and PutOuts.

#Coefficient estimates associated with this model
coef(regfit.full,6)

####FORWARD STEPWISE SELECTION####
regfit.fwd=regsubsets(Salary~.,data=Hitters ,nvmax=19, method ="forward")
summary(regfit.fwd)

#To get model with lowest BIC
plot(regfit.fwd,scale="bic")
coef(regfit.fwd,6)



