####QUESTION 1####

#Using the Boston data set, fit a LDA and QDA model
#in order to predict whether a given suburb has 
#a crime rate above or below the median. Describe your findings.

library(MASS)
library(ISLR)
data(Boston)
attach(Boston)

median(crim) 
crim01=rep(0,length(crim))
crim01[crim>median(crim)]=1
Boston=data.frame(Boston,crim01)
Boston$crim=NULL

#Splitting the data 
set.seed(26)
train_indices = sample(1:nrow(Boston),0.75*nrow(Boston))
train=Boston[train_indices, ]
test=Boston[-train_indices,]
crim01_actual=test$crim01
test$crim01=NULL

#LDA
lda_model=lda(crim01~indus+nox+age+dis+rad+tax,data=train) 
lda_model

lda.pred=predict(lda_model,test) 
names(lda.pred) 

lda.class=lda.pred$class 
table(lda.class,crim01_actual) 
mean(lda.class==crim01_actual)#85 % accuracy 

#QDA
qda_model=qda(crim01~indus+nox+age+dis+rad+tax,data=train) 
qda_model

qda.pred=predict(qda_model,test) 
names(qda.pred) 

qda.class=qda.pred$class 
table(qda.class,crim01_actual) 
mean(qda.class==crim01_actual)#93% accuracy 
