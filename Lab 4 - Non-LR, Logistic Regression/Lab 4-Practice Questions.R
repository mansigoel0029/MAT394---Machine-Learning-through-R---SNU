#Using the Boston data set, fit a model
#in order to predict whether a given suburb has 
#a crime rate above or below the median.Describe your findings.

library(MASS)
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

#Building the model
model1=glm(crim01~.,data=train,family=binomial)

#Making predictions
probs=predict(model1,test,type="response")
pred_test=rep(0,127)
pred_test[probs>0.5]=1
table(pred_test,crim01_actual)

#Accuracy
mean(pred_test==crim01_actual) #0.8661417
mean(pred_test!=crim01_actual)#0.1338583


