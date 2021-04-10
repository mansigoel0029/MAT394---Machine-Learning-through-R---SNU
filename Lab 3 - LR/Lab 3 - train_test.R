dataset = read.csv("cereal.csv")
attach(dataset)
dataset$name=NULL
dataset$mfr=NULL
dataset$type=NULL

# Split the data into training and test set
set.seed(26)
train_indices = sample(1:nrow(dataset),0.75*nrow(dataset))
train = dataset[train_indices, ]
test = dataset[-train_indices,]
y_actual = test$rating
test$rating = NULL
attach(train)

#Model Building
fit1 = lm(rating~calories+protein+fat+carbo+sugars,data=train)
summary(fit1)
fit2 = lm(rating~calories+protein+fat,data=train)
summary(fit2)
fit3 = lm(rating~calories+protein,data=train)
summary(fit3)

# Make predictions on the test set
pred1 = predict(fit1,test)
pred2 = predict(fit2,test)
pred3 = predict(fit3,test)

#Calculating R2 to judge perform of the model
library(caret)
r2_1 = R2(pred1,y_actual)
r2_2 = R2(pred2,y_actual)
r2_3 = R2(pred3,y_actual)


#An R2 statistic that is close to 1 indicates that a large proportion
#of the variability in the response has been explained by the regression. 
#A number near 0 indicates that the regression 
#did not explain much of the variability in the response


