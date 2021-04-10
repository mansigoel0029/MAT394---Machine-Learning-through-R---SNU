####10 FOLD CROSS VALIDATION####

dataset = read.csv("cereal.csv")
attach(dataset)
library(caret)

# Define training control as 10-fold cross-validation
set.seed(26) 
train.control = trainControl(method = "cv", number = 10)
# Training the models
model.cv.1 = train(rating~calories , data = dataset, method = "lm",
                    trControl = train.control)
model.cv.2 = train(rating~ poly(calories,2) , data = dataset, method = "lm",
                    trControl = train.control)
model.cv.3 = train(rating~ poly(calories,3) , data = dataset, method = "lm",
                    trControl = train.control)
model.cv.4 = train(rating~ calories:sugars , data = dataset, method = "lm",
                    trControl = train.control)
model.cv.5 = train(rating~calories+sugars+fat+protein+fiber+carbo+sodium+potass, data = dataset, method = "lm",
                    trControl = train.control)

# Summarize the results
print(model.cv.1)
print(model.cv.2)
print(model.cv.3)
print(model.cv.4)
print(model.cv.5)

# method = 'LOOCV' for leave one out cross validation 




