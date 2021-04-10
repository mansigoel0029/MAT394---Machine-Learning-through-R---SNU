library(ISLR)
library(MASS)
data(Auto) 
attach(Auto) 
names(Auto) 

#part(a)
median(mpg)
mpg01=rep(0,length(mpg))
mpg01[mpg > median(mpg)]=1
Auto=data.frame(Auto, mpg01)
Auto$name=NULL
Auto$mpg=NULL

#part(b)
cor(Auto[,-9]) #mpg01 is:
#Positively correlated to mpg
#Negatively correlated to cylinders,displacement,horsepower and weight

#part(c)
set.seed(26)
train_indices = sample(1:nrow(Auto),0.8*nrow(Auto))
train=Auto[train_indices, ]
test=Auto[train_indices,]
mpg01_actual=test$mpg01
test$mpg01=NULL

#part(d)
lda_model=lda(mpg01~cylinders+displacement+horsepower+weight,data=train) 
lda_model

lda.pred=predict(lda_model,test) 
names(lda.pred) 

lda.class=lda.pred$class 
table(lda.class,mpg01_actual) 
mean(lda.class!=mpg01_actual) 
mean(lda.class==mpg01_actual) #90% accuracy 

#part(e)
qda_model=qda(mpg01~cylinders+displacement+horsepower+weight,data=train) 
qda_model

qda.pred=predict(qda_model,test) 
names(qda.pred) 

qda.class=qda.pred$class 
table(qda.class,mpg01_actual) 
mean(qda.class!=mpg01_actual) 
mean(qda.class==mpg01_actual) #91% accuracy 

#part(f)
glm.fit=glm(mpg01~cylinders+displacement+horsepower+weight,
            data=train,family =binomial)
summary(glm.fit)

glm.pred=predict(glm.fit,test,type='response') 
glm.predict=rep(0,length(glm.pred))
glm.predict[glm.pred>0.5]=1
table(glm.predict,mpg01_actual) 

mean(glm.predict!=mpg01_actual) 
mean(glm.predict==mpg01_actual) #92% accuracy 

