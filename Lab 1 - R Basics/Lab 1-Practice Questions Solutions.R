####Practice Questions Solutions####

#Q1
library(MASS)
data(Boston)
attach(Boston)
dim(Boston)
?Boston

#Q2
plot(dis,crim) 
plot(medv,crim) 
#Those with a smaller value of medv tend to have a higher value of crim 
#After a certain point, an increase in medv seems to have little impact on the crime rate.
#A very similar non-linear relationship can be seen between crim and dis. 

#Q3
cor(crim,Boston) #negative correlation with medv,dis,black, positive correlation with indux,nox,rad,tax

#Q4
High.Crime = Boston[Boston$crim>60,] #8
range(Boston$crim)

#Q5
sum(Boston$chas==1) #35

#Q6
median(ptratio) #19.05


