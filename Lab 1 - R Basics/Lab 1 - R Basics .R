####BASIC R COMMANDS####

#c() is used for concatenation
x <- c(5,6,2) # = can also be used instead of <-
y = c(2,6,2)
z = c(x,y)

#Basic Arithmetic Operations
x+y ; x-y ; x/y ; x*y ; x^2 
#Both vectors must be of the same length to perform these operations 

#How to get the documentation of a function
?c

#Other functions 
length(x) ; sqrt(x)
rnorm(50) #creates 50 standard normal random variables [N ~ (0,1)]
rnorm(50,mean=50,sd=0.1)
z=seq(1,10,length=20) #creates a sequence of 20 numbers between 1 to 10 
w=1:10

set.seed(100) #to get reproducible results 
mean(x) ; var(x) ; sd(x)

#Creating a matrix
A=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
A=matrix(data=c(1,2,3,4), nrow=2, ncol=2,byrow=TRUE) #matrix created row wise
dim(A) #dimension of a matrix

####INDEXING IN R####
B=matrix(1:16,4,4)
B[2,3]
B[1:3,2:4]
B[c(1,3),c(2,4)]
B[1:2,]
B[,1:2]
B[1,]
B[-c(1,3),]
B[-c(1,3),-c(1,3,4)]

####PLOTTING IN R####
x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="x-axis",ylab="y-axis",main="Plot of X vs Y")

####LOADING DATA####
data=read.csv("test.csv")
data=na.omit(data) 

library(ISLR)
data("Auto")
attach(Auto) #makes variables in data available by names 
names(Auto)
sum(is.na(Auto)) #checking for NA values 

summary(Auto)
