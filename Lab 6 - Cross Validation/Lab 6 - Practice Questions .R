#Practice Questions

#(a)
set.seed(1)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

#(b)
plot(x,y) #quadratic model

#(c)
library(boot)
set.seed(123)
data=data.frame(y,x)
cv.err=rep(0,4)
for (i in 1:4) {
  model=glm(y~poly(x,i),data=data)
  cv.err[i]=cv.glm(data,model)$delta[1]
}
cv.err

#(d)
set.seed(1)
data=data.frame(y,x)
cv.err=rep(0,4)
for (i in 1:4) {
  model=glm(y~poly(x,i),data=data)
  cv.err[i]=cv.glm(data,model)$delta[1]
}
cv.err
#The results are same as there is no randomness in LOOCV since LOOCV keeps 
#aside a single observation each time and since the dataset is not changing, 
#these single observations wont change. 

#(e)
#The quadratic model has the lowest LOOCV error which is what we expected
#since the actual trend in our data is quadratic as seen in (b). 

#(f)
summary(glm(y ~ x, data = data))
summary(glm(y ~ x + I(x^2), data = data))
summary(glm(y ~ x + I(x^2) + I(x^3), data = data))
summary(glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = data))
#p-values of x and x^2 are the only ones which are <0.05, which shows
#these two are statistically significant in predicting y which is in
#agreement with LOOCV results to not go for a degree 3 or degree 4 model. 

