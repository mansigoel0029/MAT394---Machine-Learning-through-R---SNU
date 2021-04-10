####PERCEPTRON LEARNING ALGORITHM####

#Algorithm:
# 1) w1x1 + w2x2 + w3.1 = 0
# 2) Start with any w = (w1,w2,w3)
# 3) Choose a sample point (x,y) which is misclassified by W
# 4) Update W := W + y*x
# 5) Repeat 3,4 till number of missclassified points is 0 

#The line is a.x1 + b.x2 + c.1 =0. For eg. 2 x + 3 y + 1 =0
#Slope = -a/b
#Intercept = -c/b

#Creating a sample dataset with 50 data points randomly chosen from a uniform distribution.
#The dataset has 3 predictor variables x1 and x2.
D <- data.frame(x1 = runif(50, min = -1, max = 1),
                x2 = runif(50, min = -1, max = 1))

#The target function f. 
w=cbind(2,3,1) 

#Creating the chosen function g(x) as defined in lecture notes. sign(∑w_i.a_i)
#The function returns a 1 or a -1. 
g<-function(x, w) {
  return(sign(w[1]*x$x1 + w[2]*x$x2 + w[3]))  
}

#Assignning labels to the sample dataset and storing it in the vector y1. 
y1<-g(D,w)

#Binding together the data points with their respective true labels. 
D<-cbind(D,y1) 

#Plotting the data points and the target function f. 
library(ggplot2)
p_f=ggplot(data=D, aes(x = x1, y = x2, col =as.factor(y1)))+
  geom_point()+
  geom_abline(slope = -w[1]/w[2], intercept = -w[3]/w[2], colour="red")+
  theme(legend.position = "none")
p_f

#Starting with step 2) of the algorithm. 
iter <- 0
w <- cbind(0, 0, 1) 

#Loop to repeatedly perform steps 3) and 4) of the algorithm 
repeat {
  y_pred <- g(D, w) # new labels predicted by w
  D_mis <- subset(D, y1 != y_pred) # misclassified points
  if(nrow(D_mis) == 0)
    break
  x_t <- D_mis[1, ] # select the first misclassified point
  w <- w + c(x_t$x1, x_t$x2,1) * x_t$y1 # PLA updation rule
  iter <- iter + 1
}
#2.01x+3.2y+1=0

#Plotting the target function f(p_f) and the constructed function g(p_g)
p_g=p_f+ geom_abline(slope = -w[1]/w[2], intercept = -w[3]/w[2], colour="blue")
p_g
