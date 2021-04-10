#ggplot2 also termed as Grammer of Graphics is a free, opensource and easy to use 
#visualization package widely used in R. 
#It is the most powerful visualization package written by Hadley Wickham.

####COMPONENTS OF GGPLOT: 
#Data:The dataset for which you want to plot the graph 
#Aesthetics:The scales onto which the data is mapped(x axis, y axis, fill, colour, shape, size)
#Geometry:Visual elements to plot the data(points, line, histogram etc)
#Facets:Groups by which we divide the data(useful for large datasets)

                               #################

library(datasets)
data(iris)
attach(iris)

library(ggplot2)
ggplot(data=iris) #data
ggplot(data=iris,aes(x=Sepal.Length,y=Petal.Length)) #aesthetics
ggplot(data=iris,aes(x=Sepal.Length,y=Petal.Length)) + geom_point() #geometry (geom_point is for scatterplots)

####Aesthetics
ggplot(data=iris,aes(x=Sepal.Length,y=Petal.Length,col=Species)) + geom_point()
ggplot(data=iris,aes(x=Sepal.Length,y=Petal.Length,shape=Species)) + geom_point()
ggplot(data=iris,aes(x=Sepal.Length,y=Petal.Length,col=Species,shape=Species)) + geom_point()

####Geometries
data=read.csv("houses.csv")
data$X.1=NULL
data$X=NULL

#Histogram
ggplot(data=data,aes(x=price)) + geom_histogram()
ggplot(data=data,aes(x=price)) + geom_histogram(bins=100)
ggplot(data=data,aes(x=price)) + geom_histogram(bins=50,fill="red")
ggplot(data=data,aes(x=price)) + geom_histogram(bins=50,fill="red",col="green")
ggplot(data=data,aes(x=price,fill=as.factor(air_cond))) + geom_histogram(position="fill")
ggplot(data=data,aes(x=price,fill=as.factor(air_cond))) + geom_histogram()

#position gives the proportion of houses, as price increases,
#a greater proportion of houses have air conditioning 
#houses with prices close to 8lakhs will have air conditioning 


#Bar Plots
ggplot(data=data,aes(x=waterfront)) +geom_bar()

ggplot(data=data,aes(x=waterfront,fill=as.factor(air_cond))) +geom_bar(position="fill")
#make a bar plot which indicates presence of waterfront in a house along with
#an indication of how many houses will have air conditioning in both the categories


#Smooth Line
ggplot(data=data, aes(y=price,x=living_area))+geom_smooth()
ggplot(data=data, aes(y=price,x=living_area,col=as.factor(air_cond)))+geom_smooth()

####Faceting

ggplot(data=data,aes(y=price,x=living_area,col=as.factor(air_cond)))+
  geom_point()+geom_smooth()+facet_grid(~air_cond)


#modify the above line of code to colour and divide data based on the number of fireplaces 
ggplot(data=data,aes(y=price,x=living_area,col=as.factor(fireplaces)))+
  geom_point()+geom_smooth()+facet_grid(~fireplaces)

