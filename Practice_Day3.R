

x<- seq(1,100, by=2.5)
x[-2]


idx<- c(1,4,6) #create a vector with three number 

library(car)
install.packages(car)
library(car)
install.packages("car")
library("car")
library(car)
install.packages("car")
library(car)
m1<- matrix(c(4,7,3,8,9,2), nrow = 2)# Generate a 2*3 matrix
m1
m2<- matrix(
  c(2,4,3,1,5,7), nrow = 2, ncol = 3, byrow = TRUE)
m2
  
  
  
number_1<- rnorm(80, mean = 0, sd=1) #crea a vector with 80 number 
mat_1<- matrix(number_1, nrow = 20, ncol = 4)
#######
#creat raster
r1<- raster(nrows=10, ncols=10)
library(raster)
install.packages("raster")
library(raster)
library(raster)
install.packages("raster")
library(raster)
r1[]<- rnorm(100) #poplute the raster with value

r1
plot(r1)
library(sp)
poil<- cbind(c(rnorm(10)), c(rnorm(10))) #creat ten random coordinate 

poil
poil.sp<- SpatialPoints(poil)  #convert to spatial point data set
plot(poil.sp) # plot the data
# creat a value 
df<- data.frame(attr1= c("a", "b", "z","d", "e", "q", "w", "r", "z", "y"), attr2= c(101:110))
poi1.spdf<- SpatialPointsDataFrame(poil.sp,df) # add the value to spatial point data set
plot(poi1.spdf) #
####
install.packages("RStoolbox")
library(RStoolbox)
lsat
raster_data[[3]]
lsat[[1]]
plot(lsat[[1]])
plot(lsat$B1_dn)
plot(lsat$B2_dn)
raster_data
x<- lsat[[1]] #first the first band in the new object

x

data(lsat); data(leroy)
library(move)
install.packages("move")
data(lsat); data("leroy")
env<- raster(leroy, vals=rnorm(100))
x<- lsat [1:10,]
x
plot(x)
x<-  lsat[] # all values 

x<- getValues(lsat)
x
plot(x)
x<-  lsat[lsat=10]
x
plot(x)
library(move)
# Get the the vector data from RStoolbox package(Supervised claasification)
poly<- readRDS(system.file("external/trainingPolygons.rds", package = "RStoolbox"))
#Create a raster with the properties (extend and projection the vector )
env<- raster(poly, vals= rnorm(100))
poly
#Extract raster values based on the vector
x<- extract(env, poly)
