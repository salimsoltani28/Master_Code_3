
# Libraries
library(gridExtra)
library(ggplot2)

# Load the diamonds dataset
data(diamonds)

# Create a histogram, assign to "plot1"
plot1 <- qplot(price,data=diamonds,binwidth=1000)

# Create a scatterplot (plot2)
plot2 <- qplot(carat,price,data=diamonds,colour = cut)

# Arrange and display the plots into a 2x1 grid
grid.arrange(plot1,plot2,ncol=1)

install.packages("RCurl")
library(RCurl)
x<- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vTbXxJqjfY-voU-9UWgWsLW09z4dzWsv9c549qxvVYxYkwbZ9RhGE4wnEY89j4jzR_dZNeiWECW9LyW/pub?gid=0&single=true&output=csv")))
#see the data
summary(x)
#instal package
install.packages("reshape")
library(reshape)

x2<- melt(data=x)
#plot
ggplot(x2,aes(x=variable, y=value))+ geom_boxplot()
# add cumulative sum
x.cx<- data.frame(variable.names(x), cs=t(cumsum(x)[nrow(x),]))

#we change the name to fit the melt output and to be able to merge it later on
names(x.cx)<- c("variable", "cumsum")
#reshaping the data, look at the data to see the differences
x2<- melt(data=x)
# merge the two data frames based on "variable" column name "your" name
x3<- merge(x.cx,x2,by.x = "variable", all = T)
#plot
ggplot(x3, aes(x=variable, y=value, color= cumsum))+ geom_point()

############################## work on this
ggplot(x3, aes(x=variable, y= value, color=cumsum))+geom_boxplot(alpha=0.5)+geom_point(alpha=0.7, size= 1.5, position = poisition_jitter(width= .25, hgeit= .25))
#

# load the package to add the gender

install.packages("gender")
library(gender)
x.g<- gender(names (x))
# change the column name of the names to variable again fgor later mergin 
colnames(x.g)[1]<- "variable"
#merge it with the previouslz vcreated data 
x4<- merge(x3, x.g, by.x = "variable", all = T)

#PLOT THE TIME PER PERSON DIVIDED BZ GENDER 
ggplot(x4, aes(x=variable, y=value, color= cumsum))+ geom_boxplot()+ facet_wrap(~gender)

################################################
fieldata_wide<- read.table(header = TRUE, text = '
plot_id  name  Cover  LAI  DBH
1 Sophie 7.9 12,3 10.7
2 Achmed 6.3 10.6 11.1
3 Achmed 9.5 13.1 13.8
4 Sophie 11.5 13.4 12.9
                           ')
fieldata_wide_long<- melt(fieldata_wide, 
                          #ID variable a´- all the variable to keep but not split aprt on 
                          id.vars = c("plot_id", "name"),
                          # the source columns
                          measure.vars = c("Cover", "LAI", "DBH"),
                          #name of thje sestination col.umn that will identifty the original column that the measurm,ent came from 
                         variable_name = "method",
                         value.name= "measurement"
                           )
# look at the data 

# keep plot_id and name but samples should be in columns and populated with the measurment value
install.packages("reshape2")
library(reshape2)
data_wide<- dcast( fieldata_wide_long, plot_id + name~ sample, value.var= "measurement")
####################### GEOFACING COMMAND  
install.packages("ggmap")
library(ggmap)
library(mapproj)
# get the dat for a defin3ed location 
map.wue<- get_map("Wurzburg")
# plot the  map  of thios location 
ggmap(map.wue)
# zoom in 
ggmap(map.wue, zoom= 15)
# over view map
map<- get_map("Bayern", zoom = 6)
# plot map


# #extract the underlzing data frame values 
library(raster)
library(RStoolbox)
lsat.df<- data.frame(coordinates(lsat), getValues(lsat))
## remove background if needed 
lsat.df<- lsat.df[lsat.df$B1_dn!=0,]
# plot the data nad  speicifzy which band to be used 
ggplot(lsat.df)+ geom_raster(aes(x=x, y=y, fill=B3_dn))+ scale_fill_gradient(na.value = NA)+ coord_equal()
# adding another colour scheme
#same as above but other color gradient 
ggplot(lsat.df)+ geom_raster(aes(x=x, y=y, fill= B3_dn))+ scale_fill_gradient(low = "black", high = "white", na.value = NA)+coord_equal()


### same plot as before but store it in "a"
a<- ggplot(lsat.df)+ geom_raster(aes(x=x, y=y, fill= B3_dn))+ scale_fill_gradient(low = "black", high = "white", na.value = NA)+coord_equal()
a# call a then you can plot it #

#get a spatial vector from the RS toolbox package 
library(RStoolbox)
# get the data
poly<- readRDS(system.file("external/trainingPolygons.rds", package = "RStoolbox"))

plots<- as.data.frame(coordinates(poly))
#
a+guides(fill= guide_colorbar())+geom_point(data = plots, aes(x=V1, y=V2), shape=4, colour= "yellow")+ theme(axis.line.x = element_blank())

# steigerwald data 
library(steigerwald)
plot(steigerwald)
