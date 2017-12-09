# Hausaufgabe für R
library(sp)
pi * 10^2
"*"(pi, "^"(10, 2))


pi * (1:10)^2


x <- pi * 10^2
x
print(x)
print(x, digits = 12)

class(x)
typeof(x)


class(cars)
names(cars)
summary(cars)
str(cars)
class(dist ~ speed)
lm(dist ~ speed, data = cars)

cars$qspeed <- cut(cars$speed, breaks = quantile(cars$speed),
                   include.lowest = TRUE)
as.factor(cars$qspeed)

plot(dist ~ speed, data = cars)
plot(dist ~ qspeed, data = cars)

lm(dist ~ qspeed, data = cars)


library(sp)
getClass("Spatial")
getClass("CRS")


m <- matrix(c(0, 0, 1, 1), ncol = 2, dimnames = list(NULL, c("min", "max")))
crs <- CRS(projargs = as.character(NA))
crs


S <- Spatial(bbox = m, proj4string = crs)
S

bb <- matrix(c(350, 85, 370, 95), ncol = 2, dimnames = list(NULL, c("min", "max")))
Spatial(bb, proj4string = CRS("+proj=longlat"))
getwd()

CRAN_df <- read.table("F:/EAGLE/Courses/MB2 R Programming/Presentation/Home work/CRAN051001a.txt", header = TRUE)
CRAN_mat <- cbind(CRAN_df$long, CRAN_df$lat)
row.names(CRAN_mat) <- 1:nrow(CRAN_mat)
row.names(CRAN_mat)<- 1:nrow(CRAN_mat)
str(CRAN_mat)

getClass("SpatialPoints")


llCRS <- CRS("+proj=longlat +ellps=WGS84")
CRAN_sp <- SpatialPoints(CRAN_mat, proj4string = llCRS)
summary(CRAN_sp)
#retrive bounding box from spatial data
bbox(CRAN_sp)

proj4string(CRAN_sp)
#a
proj4string(CRAN_sp) <- CRS(as.character(NA))
proj4string(CRAN_sp)
#assign the value to CRAN_sp
proj4string(CRAN_sp) <- llCRS

#assign the value of Brazil if find it in the column
brazil <- which(CRAN_df$loc == "Brazil")
# retrive the coordinate from CRAN_sp and assign to Brazil 
coordinates(CRAN_sp)[brazil, ]
#show the summery of the data 
summary(CRAN_sp[brazil, ])

#By using the the matrix[] we can remove the values which are negative 
south_of_equator <- which(coordinates(CRAN_sp)[, 2] <  0)

summary(CRAN_sp[-south_of_equator, ])
#show the structer 
str(row.names(CRAN_df))

#constract dtat frame from spatial point
CRAN_spdf1 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df, proj4string = llCRS, match.ID = TRUE)
CRAN_spdf1[5, ]
str(CRAN_spdf1$loc)
str(CRAN_spdf1[["loc"]])


s <- sample(nrow(CRAN_df))
CRAN_spdf2 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df[s,], proj4string = llCRS, match.ID = TRUE)
#test two object that they are nearly equal (if you get TRUE that is same )
all.equal(CRAN_spdf2, CRAN_spdf1)
#according to row number see where are the information is located
CRAN_spdf2[4, ]
#Create another data frame from CRAN_df to see if the row is not matching 
CRAN_df1<- CRAN_df
# change the row names by following function to letters 
row.names(CRAN_df1)<- sample(c(outer(letters, letters, paste, sep= "")), nrow(CRAN_df1))
# if you now execute the below command you will recieve a message saying that your row names do not match
CRAN_SPDF3<- SpatialPointsDataFrame(CRAN_mat, CRAN_df1, proj4string = llCRS, match.ID = TRUE)
#know about it what is it 
getClass("SpatialPointsDataFrame")
# see the columns name of the spatial dataframe
names(CRAN_spdf1)
#see the structure of it (possibly with lat and long)
str(model.frame(lat ~ long, data = CRAN_spdf1), give.attr = FALSE)
#Contstracting the object by giving SPDF 
CRAN_spdf4 <- SpatialPointsDataFrame(CRAN_sp, CRAN_df)
#then see if it is same
all.equal(CRAN_spdf4, CRAN_spdf2)
#create another data frame for our work
CRAN_df0 <- CRAN_df
CRAN_df0
#give coordinates from another data frame and change it to spatial data frame 
coordinates(CRAN_df0) <- CRAN_mat
#yeep and now check it 
CRAN_df0
# add refrence coordinates 
proj4string(CRAN_df0) <- llCRS
#good job and now see if there is coordinate refrence 
CRAN_df0
#then campare it if it is still same and yeep you got TRUE 
all.equal(CRAN_df0, CRAN_spdf2)
#see the structure 
str(CRAN_df0, max.level = 2)


CRAN_df1 <- CRAN_df
coordinates(CRAN_df1) <- c("long", "lat")
proj4string(CRAN_df1) <- llCRS
str(CRAN_df1, max.level = 2)


turtle_df <- read.csv("E:/12EAGLE/MB2 R Programming/seamap105_mod.csv")
summary(turtle_df)
#Create the testmap mode 
timestamp<- as.POSIXct(strptime(as.character(turtle_df$obs_date),"%m/%d/%Y %H:%M:%S"),"GMT")
#use your command to create reale test map one more column
turtle_df1<- data.frame(turtle_df, timestamp= timestamp)
# create a command that if there is minus value in (lon) column then plus 360 
turtle_df1$lon<- ifelse(turtle_df1$lon< 0, turtle_df1$lon +360, turtle_df1$lon)
#definitly you have no problem with testing it (then do it )
turtle_df1
#create times tamp 
turtle_sp<- turtle_df1[order(turtle_df1$timestamp),]
# Give coordinates columns
coordinates(turtle_sp)<- c("lon","lat")
#you see there is still NA in coord.ref you need one more action 
turtle_sp
#do it now 
proj4string(turtle_sp)<- CRS("+proj=longlate +ellps=WGS84")
#Lets know about type of commands
getClass("Line")
#it is only sp package and has no relation with spatial class
getClass("Lines")
# it has spatial context
getClass("SpatialLines")

install.packages("maps")
install.packages("maptools")
library("maptools")
library(maps)
library(maptools)
#lets use the command to assign for japan (which is comming for spatial line)
japan<- map("world", "japan", plot = FALSE)

#assign the coordinate system 
p4s<- CRS("+proj=longlat +ellps=WGS84")
#create spatial line for japan
SLjapan<- map2SpatialLines(japan,proj4string = p4s)

SLjapan
# see the stracture
str(SLjapan, max.level = 2)
#apply saply to know how many line object it contains 
Lines_len<- sapply(slot(SLjapan, "lines"), function(x)length(slot(x, "Lines")))

#see how many line object it contains 
table(Lines_len)
# we can use ContourLines2SLDF function included in maptools (Converter functions to build SpatialLinesDataFrame objects)
#Using the vlcano exampl (calculate the contour lines from given data set)
volcano_sl<- ContourLines2SLDF(contourLines(volcano))
#we can see the level of contour (there is 10 level name)
t(slot(volcano_sl, "data"))

#give projection to it
llCRS <- CRS("+proj=longlat +ellps=WGS84")
#import data from auckshore (MapGen2SL: will convert data to Spatial Lines DataFrame obeject)
auck_shore <- MapGen2SL("E:/12EAGLE/MB2 R Programming/auckland_mapgen.dat", llCRS)
#see how is it 
summary(auck_shore)
#plot it
plot(auck_shore)
#add connected segment to plot
lns<- slot(auck_shore, "lines")

#table uses the cross-classifying factors to build a contingency table

table(sapply(lns, function(x)length(slot(x, "Lines"))))
#apply function over vectore 
islands_auck<- sapply(lns, function(x){
  crds<- slot(slot(x, "Lines")[[1]], "coords")
  identical(crds[1,], crds[nrow(crds),])})

table(islands_auck)


getClass("Polygon")
getClass("Polygons")
getClass("SpatialPolygons")
#the data is generally big so we can go to get smaller 
islands_sl<- auck_shore[islands_auck]
#add connected line segments to a plot
list_of_Lines <- slot(islands_sl, "lines")

islands_sp <- SpatialPolygons(lapply(list_of_Lines, function(x) {
  Polygons(list(Polygon(slot(slot(x, "Lines")[[1]],
                             "coords"))), ID = slot(x, "ID"))
}), proj4string = CRS("+proj=longlat +ellps=WGS84"))
summary(islands_sp)
slot(islands_sp, "plotOrder")
# order the numbers
order(sapply(slot(islands_sp, "polygons"), function(x)slot(x, "area")), decreasing = TRUE)


library(maps)
# get state map from maps package 
state.map<- map("state", plot = FALSE, fill = TRUE)

#split the infor to state names 
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])

library(maptools)
#convert to spatial polygon
state.sp <- map2SpatialPolygons(state.map, IDs = IDs, proj4string = CRS("+proj=longlat +ellps=WGS84"))
#read the satelite data
sat <- read.table("E:/12EAGLE/MB2 R Programming/state.sat.data_mod.txt", row.names = 5, header = TRUE)
str(sat)
#get and set row names for Data Frames
id <- match(row.names(sat), row.names(state.sp))
id
#apply the row names 
row.names(sat)[is.na(id)]
sat1<- sat[!is.na(id),]

#change spatial data frame polygon 
state.spdf <- SpatialPolygonsDataFrame(state.sp, sat1)
str(slot(state.spdf, "data"))

str(state.spdf, max.level = 2)

#if we channge the row 2 name to Arizon, so it is no longer matching and the error goanna happen
rownames(sat1)[2] <- "Arizona"
# then do the command and you will  recieve error function 
SpatialPolygonsDataFrame(state.sp, sat1)
# assign the value to DC
DC <- "district of columbia"
not_dc <- !(row.names(state.spdf) == DC)
# add the not_dc in the row of the state.spdf 
state.spdf1 <- state.spdf[not_dc, ]
# see the value which are not matching 
table(state.spdf1$mscore)
dim(state.spdf1)

summary(state.spdf1)
install.packages("highfrequency")
load("high.RData")
manitoulin_sp <- load("high.RData")
manitoulin_sp <- high[[4]]
length(slot(manitoulin_sp, "polygons"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "hole"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "ringDir"))

library(rgeos)
manitoulin_sp <- createSPComment(manitoulin_sp)
sapply(slot(manitoulin_sp, "polygons"), comment)
getClass("GridTopology")

bb <- bbox(manitoulin_sp)
bb

cs <- c(0.01, 0.01)
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
manitoulin_grd <- GridTopology(cellcentre.offset = cc,
                               cellsize = cs, cells.dim = cd)
manitoulin_grd
getClass("SpatialGrid")
p4s <- CRS(proj4string(manitoulin_sp))
manitoulin_SG <- SpatialGrid(manitoulin_grd, proj4string = p4s)
summary(manitoulin_SG)
path  <- getwd()
path <- paste(sep="", path , "/70042108/70042108")
auck_ras1 <- raster(paste(sep="",path, ".tif"))
auck_el1 <- as(auck_ras1, 'SpatialGridDataFrame')
auck_el1

plot(auck_el1)
class(auck_el1)

auck_el1$grid
getClass("SpatialGridDataFrame")
slot(auck_el1, "grid")
slot(auck_el1, "bbox")
object.size(auck_el1)
object.size(slot(auck_el1, "data"))
#is.na(auck_el1$data) <- auck_el1$data <= 0
#summary(auck_el1$data)
auck_el2 <- as(auck_el1, 'SpatialPixelsDataFrame')
plot(auck_el2)
object.size(auck_el2)
object.size(slot(auck_el2, "grid.index"))
object.size(slot(auck_el2, "coords"))
#sum(is.na(auck_el1$data)) + nrow(slot(auck_el2, "coords"))
prod(slot(slot(auck_el2, "grid"), "cells.dim"))
auck_el_500 <- auck_el2[auck_el2$band1 > 500, ]
summary(auck_el_500)
object.size(auck_el_500)
data(meuse.grid)
mg_SP <- SpatialPoints(cbind(meuse.grid$x, meuse.grid$y))
summary(mg_SP)
mg_SPix0 <- SpatialPixels(mg_SP)
summary(mg_SPix0)
prod(slot(slot(mg_SPix0, "grid"), "cells.dim"))
mg_SPix1 <- as(mg_SP, "SpatialPixels")
summary(mg_SPix1)
library(raster)
path
r <- raster(paste(sep="",path, ".tif"))
class(r)
inMemory(r)
object.size(r)
cellStats(r, max)
cellStats(r, min)
inMemory(r)


out <- raster(r)
bs <- blockSize(out)
out <- writeStart(out, filename = tempfile(), overwrite = TRUE)
for (i in 1:bs$n) {
  v <- getValues(r, row = bs$row[i], nrows = bs$nrows[i])
  v[v <= 0] <- NA
  writeValues(out, v, bs$row[i])
}
out <- writeStop(out)
cellStats(out, min)
cellStats(out, max)
inMemory(out)
plot(out, col = terrain.colors(100))
r1 <- as(out, "SpatialGridDataFrame")
summary(r1)
