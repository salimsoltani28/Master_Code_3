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

load("high.RData")
manitoulin_sp <- load("high.RData")
manitoulin_sp <- high[[4]]
length(slot(manitoulin_sp, "polygons"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "hole"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "ringDir"))

library(rgeos) #Interface to Geometry Engine - Open Source
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
path <- paste(sep="", "E:/12EAGLE/MB2 R Programming")
auck_ras1<- raster(paste(sep = "","E:/12EAGLE/MB2 R Programming/70042108.tif"))
#add auck_ras1 with spatialGridDataframe to 
auck_el1 <- as(auck_ras1, 'SpatialGridDataFrame')


plot(auck_el1)
class(auck_el1)
auck_el1$grid

getClass("SpatialGridDataFrame")
slot(auck_el1, "grid")
slot(auck_el1, "bbox")
object.size(auck_el1)
object.size(slot(auck_el1, "data"))
#is.na(auck_el1$band1) <- auck_el1$band1 <= 0
#summary(auck_el1$data)
#include auck_el1 in and change it to SPDF
auck_el2 <- as(auck_el1, 'SpatialPixelsDataFrame')
#plot it
plot(auck_el2)
# see the object size as it is package component
object.size(auck_el2)
#see the object size of subgrid
object.size(slot(auck_el2, "grid.index"))
#also refer to coords one sub grid
object.size(slot(auck_el2, "coords"))
#sum(is.na(auck_el1$data)) + nrow(slot(auck_el2, "coords"))

#to know amount of dim cells
prod(slot(slot(auck_el2, "grid"), "cells.dim"))
#considering raster cell over 500 m
auck_el_500<- auck_el2[auck_el2$band1> 500,  ]
#see the summer 
summary(auck_el_500)
#see the object size
object.size(auck_el_500)
#Prediction Grid for Meuse Data Set
data("meuse.grid")
data(meuse.grid)
#combine and make spatial points to x and y
mg_SP <- SpatialPoints(cbind(meuse.grid$x, meuse.grid$y))
#and see the summery
summary(mg_SP)
#defines  spatial grid by offset
mg_SPix0 <- SpatialPixels(mg_SP)
#see the summery 
summary(mg_SPix0)
#prod returns the product of all the values present in its argument
prod(slot(slot(mg_SPix0, "grid"), "cells.dim"))
mg_SPix1 <- as(mg_SP, "SpatialPixels")
summary(mg_SPix1)
library(raster)
path
#bring the raster
r <- raster( "E:/12EAGLE/MB2 R Programming/70042108.tif")
#to see they type of data 
class(r)
#to see if it is on the temporary memory
inMemory(r)
#see the size of data 
object.size(r)
#max number of cell
cellStats(r, max)
#minimum number of cell
cellStats(r, min)


#temporary file saving
out <- raster(r)
#This function can be used to suggest chunk sizes (always a number of entire rows),
bs <- blockSize(out)
#write values to file 
out<- writeStart(out, filename = tempfile(), overwrite=TRUE)

#as we are dealing with elevation then we dont need the elevation smaller than 0, then assign NA to value of smaller than zero 
for (i in 1:bs$n) {
  v <- getValues(r, row = bs$row[i], nrows = bs$nrows[i])
  v[v <= 0] <- NA
  writeValues(out, v, bs$row[i])
}
#write a values to file 
out <- writeStop(out)
#to see the biggest 
cellStats(out, min)
#see the maximum number of cells
cellStats(out, max)
# see if it is located in the memory
inMemory(out)
#create a vector on n contiguous color
plot(out, col= terrain.colors(100), size=100)
# now conver out as spatialGridDataFrame
r1 <- as(out, "SpatialGridDataFrame")
#see the summery
summary(r1)
#################################third chapter ########################33
#we will work with tradational plotting
library(sp)
#we will use sample data
data("meuse")
#assign coordinates to it
coordinates(meuse) <- c("x", "y")
#plot it
plot(meuse)
#add a title for it
title("points")
#
cc<- coordinates(meuse)
#create a spatial line from meuse point data by joining the points
m.sl<- SpatialLines(list(Lines(list(Line(cc)),"line1")))
#plot it
plot(m.sl)
#use different data and load it 
data("meuse.riv")

#make a list from it 
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), "meuse.riv"))
#create polygon from it 
meuse.pol<- SpatialPolygons(meuse.lst)
#plot it 
plot(meuse.pol, col= "gray")
#give a title for it 
title("Polygon")


#using another technic to increase the readiblity of the map
data("meuse.area")
image(meuse.grid, col = "lightgrey")
#
plot(meuse.pol, col = "grey",, add = TRUE)
#use add command to add on the plo t
plot(meuse, add = TRUE)
#add more layout on the plots
layout(matrix(c(1,2),1,2))
#
plot(meuse.pol,axis=TRUE)

#add two side axis for the map with cex= means symbol size 
axis(1, at = c(178000 + 0:2 * 2000), cex.axis = 0.7)#x
axis(2, at = c(326000 + 0:3 * 4000), cex.axis = 0.7)#y
#add a box around your plot
box()
#devide by par
oldpar = par(no.readonly = TRUE)

layout(matrix(c(1, 2), 1, 2))
# plot once with axes
plot(meuse, axes = TRUE, cex = 0.6)
#plot river on it
plot(meuse.pol, add = TRUE)
#give a title for it 
title("Sample locations")
par(mar = c(0, 0, 0, 0) + 0.1)
# now we campare with no axes 
plot(meuse, axes = FALSE, cex = 0.6)
plot(meuse.pol, add = TRUE)
box()
#par will adjust more space around the 
par(oldpar)

#adding north arrow on the map 
plot(meuse)
plot(meuse.pol, add = TRUE)
plot(meuse)

#add scale bar on the Layout with it after ctrl+enter click on the map to show your north arrow
SpatialPolygonsRescale(layout.scale.bar(), offset = locator(1), scale = 1000, fill = c("transparent", "black"), plot.grid= FALSE)
#place 0 of the scale bar in the bigining of the scale bar by clicking 
text(locator(1), "0")
#same as above place 1km somwhere end of scale bar
text(locator(1), "1 km")
#do you want have the north arrow too then follow the below procedure
SpatialPolygonsRescale(layout.north.arrow(), offset = locator(1), scale = 400, plot.grid = FALSE)


###########3.1.3 Degrees in Axes Labels and Reference Grid  ###

library(maptools)
library(maps)
#extract the map of world from map package with certain x limit and y limit
wrld<- map("world", interior = FALSE, xlim = c(-179, 179), ylim = c(-89,89), plot = FALSE)
# convert map objects to sp classes
wrld_p<- pruneMap(wrld, xlim = c(-179, 179))
#scpecify the coordinate system 
llCRS <- CRS("+proj=longlat +ellps=WGS84")
#make spatial line from wrld_p and give coordinate system 
wrld_sp<- map2SpatialLines(wrld_p, proj4string = llCRS)
#create a new project system 
prj_new <- CRS("+proj=moll")
library(rgdal)
#change the prjection of the data
wrld_proj<- spTransform(wrld_sp, prj_new)
#now make gridlines 
wrld_grd<- gridlines(wrld_sp, easts = c(-179, seq(-150, 150, 50), 179.5), norths = seq(-75, 75, 15), ndiscr = 100)
#change the ptojec
wrld_grd_proj <- spTransform(wrld_grd, prj_new)
#Create N-S and E-W grid lines over a geographic region
at_sp<- gridat(wrld_sp, easts = 0, norths = seq(-75, 75, 15), offset = 0.3)
#change the project 
at_proj <- spTransform(at_sp, prj_new)
#plot the map 
plot(wrld_proj, col= "grey60")
#plot with different color intensity and projection 
plot(wrld_grd_proj, add= TRUE, lty= 0, col= "grey70")
#add lable of coordinates on the plots (gridded curves')
text(coordinates(at_proj), pos= at_proj$pos, offset= at_proj$offset, labels= parse(text = as.character(at_proj$labels)), cex=0.6)

##########3.1.4 Plot Size, Plotting Area, Map Scale and Multiple Plots############3

# to control and see the area of plot 
par("pin")

par(pin = c(4, 4))
# to change the size of plot area we first close the current plot 
dev.off()
#change the size of plot 
X11(width = 10, height = 10)
# if we want write in on the file then we use the bellow command
pdf("file.pdf", width = 5, height = 7)


# change the size of margin 
pin<- par("pin")
# specify xy margin from bounding box of meuese
dxy<- apply(bbox(meuse), 1, diff)
# take a ratio of the two columns 
ratio<- dxy[1]/dxy[2]
#

#if we want to divide the plot area by two or three columns we can do by executing the below command
par(mfrow = c(2, 3))
#
layout(matrix(1:6, 2,3, byrow = TRUE))
par(pin= c(ratio*pin[2], pin[2]), xaxs= "i", yaxs= "i")
# plot with pch=1 circuler style of points 
#use different plot possiblities for ploting 
plot(meuse, pch= 1, cex=1, col="red", bg="gray", )
#draw a bounding box around the plot 
box()
#apply image funcitons on the data 
tryimage<- brick("F:/03Projects/R practice/crop_p224r63_all_bands.tif")
plot(tryimage$crop_p224r63_all_bands.1, zlim= 10, byrow = FALSE)

####Plotting Attributes and Map Legends#####
#select the color
grays= gray.colors(4, 0.55, 0.95)
#
install.packages("gstat")
library(gstat)
library(raster)
library(RStoolbox)

zn.idw<- krige(log(zinc)~1, meuse, meuse.grid)
zn.idw <- krige(log(zinc) ~ 1, meuse, meuse.grid)
image(zn.idw, col= grays, breaks = log(c(100, 200, 400, 800, 1800)) )
image(zn.idw, col = grays, breaks = log(c(100, 200, 400,
                                            + 800, 1800)))
plot(meuse.pol)
plot(meuse, pch = 1, cex = sqrt(meuse$zinc)/20, add = TRUE)
#add legend measure 
legVals <- c(100, 200, 500, 1000, 2000)
#add legend
legend("left", legend = legVals, pch = 1, pt.cex = sqrt(legVals)/20,bty = "n", title = "measured")
# another type of legen
legend("right", legend = c("100-200", "200-400", "400-800","800-1800"), fill = grays, bty = "n", title = "interpolated")

############################Trellis/Lattice Plots with spplot ###############
#provides plotting of spatial data with attributes

zn<- meuse.grid$x
library(lattice)
levelplot(z~x +y| name,spmap.to.lev(zn[c("direct","log")]),asp= "iso")
levelplot(z ~ x + y | name, spmap.to.lev(zn[c("direct","log")]), asp = "iso")
spplot(zn[c("direct", "log")])
#######   
## spplot 
library(maptools)
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")
im <- as.image.SpatialGridDataFrame(meuse.grid["dist"])
cl <- ContourLines2SLDF(contourLines(im))
spplot(cl)
flood<- meuse.grid$ffreq
spplot(flood)


############################
#create river data from meuse.pol data
river<- list("sp.polygons", meuse.pol)
#
north<- list("spatialpolygonsRescale", layout.north.arrow(), offset= c(178750, 332500), scale= 400)
#
scale<- list("spatialPolygonsRescale", layout.scale.bar(), offset= c(180200, 329800),scale= 1000, fill= c("trnsparent", "black"))
#
txt1 <- list("sp.text", c(180200, 329950), "0")
txt2 <- list("sp.text", c(181200, 329950), "1 km")
pts <- list("sp.points", meuse, pch = 3, col = "black")

meuse.layout<- list(river, north, scale, txt1, txt2, pts)

spplot(zn, sp.layout = meuse.layout)

###################ggplot##########333
library(ggplot2)
methods(fortify)


m = as(meuse, "data.frame")
#use ggplot to plot m x vsy (aes used to specify which variable should be plotted)
#geom_point() dictates that the plot should be a scatter plot, and
#coord_equal() makes sure that units along the x-axis equal those along the y-axis (data are projected).
ggplot(m, aes(x,y))+ geom_point()+ coord_equal()

###load the statiscal package 
install.packages("latticeExtra")
library(latticeExtra)
#specify the data and its plot
p = spplot(meuse["zinc"])
#change the type 
m = SpatialPolygonsDataFrame(meuse.pol, data.frame(col = 1),match.ID = FALSE)
#and make aplot of m 
l = spplot(m)
#plot
l + p
p + l

############Interactive Plots###################
plot(meuse)
meuse.id <- identify(coordinates(meuse))

plot(meuse)
region <- locator(type = "o")
n <- length(region$x)
p <- Polygon(cbind(region$x, region$y)[c(1:n, 1), ],hole = FALSE)
ps <- Polygons(list(p), ID = "region")
sps <- SpatialPolygons(list(ps))
plot(meuse[sps, ], pch = 16, cex = 0.5, add = TRUE)

###another type of plot
library(maptools)
#projection 
prj <- CRS("+proj=longlat +datum=NAD27")
#specify the data
nc_shp <- system.file("shapes/sids.shp", package = "maptools")[1]
#
library(rgdal)
#unify the data with other information like projection
nc <- readShapePoly(nc_shp, proj4string = prj)
#plot it
plot(nc)
#get information from points like coordinates and then click on the plot side to specify those points and press escape to finish it
pt <- locator(type = "p")
#print it 
print(pt)
#change to spatial points 
pt.sp = SpatialPoints(cbind(pt$x, pt$y), proj4string = prj)
over(pt.sp, nc)
### make interactive plot and then click on the points in the plot  and it will show the row number
ids <- spplot(meuse, "zinc", identify = TRUE)

#
library(lattice)
trellis.focus("panel", column = 1, row = 1)
# this will give interactive clicking option to click on the plot and it will pop up the row value of corrospondent point
ids <- panel.identify()
trellis.unfocus()

#
library(grid)
trellis.focus("panel", column = 1, row = 1)
as.numeric(grid.locator())
trellis.unfocus()
### select the customized color interpolation
rw.colors<- colorRampPalette(c("red", "white"))
#  plot it in form of colors
image(meuse.grid["dist"], col= rw.colors(10))
#reates nice looking color palettes especially for thematic maps
library(RColorBrewer)
#example of color plette
example(brewer.pal)


#####Class Intervals######
# generate class intervals for colors 
library(RColorBrewer)
install.packages("classInt")
library(classInt)
install.packages("RColorBrewer")
library(RColorBrewer)
# create nice looking palettes for thematic maps
pal <- brewer.pal(5, "Reds")
#5 quantile and intervals
q5 <- classIntervals(meuse$zinc, n = 5, style = "quantile")
q5

#
diff(q5$brks)

#### in fisher style
fj5 <- classIntervals(meuse$zinc, n = 5, style = "fisher")
fj5

#
diff(fj5$brks)
plot(fj5, pal = pal)

#find color arguement to build the legend
q5Colours <- findColours(q5, pal)
plot(meuse, col = q5Colours, pch = 19)
legend("topleft", fill = attr(q5Colours, "palette"),legend = names(attr(q5Colours, "table")), bty = "n")

## try different type of color plting
cuts = (0:10)/10
spplot(meuse.grid, "dist", colorkey = list(labels = list(at = cuts)),at = cuts)





###################chapter 4 Spatial Data Import and Export ########

library(rgdal)
#read the type of datum in news field from online source
NEWS <- "http://svn.osgeo.org/metacrs/proj/trunk/proj/NEWS"
# read text line from connection
PROJ4_NEWS <- readLines(url(NEWS))
# Pattern Matching and Replacement
lns <- grep("Release Notes|EPSG", PROJ4_NEWS)
# show the header of the data
head(PROJ4_NEWS[lns])

#datum from European Petroleam Survey Group (EPSG)
EPSG<- make_EPSG()
#see the information 
EPSG[grep("^# ED50$", EPSG$note), ]

#CRS
CRS("+init=epsg:4230")
#
ED50 <- CRS("+init=epsg:4230 +towgs84=-87,-96,-120,0,0,0,0")
ED50
#

##Projection and Transformation#
IJ.east<- as(char2dms("4d31'00\"E"), "numeric")
# give north info
IJ.north <- as(char2dms("52d28'00\"N"), "numeric")
#change it to spatial point
IJ.ED50 <- SpatialPoints(cbind(x = IJ.east, y = IJ.north), proj4string = ED50)
#
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
#These two helper functions convert character vectors and decimal degree vectors to the DMS-class representation of degrees, minutes, and decimal seconds. "DMS" objects cannot contain NAs.
x <- as(dd2dms(coordinates(res)[1]), "character")
y <- as(dd2dms(coordinates(res)[2], TRUE), "character")
cat(x, y, "\n")
#Euclidean or Great Circle distance between points
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat = TRUE) *1000
#
library(maptools)
gzAzimuth(coordinates(IJ.ED50), coordinates(res))

#campare between the CRS to see some distortions and difference
proj4string(IJ.ED50) <- CRS("+init=epsg:4230")
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat = TRUE) *1000
gzAzimuth(coordinates(IJ.ED50), coordinates(res))

#Pattern Matching and Replacement
EPSG[grep("Atlas", EPSG$note), 1:2]

CRS("+init=epsg:2163")
#List PROJ.4 tag information
proj <- projInfo("proj")
#see the name ifo of the proj
proj[proj$name == "laea", ]

#see the ellipsoied
ellps <- projInfo("ellps")
ellps[grep("a=6370997", ellps$major), ]

#########degree minute second#####

IJ.dms.E <- "4d31'00\"E"
IJ.dms.N <- "52d28'00\"N"
#then we change this chractrastic to DM using below function 
IJ_east <- char2dms(IJ.dms.E)
IJ_north <- char2dms(IJ.dms.N)
IJ_east
IJ_north
# get slots of DMS
getSlots("DMS")
###
c(as(IJ_east, "numeric"), as(IJ_north, "numeric"))

##########Vector File Formats######
# How to read the vector formats
head(ogrDrivers(), n = 10)
#View, List or Get R Source of Package Vignettes
vignette("OGR_shape_encoding", package = "rgdal")
#

scot_dat <- read.table("E:/12EAGLE/MB2 R Programming/scotland.dat", skip = 1)
names(scot_dat)<- c("ID_D","District", "Observed", "Expected", "PcAFF", "Latitude", "Longitude")

#
library(rgdal)
ogrInfo(".", "scot_dat")
# read the data
scot_LL <- readOGR(dsn = "E:/12EAGLE/MB2 R Programming/", layer = "scotland")
# see the coordinates 
proj4string(scot_LL)
#assign a coodinate 
sapply(slot(scot_LL, "data"), class)
#see the ID data 
scot_LL$ID
#see district data of scot
scot_dat$District

# campare the two data and see it is not matching
ID_D <- match(scot_LL$ID, scot_dat$District)
scot_dat
scot_dat1 <- scot_dat[ID_D, ]
row.names(scot_dat1) <- row.names(scot_LL)
library(maptools)
#combind two data
scot_LLa <- spCbind(scot_LL, scot_dat1)
#NOW campare it
all.equal(scot_LLa$ID, scot_LLa$District)


#
scotland<- getData("GADM", country="SCOT", level=2)
install.packages("spdep")
library(spdep)
O <- scot_LLa$Observed
E <- scot_LLa$Expected
scot_LLa$SMR <- probmap(O, E)$relRisk/100
library(DCluster)
scot_LLa$smth <- empbaysmooth(O, E)$smthrr

#Export it to shapefiel by using below command
drv <- "ESRI Shapefile"
writeOGR(scot_BNG, dsn = ".", layer = "scot_BNG", driver = drv)
list.files(pattern = "^scot_BNG")

##WFS Web Feature Service command 

dsn <- "WFS:http://effis.jrc.ec.europa.eu/applications/data-and-services"

ogrListLayers(dsn)
#get the fire data from link 
Fires <- readOGR(dsn, "EFFIS:FiresAll")
#see the title column of data sets
names(Fires)

##
#specify the coordinate system 
x <- c(-15, -15, 38, 38, -15)
y <- c(28, 62, 62, 28, 28)
crds <- cbind(x = x, y = y)
#
bb <- SpatialPolygons(list(Polygons(list(Polygon(coords = crds)),"1")))

library(maptools)
data(wrld_simpl)
proj4string(bb) <- CRS(proj4string(wrld_simpl))
library(rgeos)
#gIntersection: Function for determining the intersection between the two given geometries
slbb <- gIntersection(bb, as(wrld_simpl, "SpatialLines"))
spl <- list("sp.lines", slbb, lwd = 0.7, col = "khaki4")

Fires$dt <- as.Date(as.character(Fires$FireDate), format = "%d-%m-%Y")
Fires0 <- Fires[-which(coordinates(Fires)[, 2] < 0),]
Fires1 <- Fires0[order(Fires0$dt),]
library(spacetime)
Fires2 <- STIDF(as(Fires1, "SpatialPoints"), Fires1$dt,as(Fires1, "data.frame"))
#plot in space and time 
stplot(Fires2, number = 3, sp.layout = spl, cex = 0.5)

###########GPS DATA:OGR GPX #############
#export GPX data
names(Fires1)[1] <- "name" #incase it does not have this column name
GR_Fires <- Fires1[Fires1$Country == "GR", ]
writeOGR(GR_Fires, "EFFIS.gpx", "waypoints", driver = "GPX",dataset_options = "GPX_USE_EXTENSIONS=YES")

#then you can read your exported data
GR <- readOGR("EFFIS.gpx", "waypoints")
GR[1, c(5, 24:28)]




####################Other Import/Export Functions#################

#maptools is alternative for OGR if it is not available 
getinfo.shape("scot_BNG.shp")

#####shapefiles: readShapeSpatial.
###It is matched by an equivalent exporting function: writeSpatialShape in maptools###
  

#######################Raster File Formats########################3
##"ReadImages" and "biOps" packages and the "EBImage"

auck_el1 <- readGDAL("E:/12EAGLE/MB2 R Programming/70042108.tif")
summary(auck_el1)
is.na(auck_el1$band1) <- auck_el1$band1 <= 0 | auck_el1$band1 >10000
#by GDAL.open you can open an image 
#open read only image 
x <- GDAL.open("E:/12EAGLE/MB2 R Programming/70042108.tif")
#getDriver GDALReadOnlyDataset-class 
xx <- getDriver(x)
xx
#show the format
getDriverLongName(xx)
#see the type of 
x
dim(x)

GDAL.close(x)
#Read/write between GDAL grid maps and Spatial objects
GDALinfo("E:/12EAGLE/MB2 R Programming/70042108.tif")
#assing the value 
barks<- c(10,20,50,100, 150,200, 300,400, 500, 600, 700)
#Color Palettes
pal<- terrain.colors(11)
pal
#campare it 
length(pal)== length(pal)-1

auck_el1$band1<- findInterval(auck_el1$band1, vec = barks, all.inside = TRUE)-1

writeGDAL(auck_el1,"demIndex.tif", drivername = "GTiff", type = "Byte", colorTables = list(pal), mvFlag = length(barks)-1)

Gi <- GDALinfo("demIndex.tif", returnColorTable = TRUE)

CT<- attr(Gi, "ColorTable", returnColorTable= TRUE)
CT[CT > "#000000"]


library(gstat)
#IDW
log_zinc <- idw(log(zinc) ~ 1, meuse, meuse.grid)["var1.pred"]
Summary(log_zinc)
#EXPORT to geotif format
writeGDAL(log_zinc, fname = "log_zinc.tif", drivername = "GTiff",type = "Float32", options = "INTERLEAVE=PIXEL")

GDALinfo("log_zinc.tif")


### use soil data 

soil<- meuse.grid["soil"]
table(soil$soil)

#change it as integer using following command 

soil$soil<- as.integer(soil$soil)-1
Cn <- c("Rd10A", "Rd90C/VII", "Bkd26/VII")

#export the data
writeGDAL(soil, "Soil.tif", drivername = "GTiff", type = "Byte",catNames = list(Cn), mvFlag = length(Cn))
#prepare for info check 
Gi <- GDALinfo("Soil.tif", returnCategoryNames = TRUE)
attr(Gi, "CATlist")[[1]]
#take a summery of soil data
summary(readGDAL("Soil.tif"))

#R driver format
writeGDAL(log_zinc, fname = "log_zinc.rda", drivername = "R")
GDALinfo("log_zinc.rda")

# GDAL support the wms service
service_xml <- "frmt_wms_openstreetmap_tms.xml"

offset <- c(19339000, 34546000)
osm <- readGDAL(service_xml, offset = offset, region.dim = c(2000,2000), output.dim = c(1000, 1000))
summary(osm)

#####Google Map, X
install.packages("RgoogleMaps")
library(RgoogleMaps)
MYMAP<- GetMap(center = c(60.395, 5.322), zoom = 16, destfile = "MyTile2.png", maptype = "mobile")
BB <- do.call("rbind", MYMAP$BBOX)
dBB<- rev(diff(BB))
DIM12 <- dim(MYMAP$myTile)[1:2]
cs <- dBB/DIM12
cc <- c(BB[1, 2] + cs[1]/2, BB[1, 1] + cs[2]/2)
#Grid TOpology
GT <- GridTopology(cc, cs, DIM12)
p4s <- CRS("+proj=longlat +datum=WGS84")

SG_myMap <- SpatialGridDataFrame(GT, proj4string = p4s,
                                 data = data.frame(r = c(t(myMap$myTile[, , 1])) *
                                                        255, g = c(t(myMap$myTile[, , 2])) * 255, b = c(t(myMap$myTile[,
                                                                                                                        , 3])) * 255))

myMap1 <- GetMap.OSM(lonR = c(5.319, 5.328), latR = c(60.392,
                                                      60.398), scale = 4000, destfile = "MyTile.png")

#download data from open street map 
library(osmar)
api <- osmsource_api()
box <- corner_bbox(5.319, 60.392, 5.328, 60.398)
torget <- get_osm(box, source = api)
torget1 <- as_sp(torget, "lines")
sort(table(torget1$user), decreasing = TRUE)[1:3]
plot(torget1)

bybane <- find(torget, way(tags(k == "light_rail")))
plot(bybane)
bybane <- find_down(torget, way(bybane))
bybane <- subset(torget, ids = bybane)
bybane <- as_sp(bybane, "lines")

##
#eXport to KML
writeOGR(Fires[, c("gml_id", "FireDate", "Area_HA")],dsn = "fires.kml", layer = "fires", driver = "KML")

library(maptools)
grd <- as(meuse.grid, "SpatialPolygons")
proj4string(grd) <- CRS(proj4string(meuse))
grd.union <- unionSpatialPolygons(grd, rep("x", length(slot(grd,
                                                              "polygons"))))
ll <- CRS("+proj=longlat +datum=WGS84")
grd.union.ll <- spTransform(grd.union, ll)
