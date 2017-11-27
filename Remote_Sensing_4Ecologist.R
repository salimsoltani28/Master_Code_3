
# Practice for Book 

zipfiles<- list.files(path= "C:/Users/Nasrullah/Documents/Downloads", pattern = "S2A_MSIL1C_20170929T060631_N0205_R134_T42SUD_20170929T061504.zip", full.names = TRUE)

eeResults<- lapply(zipfiles,unzip,exdir= "C:/Users/Nasrullah/Documents/Downloads")
eeResults
eeResults<- readEE(eeResults)
library(RStoolbox)
eeResults<- readEE(eeResults)
eeResults
library(ggplot2)

ggplot2(subset(eeResults,Year< 2015& Cloud.cover<20))+ geom_tile(aes(x=Doy,y= Year, alpha= Cloud.cover,fill= satellite), width= 2, size=2)+
  scale_alpha_continuos(name= "Cloud Cover (%)", range= c(1,0.5))
install.packages("ggplot")
################
# GetDATA function
library(raster)
x<- getData("ISO3")
x[x[,"NAME"]=="South Africa",]
prec<- getData("worldclim", var="prec", res= 2.5)
plot(prec,1)
names(prec)<- c("jan", "Feb", "jul")
plot(prec,1)
#Get the vector data from online source
afg<- getData("GADM",country = "AFG", level =1)
#plot the data 
plot(afg)
# Export the data to your hard drive (Vector), First you need to instal (GDAL) package that OGR located under it

install.packages("rgdal")
library(GDAL)
writeOGR()
#Export Raster to hard drive from R
writeRaster(prec)
library(rgdal)
study_area<- readOGR("F:/03Projects/Remote Sensing for Eco-Logist/data_book/vector_data/study_area_11.shp")
setwd("F:/03Projects/Remote Sensing for Eco-Logist/data_book/vector_data")
study_area<- readOGR("study_area_ll.shp")
plot(study_area)
#Bring up the package 
library(wrspathrow)
pathrowSA
#creat pathrowSAsp using formula 
pathrowSAsp<- pathrow_num(study_area,as_polys = TRUE)
# plot it 
plot(pathrowSAsp)
# We can add our study area to see the exact location inside path and row
plot(study_area, add= TRUE)
## set the directory to your raster folder, in order to unzip the data
setwd("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/EarthExplorer")
zipfiles<- list.files(pattern = ".zip", full.names = TRUE)
install.packages("RStoolbox")
library(RStoolbox)
#unzip all files which is located in the folder
eeResults<- lapply(zipfiles, unzip)
eeResults<- readEE(eeResults)
# Do you want see what is going on behind a formula then print it 
print(readEE)
install.packages("ggplot")
library(ggplot2)

# Bring up your climate raster data to work in in R ( I am excited doing that for the first time)

ggplot(subset(eeResults, year< 2015 & cloud.Cover<20))+ geom_tile(aes(x= Doy, Y= Year, alpha= cloud.Cover, fill= Satellite), width= 2 ,size= 2)+ 
  scale_y_continuous(breaks = c(1984:2014))+ scale_alpha_continuous(name= "cloud cover(%)",range = c(1,0.5))


#Access data from open street map
library(osmar)
install.packages("osmar")
study_area_bb<- center_bbox(-49.75, -4.72, 30000, 30000)
OSMdata<- get_osm(study_area_bb, source = osmsource_api())
plot(OSMdata)

#import data raster with one band
library("raster")
#insert the single band using (raster) function 
p224r63_2011_B1<- raster("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B1.tif")
p224r63_2011_B1
Band2<- raster("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B2.tif")
Band3<- raster("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B3.tif")
Band4<- raster("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B4.tif")
Band5<- raster("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B5.tif")
Band6<- raster("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B6.tif")
Band7<- raster("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/LT52240632011210/LT52240632011210CUB01_B7.tif")
plot(Band3)
#stack all Band together 
p224r63_2011<- stack(p224r63_2011_B1,Band2,Band3,Band4,Band5,Band6,Band7)
plot(p224r63_2011)
#easily import all bands with one function 
allband<- list.files("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/All_BandPractice",all.files = FALSE, full.names = TRUE)
# then stack together 
allband
p224r63_2011<- stack(allband)
#Rename or possibly remove some part of names 
names(p224r63_2011)<- gsub(pattern = "LT52240632011210CUB01_","_",x=names)
p224r63_2011

names(p224r63_2011)<- gsub(pattern = "LT52240632011210CUB01_","", x=names(p224r63_2011))
p224r63_2011
#Save the raster in one single Band through use of the symbol
writeRaster(p224r63_2011, filename = "F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/All_BandPractice/LT52240632011210.tif")
# import all bands from a multi band file use the belwo function
p224r63<- brick("F:/03Projects/Remote Sensing for Eco-Logist/data_book/raster_data/final/p224r63_2011.grd")
inMemory(p224r63)
library(sp)
library(rgdal)
#conversion to sp from Raster (sp didnt work as this)
P224r63_2011_sp<- as(p224r63_2011, "spatialPixelsDataFrame")
p224r63_2011

#export raster for making sure that is permanantly saved.

#file format if you are switching between differen sfotware then you have svae it in order other software can read that.
hdr(p224r63_2011_B1,format = "ENVI")
# Add header to ENVI
rasterOptions(addheader = "ENVI")
#read vector recomended command is readOGR because it import with all projection and other information which is needed
vect<- readOGR(dsn = "F:/03Projects/Remote Sensing for Eco-Logist/data_book/vector_data", layer = "area_of_interest")
plot(vect)
#read a vector data from UNEP 
UNEP<- readOGR("F:/03Projects/Remote Sensing for Eco-Logist/data_book/vector_data", "PAs_UNEP_WCMC_p224r63")
plot(UNEP)
#import road data with use of readOGR
road<- readOGR("F:/03Projects/Remote Sensing for Eco-Logist/data_book/vector_data", "roads_p224r63_UTMsouth")
plot(road)
#import GPS file in the R using readOGR command
gpsPoints<- readOGR(dsn = "F:/03Projects/Remote Sensing for Eco-Logist/data_book/vector_data/field_measuerements.gpx", layer = "waypoints")
plot(gpsPoints)
#import CSV files and then convert it to spatial software adaptable 
csv<- read.csv("F:/03Projects/Remote Sensing for Eco-Logist/data_book/vector_data/csv_file_locationdata.csv")
#convert th csv data to spatial data
csv.sp<- SpatialPoints(coords = csv[,c("X","Y")])
#build spatialPointsDataFrame for the data
csv.spdf<- SpatialPointsDataFrame(csv[,c("X","Y")], data = csv[3:5])
csv.spdf
coordinates(csv)<- c("X", "Y")
class(csv)
#Get or set the coordinate reference system (CRS) of a Raster* object.
projection(csv)<- projection(p224r63_2011)
#Check the extent of object
ex<- extent(p224r63_2011)
ex
plot(ex)
ex*0.5
ex
#Exporting vector data, in order to open your data to most GIS program usually the ESRI shp used to export in this format use writeOGR
writeOGR(csv.spdf,"F:/03Projects/Remote Sensing for Eco-Logist/Processed","csv_spdf_as_shp", driver = "ESRI Shapefile" )
#Export to KML, KML require the unprjected x.y and now we can export using writeOGR command
writeOGR(gpsPoints, dsn = "F:/03Projects/Remote Sensing for Eco-Logist/Processed/gpsPoints_GE.kml", layer = "GPSpoints", driver = "KML")
library(maptools)
#to see the prjection of GPS points 
projection(gpsPoints)
#write GPX file 
writeOGR(gpsPoints, "F:/03Projects/Remote Sensing for Eco-Logist/Processed/place_togo.gpx", layer = "waypoints", driver = "GPX", dataset_options = "GPX_USE_EXTENSIONS=YES")
#How to plot spatial data in R
plot(p224r63_2011)
#plot only 5th layer
plot(p224r63_2011,5)
#change the color palette
plot(p224r63_2011,5,col= grey.colors(100))
#set layer transparency to 50%
plot(p224r63_2011,5, alpha= 0.8)
#alternative function (less flexible than plot and no legend)
image(p224r63_2011,5)
#plot with coarse pixel
plot(p224r63_2011,5,maxpixels= 2e+05)
# plot the first four bands 
spplot(p224r63_2011, 1:4)
#plot single layer with different option 
library(RStoolbox)
ggR(p224r63_2011,5)
#plot with legend
ggR(p224r63_2011, 5, geom_raster = TRUE)
#with a custom legend 
ggR(p224r63_2011, 5, geom_raster = TRUE)+ scale_fill_gradientn(colours=rainbow(100))
# plot RGB color for image "stretch remove the value which is beyond the quantile
plotRGB(p224r63_2011,3,2,1, stretch="lin")
p224r63_2011
#plot starting from 4th band
plotRGB(p224r63_2011,4,3,2, stretch="lin")
# plot with ggRGB
ggRGB(p224r63_2011,3,2,1, b=1,stretch = "lin")
# plot band 5
plot(p224r63_2011,5)
#plot the points over the image 
points(csv.spdf,col= "blue")
#add study area on the plot 
plot(study_area, add= TRUE, lwd= 10)
# using plot extend command you can add the extent of shape around it 
plot(road)
plot(extent(road), add=TRUE)
#chang the spatial data frame to nonspatial dataframe
df_pts<- as.data.frame(csv.spdf)
#change the polygon to data frame using below command 
df_poly<- fortify(study_area)
library(ggplot2)
head(df_poly,3)
#change the raster to data frame using ggplot2 but not recommanded for larger raster
 df_ras<- as.data.frame(p224r63_2011,xy=TRUE)
df_ras
df_ras<- fortify(p224r63_2011, maxpixels= 1e+05)
# ggplot2 commands for different data 
#plot raster with the ggplot function 
ggRGB(p224r63_2011, 4, 3, 2, stretch = "lin")
#plot point with 
geom_point(data = df_pts, aes(x= X, y= Y), size= 5, col= "yellow")+
geom_path(data =df_poly,aes(x= long, y= lat, group= group), size= 2, col= "blue")+
coord_equal()
#Basic spatial data handling in R
#Bring up the first row
values<- p224r63_2011_B1[1,]
head(values)
#Second collum
values<- p224r63_2011_B1[,2]
head(values)
#column 7-300
values<- p224r63_2011_B1[,7:300]
head(values)
values
#single pixel 1st row & 2nd column
values<- p224r63_2011_B1[1,2]
values
#get value from specific cell 
p224r63_2011_B1[c(3141,5926)]
#select all value
values<- p224r63_2011_B1[,]

values
#also 
values<- p224r63_2011_B1[]
values
# extract specific band from a stack 
p224r63_2011[[2]]
#Query more than one layer or Band for instance 5 band drom 7
subs<- p224r63_2011[[1:6]]
subs
#also possible with the function 
subs<- p224r63_2011[[c("B1_sre", "B6_bt")]]
subs<- p224r63_2011$LT52240632011210.1
subs
p224r63_2011[[c(1,3,7)]]
# remove layer by adding - sing in your syntix
p224r63_2011.drop15<- p224r63_2011[[-c(1,5)]]
p224r63_2011.drop15
p224r63_2011
#also to remove a lyer
p224r63_2011.drop15<- dropLayer(p224r63_2011, c(1,5))
p224r63_2011.drop15
# specify one layer and divide it by 2 then make a new name for it
newlayer<- p224r63_2011[[1]]/2
names(newlayer)<- "new"
newlayer
#you can add a layer by using the follwoing command 
p224r63_2011.add<- addLayer(p224r63_2011,newlayer)
p224r63_2011.add
#querying pixel will return with single column per layer and and pixel in row
vaues<- p224r63_2011[3:7,84]
vaues
#to extract value from Band5 and row 3-7 and column 90
values<- p224r63_2011[[5]][3:7,90]
values
#
layercopy<- p224r63_2011[[5]]
#query value before modification 
layercopy[70:71,90:91]
#replace values by two NA values 
layercopy[70,90:91]<- NA
layercopy
#query the layer after modification (which NA apears in the result)
layercopy[70:71,90:91]
#set all value of the layer to 0
layercopy[]<-0
layercopy
#query from a specific band all value smaller then 0.2 or any
queryraster<- p224r63_2011[[5]]< 0.2
queryraster
plot(queryraster)
dataType(queryraster)
#now we can use this LOGIC layer to query desired pixel value from 
values<- p224r63_2011[queryraster]
values
layercopy[queryraster]<- NA
