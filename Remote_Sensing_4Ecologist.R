
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
study_area<- readOGR("F:/03Projects/Remote Sensing for Eco-Logist/data_book/vector_data","study_area_11")
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
