
## 4th Day 
#If a multiplied with a is unequal 2 print something (R is great)
a<- sqrt(2)
if(a*a !=40)
{
  print("R is great!")
}
# j is zero also smaller than 1 and print the function inside the big bracket
j<- 0
while(j<1)
{
  j<- j+0.1;print(j)
}
# add your personlized function
myfunction<- function(x,y){
  z<- x+y
  return(z)
}
#test your function
myfunction(4,3)
# second formula
my2function<- function(x,y){
  x+y
}
my2function(4,3)
# Why is it important for RS (avoid the duplication of duplication)
fun_ndvi<- function(nir, red) {(nir-red)/(nir+red)}
# Import raster data
#raster() import single band
#brick multi layer raster from one file 
#stack ()multi layer from seprate file (same extent and resolution)

Band_1<- raster("F:/03Projects/R practice/crop_p224r63_all_bands.tif", band=1)
Band_2<- raster("F:/03Projects/R practice/crop_p224r63_all_bands.tif", band=2)
Band_3<- raster("F:/03Projects/R practice/crop_p224r63_all_bands.tif", band=3)
Band_4<- raster("F:/03Projects/R practice/crop_p224r63_all_bands.tif", band=4)
Band_5<- raster("F:/03Projects/R practice/crop_p224r63_all_bands.tif", band=5)

# Combind rasters of indentical dimentsions from raster objects
allbands<- stack(Band_1,Band_2,Band_3,Band_4,Band_5)
plot(allbands)
Allband_brick<- brick("F:/03Projects/R practice/crop_p224r63_all_bands.tif")
Allband_brick
# add one more band on your image
allplus<- stack(Allband_brick,Band_2)
allplus
#or 
allplus<- addLayer(Allband_brick, Band_1)
# or rmonving one layer
allwitout<- dropLayer(Allband_brick, Band_1)
allwitout
###ploting raster object
plotRGB(allbands,3,2,1)
# Add color stretch 
plotRGB(allbands, 3,2,1,stretch= "lin")
#### a ggplot2 option using the commands provided by packgage "RStoolbox" -ggplot2 library is required
#RGB plot with lnear stretch 
ggRGB(allbands,3,2,1,stretch = "lin")

#single layer greyscale 
ggR(allbands, layer = 4, maxpixels = 1e6, stretch = "hist")
# single layer map to user defined legend
library(ggplot2)
ggR(allbands, layer = 1, stretch = "lin", geom_raster = TRUE)+ scale_fill_gradient(low="blue", high="green")
# Export it in your path
writeRaster(allbands,dataType='FLT4S', filename = 'new_data.tif',format="GTiff", overwrite=TRUE)
# KML Export
install.packages("maptools")

KML(allbands, filename="Kml_data.kml", col= rainbow(255), maxpixels= 100000)
plot(Band_3)
#draw an extent on the monitor (North-west corner and South-East corner
ext<- drawExtent()
#ext is an object of class extent
band_3_crop<- crop(Band_3, ext)
# grow and shrink extend by multiplying
plot(band_3_crop)
ext*2 # grow the extend in all four directions 
plot(ext, add= TRUE)
# Band calculation 
raster_sd<- calc(band_3_crop, fun = sd)
#adding a calculation in function
fun<- function(x){x/10}
raster_output<- calc(band_3_crop, fun)
raster_output
fun
# set NA value to -999
fun<- function(x){ x[is.na(x)]<- -999 ; return(x)}
raster_output<- calc(allbands,fun)
raster_output
plot(raster_output)
#refer to single layers
raster_output<- calc(band_3_crop,fun = function(x){x[1] +x[2]*x[3]})
raster_output
plot(raster_output)
# NDVI calculation
#import all layer and all points to a single band with [[]]
lsat<- brick("F:/03Projects/R practice/crop_p224r63_all_bands.tif")
ndvi<- (lsat[[4]]- lsat[[3]])/(lsat[[4]]+ lsat[[3]])
#or import single band 

#Make an indvi 
ndvi<- (Band_4- Band_3)/(Band_4+ Band_3)
plot(ndvi)
#if the image is bigger we might use this way 
ndvi<- overlay(Band_4, Band_3, fun= function(nir, red){(nir-red)/ (nir+red)})
plot(ndvi)
# calculate multilayered objec (objec class: Raster Stack Raster Brick)
ndvi<- calc(lsat, fun = function(x){(x[,4]-x[,3]) /(x[,4]+x[,3])})
plot(ndvi)
#or same function but also exporting the resulting VI image 
#SAVI computation with automatic data export
savi<- overlay(Band_4, Band_3, fun= function(nir, red){(nir-red)/(nir+red+0.5)*(1+0.5)}, filename= "savi.tif",format= "GTiff")
#creat your own function in order to avoid the reputation 
fun_ndvi<- function(nir,red){(nir-red)/(nir+red)}
#old
ndvi<- overlay(Band_4, Band_3, fun= function(nir,red){(nir-red)/(nir+red)})
# new verstion
ndvi<- overlay(Band_4, Band_3, fun= fun_ndvi)
plot(ndvi)
###################################33
#writing function for calculation
#get an example of chunk and start from there
x<- lsat [1:10,]
x
plot(x)
######
#vigitation indices 
#creat RVI formula
RVI<- function(nir,red){
  (nir/red)
}
RVI1<- overlay(Band_4, Band_3, fun= RVI)
plot(RVI())
#creat a function for ndvi
ndvi<- function(nir,red){
  (nir-red)/(nir+red)
}
#apply the function on the file 
image_ndvi<- overlay(Band_4,Band_3, fun= ndvi)
plot(image_ndvi)
#write a function for savi(Soil Adjusted Vegitation Index)
SAVI<- function(nir,red) {( nir - red ) / ( nir + red + 1 ) * ( 1 + 1 )}
image_savi<- overlay(Band_2,Band_3, fun= SAVI)
plot(image_savi)
