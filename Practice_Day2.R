# practicing the second day presentation

library(raster)
Afghanistan<- getData("GADM", country= "AFG", level=2)
plot(Afghanistan)
prec<- getData("worldclim", var="prec", res=.5, lon=60, lat= 43 )
plot(prec)
prec_afg1<- crop(prec,Afghanistan)
plot(prec)
plot(prec_afg1)
prec_afg2<- mask(prec_afg1, Afghanistan)
plot(prec_afg2,1)
prec_avg<- cellStats(prec_afg2,stat = "mean")
prec_avg
plot(prec_avg)
