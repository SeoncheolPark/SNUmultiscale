########################################
##Packages
########################################
require(xts)
require(forecast)
#for maps
require(maps)    # Provides functions that let us plot the maps
require(mapdata)    # Contains the hi-resolution points that mark out the countries.
require(sp)
require(rgdal)
require(maptools)
require(rgeos)
require(GeoXp)
require(ggplot2) #to use fortify function
library(shapefiles) #to simplyfy shape files
##For kriging
require(geoR) #for kriging
require(gstat)
require(rrcov)
require(automap)
#for spatial GEVs
require(SpatialExtremes)
require(fExtremes) #for GEV estimation
require(extRemes) #for GEV estimation and model checking
require(ismev)
require(gstat)
########################################
##Data transformation
########################################
#please change data directory
dataorig <- read.csv("~/Desktop/Data/precip_sample.csv", sep=",", header=T)
dataorig$dts <- strptime(dataorig$dts, "%Y.%m.%d", tz="Europe/Paris")
dataorig <- dataorig[,-1]
imsi <- split(dataorig, dataorig$stations.num) #length 35 list
#length(imsi)

time_index_daily <- seq(from = as.POSIXct("1972-12-31 00:00", tz="Europe/Paris"), to = as.POSIXct("1995-12-31 00:00", tz="Europe/Paris"), by = "day")
header_tag <- c("number","date", "precip")

imsi[[1]] <- xts(x=imsi[[1]]$prcp ,order.by=imsi[[1]]$dts, tzone="Europe/Paris")
finaldata <- imsi[[1]]
colnames(finaldata) <- "1"

for(j in 2:length(imsi)){
  imsi[[j]] <- xts(x=imsi[[j]]$prcp ,order.by=imsi[[j]]$dts, tzone="Europe/Paris")
  colnames(imsi[[j]]) <- j
  finaldata <- merge(finaldata, imsi[[j]], all=TRUE)
}
colnames(finaldata) <- unique(dataorig$stations.num)

#please change data directory
placedata <- read.csv("~/Desktop/Data/stations_coord.csv", sep=",", header=T)

setwd("~/Dropbox/Github/SNUmultiscale/Data/")

FranceRain <- list()
FranceRain$data <- finaldata
FranceRain$place <- placedata

saveRDS(FranceRain, "Rain.RDS")
