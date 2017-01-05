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

finaldata <-c()

for(i in 1:length(imsi)){
  data <- imsi[[i]]
  eventdata <- xts(rep(-1,length(time_index_daily)), order.by = time_index_daily)
  if(time_index_daily[1]!=data$dts[1]){
    NAtime <- seq(from = as.POSIXct("1972-12-31 00:00"), to = as.POSIXct(data$dts[1])-(3600*24), by = "day")
    eventdata[NAtime] <- -999
  }
  if(time_index_daily[length(time_index_daily)]!=data$dts[length(data$dts)]){
    NAtime2 <- seq(from = as.POSIXct(data$dts[length(data$dts)])+(3600*24), to = as.POSIXct("1995-12-31 23:00"), by = "hour")
    eventdata[NAtime2] <- -999
  }
  #final imputation
  for(i in 1:nrow(data)){
    eventdata[which(time(eventdata) == data$dts[i])] <- data$prcp[i]
  }
  eventdata[eventdata==-1 | eventdata==-999,] <- NA
  colnames(eventdata) <- unique(data$stations.num)
  
  finaldata <- cbind(finaldata, eventdata)
}
colnames(finaldata) <- unique(dataorig$stations.num)

#please change data directory
placedata <- read.csv("~/Desktop/Data/stations_coord.csv", sep=",", header=T)

setwd("~/Dropbox/Github/SNUmultiscale/Data/")

FranceRain <- list()
FranceRain$data <- finaldata
FranceRain$place <- placedata
