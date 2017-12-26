########################################
##Load Packages
########################################
wants <- c("abind", "animation", "automap", "cluster", "ClusterMax", "fields", "forecast", "geoR", "GeoXp", "ggmap", "ggplot2", "gstat", "lubridate", "mapdata", "maps", "maptools", "nlt" , "rgdal", "rgeos", "rrcov", "shapefiles", "sp", "SpatialExtremes", "TSA", "psych", "tseries", "wavethresh", "xts")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)
source('~/Dropbox/Github/SNUmultiscale/Codes/Sources/source.R', chdir = TRUE)
########################################
##Load Data
########################################
##you should change data location
rain <- readRDS("~/Dropbox/Github/SNUmultiscale/Data/Rain.RDS")
## show spatio-temporal time series
#rain$data
## show locations
#rain$place
sum(!is.na(rain$data))
##1994년 9월 1일부터 크게 줄어듬

plot(rain$place$stations.long, rain$place$stations.lat)

par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(7,5))
for(i in 1:ncol(rain$data)){
  plot(rain$data[,i], type='l')
}

rain_training <- rain$data


## month에 따른 plot 그려보기
par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(7,5))
for(i in 1:ncol(rain$data)){
  boxplot(as.numeric(rain$data[,i])  ~ reorder(format(date(rain$data[,i]), '%m'), date(rain$data[,i])), main=colnames(rain$data)[i])
}


## circular statistics를 확인해보자
month <- as.numeric(format(date(rain$data), '%m')); omega <- pi/6
cosmonth <- cos(omega*month) ; sinmonth <- sin(omega*month)

## function `quantregForest`: quantile regression forests
## x: matrix or data.frame ## y: response variable
## 위도, 경도, 월인 반복측정 자료로 만들어 사용해 보는 것은 어떨까?
## make training data
rain_training_new <- c()
month_format <- format(index(rain_training),"%m")
for(i in 1:ncol(rain_training)){
  #data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), month=month_format)
  data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), cosmonth=cos(pi*as.numeric(month_format)/6), sinmonth=sin(pi*as.numeric(month_format)/6), month=month_format)
  rain_training_new <- rbind(rain_training_new, data_imsi)
}
rain_training_new <- rain_training_new[complete.cases(rain_training_new),]
########################################
##Use of qunatile additive model
########################################
library(quantreg)
##rqss 함수를 사용할 것이다

rain_training_new <- as.data.frame(rain_training_new)

rqss_result <- rqss(rain ~ lon + lat + cosmonth + sinmonth, data=rain_training_new, tau=0.998)
#rqss_result <- rqss(rain_training_new$rain ~ qss(cbind(rain_training_new$lon, rain_training_new$lat, rain_training_new$cosmonth, rain_training_new$sinmonth)), tau=0.998)

## make test data
rain_test_new <- c()
month_format_test <- unique(format(index(rain_training),"%m"))
for(k in 1:nrow(rain$place)){
  #data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 12), lat=rep(rain$place$stations.lat[k], 12), month=unique(format(index(rain_training),"%m")))
  data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 12), lat=rep(rain$place$stations.lat[k], 12), cosmonth=cos(pi*as.numeric(month_format_test)/6), sinmonth=sin(pi*as.numeric(month_format_test)/6), month=month_format_test)
  rain_test_new <- rbind(rain_test_new, data_imsi)
}

rain_test_new <- as.data.frame(rain_test_new)

rqss_predict <- predict(rqss_result, data.frame(lon=rain_test_new$lon, lat=rain_test_new$lat, cosmonth=rain_test_new$cosmonth, sinmonth=rain_test_new$sin))
##newdata should lie in strictly within the convex hull of the fitting data.
cbind(rain_test_new, pred=rqss_predict)

rain_test_matrix <- matrix(rqss_predict, nrow=12)
rain_test_matrix <- rbind(rain_test_matrix[-1,], rain_test_matrix[1,])
rownames(rain_test_matrix) <- c("01","02","03","04","05","06","07","08","09","10","11","12")
colnames(rain_test_matrix) <- c(1:40)

rain_test_matrix_1 <- t(rain_test_matrix)[rownames(t(rain_test_matrix)) %in% C1,]
rain_test_matrix_2 <- rbind(t(rain_test_matrix)[rownames(t(rain_test_matrix)) %in% C1,], t(rain_test_matrix)[rownames(t(rain_test_matrix)) %in% C2,])
colnames(rain_test_matrix_1) = colnames(rain_test_matrix_2) <- Xname
#setwd("~/Dropbox/EVA2017/Results/Middle")
#write.csv(rain_test_matrix_1, "result03(benchmark1).csv")
#write.csv(rain_test_matrix_2, "result03(benchmark2).csv")

##진짜 값?

month_rain_format <- data.frame(month=format(index(rain_training),"%m"))
rain_month <- cbind(rain_training, month=month_rain_format$month)
trueresults <- c()
for(i in 1:length(unique(rain_month$month))){
  comp_quantile <- apply(rain_month[rain_month$month==unique(rain_month$month)[i],], 2, function(x) quantile(x, probs=0.998, na.rm=T)) #전체
  trueresults <- rbind(trueresults, comp_quantile)
}
trueresults <- rbind(trueresults[-1,],trueresults[1,])
row.names(trueresults) <- c(1:12)

