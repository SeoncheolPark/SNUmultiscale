########################################
##Load Packages
########################################
wants <- c("abind", "animation", "automap", "cluster", "ClusterMax", "fields", "forecast", "geoR", "GeoXp", "ggmap", "ggplot2", "gstat", "lubridate", "mapdata", "maps", "maptools", "nlt" , "rgdal", "rgeos", "rrcov", "shapefiles", "sp", "TSA", "psych", "tseries", "wavethresh", "xts")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)
########################################
##Load Data
########################################
##you should change data location
rain <- readRDS("~/Dropbox/Github/SNUmultiscale/Korea/KoreaRain(dailyfrom1976).RDS")
## show spatio-temporal time series
#rain$data
## show locations
#rain$place
sum(!is.na(rain$data))
##1994년 9월 1일부터 크게 줄어듬

plot(rain$shape)
points(rain$place$longitude, rain$place$latitude, col="red", pch=16)

par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(10,7))
for(i in 1:ncol(rain$data)){
  plot(rain$data[,i], type='l')
}

###나의 decision
rain_training <- rain$data["/1995"]
rain_validation <- rain$data["1996/"]

### compute validation quantiles

########################################
##QRF(Meinshausen, 2006)
########################################
library(quantregForest)
library(circular)

## month에 따른 plot 그려보기
par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(10,7))
for(i in 1:ncol(rain$data)){
  boxplot(as.numeric(rain$data[,i])  ~ reorder(format(date(rain$data[,i]), '%m'), date(rain$data[,i])), outline=F, main=colnames(rain$data)[i])
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
  data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$longitude[which(rain$place$name==colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$latitude[which(rain$place$name==colnames(rain_training)[i])], nrow(rain_training)), cosmonth=cos(pi*as.numeric(month_format)/6), sinmonth=sin(pi*as.numeric(month_format)/6))
  rain_training_new <- rbind(rain_training_new, data_imsi)
}
rain_training_new <- rain_training_new[complete.cases(rain_training_new),]

## month를 circular variable로 만들어 넣을 수 있을까

## 분위수 회귀 포레스트 quantile regression forest
qrf <- quantregForest(x=rain_training_new[,-1], y=rain_training_new[,1])
#qrf <- quantregForest(x=Xtrain, y=Ytrain, nodesize=10,sampsize=30)

## make test data
rain_test_new <- c()
month_format_test <- unique(format(index(rain_training),"%m"))
for(k in 1:nrow(rain$place)){
  #data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 12), lat=rep(rain$place$stations.lat[k], 12), month=unique(format(index(rain_training),"%m")))
  data_imsi <- data.frame(lon=rep(rain$place$longitude[k], 12), lat=rep(rain$place$latitude[k], 12), cosmonth=cos(pi*as.numeric(month_format_test)/6), sinmonth=sin(pi*as.numeric(month_format_test)/6))
  rain_test_new <- rbind(rain_test_new, data_imsi)
}

predictQR  <- predict(qrf, rain_test_new, what=0.998)
results <- cbind(rain_test_new, predictQR, month=rep(month_format_test, nrow(rain$place)), station.num=rep(rownames(rain$place), each=12))

results_matrix <- matrix(results$predictQR, nrow=12, ncol=69, byrow=F)
rownames(results_matrix) <- c("12","1","2","3","4","5","6","7","8","9","10","11")
colnames(results_matrix) <- rain$place$name
results_matrix <- rbind(results_matrix[-1,], results_matrix[1,])
rownames(results_matrix)[12] <- "12"

## for plotting
par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(3,4))
#for(l in 1:length(unique(results$month))){
#  ind <- unique(results$month)[l]
#  quilt.plot(results$lon[which(results$month==ind)], results$lat[which(results$month==ind)], results$predictQR[which(results$month==ind)], main=ind)
#}

#plot(rain$place$stations.long, rain$place$stations.lat, type="n")

for(l in 1:length(unique(results$month))){
  ind <- unique(results$month)[l]
  quilt.plot(results$lon[which(results$month==ind)], results$lat[which(results$month==ind)], results$predictQR[which(results$month==ind)], main=ind, zlim=c(0,242))
}

#진짜 값?
month_validation_format <- data.frame(month=format(index(rain_validation),"%m"))
rain_validation_month <- cbind(rain_validation, month=month_validation_format$month)
colnames(rain_validation_month) <- c(colnames(rain_validation), "month")

trueresults <- c()
for(i in 1:length(unique(rain_validation_month$month))){
  comp_quantile <- apply(rain_validation_month[rain_validation_month$month==unique(rain_validation_month$month)[i],], 2, function(x) quantile(x, probs=0.998, na.rm=T)) #전체
  trueresults <- rbind(trueresults, comp_quantile)
}

residuals_result <- trueresults[,-length(colnames(trueresults))]-results_matrix
rownames(residuals_result) <- c(1:12)

par(mar=c(5.1,4.1,4.1,2.1)/5)
par(mfrow=c(4,3))
for(l in 1:12){
  ind <- rownames(residuals_result)[l]
  quilt.plot(rain$place$longitude, rain$place$latitude, residuals_result[l,], main=ind, zlim=range(residuals_result))
}

par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
plot(rep(c(1:12),69), residuals_result, type='n')
for(j in 1:ncol(residuals_result)){
  lines(c(1:12), residuals_result[,j])
}


sqlfloss <- sqlf(P=rain_validation, q=results_matrix)

par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
plot(rep(c(1:12),69), sqlfloss, type='n')
for(j in 1:ncol(sqlfloss)){
  lines(c(1:12), sqlfloss[,j])
}