########################################
##Load Packages
########################################
wants <- c("abind", "animation", "automap", "cluster", "ClusterMax", "fields", "forecast", "geoR", "GeoXp", "ggmap", "ggplot2", "gstat", "lubridate", "mapdata", "maps", "maptools", "nlt" , "rgdal", "rgeos", "rrcov", "shapefiles", "sp", "SpatialExtremes", "TSA", "psych", "tseries", "wavethresh", "xts")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)
source('~/Dropbox/Github/SNUmultiscale/Codes/Sources/source.R', chdir = TRUE)

C1 <- c("2","4","5","6","11","12","13","15","16","18","19","20","21","22","23","24","25","26","28","29","30","32","33","34","35","36","38","39","40")
C2 <- c("7","8","9","10","37")
Xname <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")
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

########################################
##QRF(Meinshausen, 2006)
########################################
library(quantregForest)
library(circular)

par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(7,5))
for(i in 1:ncol(rain$data)){
  plot(apply.monthly(rain$data[,i], max, na.rm=T), type='l', ylim=c(0,13.24))
}


## month에 따른 plot 그려보기
par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(7,5))
for(i in 1:ncol(rain$data)){
  boxplot(as.numeric(rain$data[,i])  ~ reorder(format(date(rain$data[,i]), '%m'), date(rain$data[,i])), main=colnames(rain$data)[i], ylim=c(0,13.24))
}

library(lomb) #periodogram with missing data
par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(7,5))
for(i in 1:ncol(rain$data)){
  lsp(as.numeric(rain$data[,i]))
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

## month를 circular variable로 만들어 넣을 수 있을까

## 분위수 회귀 포레스트 quantile regression forest
qrf <- quantregForest(x=rain_training_new[,-c(1,6)], y=rain_training_new[,1], what=c(0.998), ntree=500)
#qrf <- quantregForest(x=Xtrain, y=Ytrain, nodesize=10,sampsize=30)

## make test data
rain_test_new <- c()
month_format_test <- unique(format(index(rain_training),"%m"))
for(k in 1:nrow(rain$place)){
  #data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 12), lat=rep(rain$place$stations.lat[k], 12), month=unique(format(index(rain_training),"%m")))
  data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 12), lat=rep(rain$place$stations.lat[k], 12), cosmonth=cos(pi*as.numeric(month_format_test)/6), sinmonth=sin(pi*as.numeric(month_format_test)/6), month=month_format_test)
  rain_test_new <- rbind(rain_test_new, data_imsi)
}

predictQR  <- predict(qrf, rain_test_new, what=0.998)
results <- cbind(rain_test_new, predictQR, month=rep(month_format_test, nrow(rain$place)), station.num=rep(rownames(rain$place), each=12))

results_matrix <- matrix(results$predictQR, nrow=12, ncol=40, byrow=F)
rownames(results_matrix) <- c("12","1","2","3","4","5","6","7","8","9","10","11")
colnames(results_matrix) <- c(1:40)
results_matrix <- rbind(results_matrix[-1,], results_matrix[1,])
rownames(results_matrix)[12] <- "12"

results_matrix_1 <- t(results_matrix)[rownames(t(results_matrix)) %in% C1,]
results_matrix_2 <- rbind(t(results_matrix)[rownames(t(results_matrix)) %in% C1,], t(results_matrix)[rownames(t(results_matrix)) %in% C2,])
colnames(results_matrix_1) = colnames(results_matrix_2) <- Xname
#setwd("~/Dropbox/EVA2017/Results/Middle")
#write.csv(results_matrix_1, "result01(benchmark1).csv")
#write.csv(results_matrix_2, "result01(benchmark2).csv")

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
  quilt.plot(results$lon[which(results$month==ind)], results$lat[which(results$month==ind)], log(results$predictQR[which(results$month==ind)]+1), main=ind, zlim=c(0,2))
}

########################################
##스므딩을 한 번 해볼까
########################################
results_matrix2 <- results_matrix
for(i in 1:12){
  Krig_result <- Krig(x=cbind(rain$place$stations.long, rain$place$stations.lat), Y=results_matrix[i,])
  results_matrix2[i,] <- Krig_result$fitted.values
}
rownames(results_matrix2) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
colnames(results_matrix2) <- c(1:40)

results_matrix2 <- matrix(results$predictQR, nrow=12, ncol=40, byrow=F)
rownames(results_matrix2) <- c("12","1","2","3","4","5","6","7","8","9","10","11")
colnames(results_matrix2) <- c(1:40)
results_matrix2 <- rbind(results_matrix2[-1,], results_matrix2[1,])
rownames(results_matrix2)[12] <- "12"

results_matrix2_1 <- t(results_matrix2)[rownames(t(results_matrix2)) %in% C1,]
results_matrix2_2 <- rbind(t(results_matrix2)[rownames(t(results_matrix2)) %in% C1,], t(results_matrix2)[rownames(t(results_matrix2)) %in% C2,])
colnames(results_matrix2_1) = colnames(results_matrix2_2) <- Xname

#write.csv(results_matrix2_1, "result02(benchmark1).csv")
#write.csv(results_matrix2_2, "result02(benchmark2).csv")

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
  quilt.plot(results$lon[which(results$month==ind)], results$lat[which(results$month==ind)], results_matrix2[l,], main=ind, zlim=c(0.6,4))
}