########################################
##Load Packages
########################################
wants <- c("abind", "animation", "automap", "cluster", "ClusterMax", "fields", "forecast", "geoR", "GeoXp", "ggmap", "ggplot2", "gstat", "lubridate", "mapdata", "maps", "maptools", "nlt" , "quantregRanger", "rgdal", "rgeos", "rrcov", "shapefiles", "sp", "SpatialExtremes", "TSA", "psych", "tseries", "wavethresh", "xts")
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

## na check
colSums(rain$data==0, na.rm=TRUE)
colSums(!is.na(rain$data))
colSums(rain$data==0, na.rm=TRUE)/colSums(!is.na(rain$data))

## 문제가 있는 station: 1번 station, 31번 station(27번째 줄), 32번 station(28번째 줄)
## 1번 station: 1987년 9월 2일부터 9월 6일까지
rain$data["1987-09-02/1987-09-06",1] <- NA
## 4번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",4], c(0.91,0.94,0.98)))),4] <- NA
## 5번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",5], c(0.91,0.94,0.98)))),5] <- NA
## 7번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",7], c(0.91,0.94,0.98)))),7] <- NA
##10번 station: 1978년 4월 전까지 NA로 처리
rain$data["/1978-03-31",10] <- NA
## 15번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",15], c(0.91,0.94,0.98)))),15] <- NA
## 19번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",19], c(0.91,0.94,0.98)))),19] <- NA
## 22번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",22], c(0.91,0.94,0.98)))),22] <- NA
## 27번 station(23번째 줄):
rain$data["/1975-08-01",23] <- NA
rain$data["1981-08-01/",23] <- NA
## 24번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",24], c(0.91,0.94,0.98)))),24] <- NA
## 31번 station(27번째 줄):
rain$data["/1975-07-22",27] <- NA
## 32번 station(28번째 줄):
rain$data["/1995-04-29",28] <- NA
## 31번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",31], c(0.91,0.94,0.98)))),31] <- NA
## 34번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",34], c(0.91,0.94,0.98)))),34] <- NA

rain_training <- rain$data


########################################
##QRF(Meinshausen, 2006)
########################################
library(quantregForest)
library(circular)

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
  data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), month=month_format)
  #data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), cosmonth=cos(pi*as.numeric(month_format)/6), sinmonth=sin(pi*as.numeric(month_format)/6), month=month_format)
  rain_training_new <- rbind(rain_training_new, data_imsi)
}
rain_training_new <- rain_training_new[complete.cases(rain_training_new),]

## month를 circular variable로 만들어 넣을 수 있을까

## 분위수 회귀 포레스트 quantile regression forest
N <- 100; result_combined <- list();
for(i in 1:100){
  set.seed(i)
  #rain_training_new[,6] <- as.factor(rain_training_new[,6])
  #qrf <- quantregForest(x=rain_training_new[,-c(1,4,5)], y=rain_training_new[,1], what=c(0.998), ntree=500, mtry=1, nodesize=5)
  #qrf <- quantregForest(x=Xtrain, y=Ytrain, nodesize=10,sampsize=30)
  qrf = quantregRanger(rain ~ lon + lat + month, data = rain_training_new, params.ranger = list(mtry = 1, importance="permutation", num.trees=500, min.node.size=20))
  
  
  ## make test data
  rain_test_new <- c()
  month_format_test <- unique(format(index(rain_training),"%m"))
  for(k in 1:nrow(rain$place)){
    data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 12), lat=rep(rain$place$stations.lat[k], 12), month=unique(format(index(rain_training),"%m")))
    #data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 12), lat=rep(rain$place$stations.lat[k], 12), cosmonth=cos(pi*as.numeric(month_format_test)/6), sinmonth=sin(pi*as.numeric(month_format_test)/6), month=month_format_test)
    rain_test_new <- rbind(rain_test_new, data_imsi)
  }
  
  #predictQR  <- predict(qrf, rain_test_new, what=0.998)
  predictQR  <- predict(qrf, rain_test_new, quantiles=c(0.998))
  results <- cbind(rain_test_new, predictQR, month=rep(month_format_test, nrow(rain$place)), station.num=rep(rownames(rain$place), each=12))
  
  #results_matrix <- matrix(results$predictQR, nrow=12, ncol=40, byrow=F)
  results_matrix <- matrix(results$`quantile= 0.998`, nrow=12, ncol=40, byrow=F)
  rownames(results_matrix) <- c("12","1","2","3","4","5","6","7","8","9","10","11")
  colnames(results_matrix) <- c(1:40)
  results_matrix <- rbind(results_matrix[-1,], results_matrix[1,])
  rownames(results_matrix)[12] <- "12"
  
  results_matrix_1 <- t(results_matrix)[rownames(t(results_matrix)) %in% C1,]
  results_matrix_2 <- rbind(t(results_matrix)[rownames(t(results_matrix)) %in% C1,], t(results_matrix)[rownames(t(results_matrix)) %in% C2,])
  colnames(results_matrix_1) = colnames(results_matrix_2) <- Xname
  
  result_combined[[i]] <- list(r=results_matrix_2,q=qrf)
}

result_combined_small <- list()
for(i in 1:length(result_combined)){
  result_combined_small[[i]] <- list(r=result_combined[[i]]$r, v=result_combined[[i]]$q$variable.importance, n=result_combined[[i]]$q$ntree, m=result_combined[[i]]$q$mtry, minnodesize=20)
}

setwd("~/Dropbox/EVA2017/AfterEVA/Data/New")
saveRDS(result_combined_small, "middle100(500treesnocircular)mtry1minnodesize20.RDS")
