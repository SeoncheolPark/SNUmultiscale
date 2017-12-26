########################################
##Load Packages
########################################
wants <- c("abind", "animation", "cluster", "fields", "forecast", "geoR", "ggmap", "ggplot2", "gstat", "lubridate" ,"quantregRanger", "reshape", "reshape2", "rrcov", "shapefiles", "sp", "TSA", "psych", "tseries", "xts")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)
source('~/Dropbox/Github/SNUmultiscale/Codes/Sources/source.R', chdir = TRUE)
#install.packages("reshape2") #for the cast function
#library(foreach)
#library(doParallel)

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

########################################
##load loss fcts
########################################
##qlf: quantile loss function, 문서의 l에 해당
qlf <- function(x,y,alpha=0.998){
  #x,y: data, alpha: quantile, y: estimated quantile, x: real data
  if(is.na(x)==TRUE | is.na(y)==TRUE){
    return(NA)
  }else if(x>y){
    return(alpha*(x-y))
  }else{
    return((1-alpha)*(y-x))
  }
}

########################################
##QRF(Meinshausen, 2006)
########################################
library(quantregForest)
library(circular)
library(gbm)
library(lubridate)

##many circular transforms

#rainp <- list()

year_format <- as.numeric(format(date(rain$data), '%Y'))
month_format <- as.numeric(format(date(rain$data), '%m'))
date_format <- as.numeric(format(date(rain$data), '%d'))

rain_new <- c()
for(i in 1:ncol(rain$data)){
  data_imsi <- data.frame(rain=as.numeric(rain$data[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain$data)[i])], nrow(rain$data)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain$data)[i])], nrow(rain$data)),year=year_format, month=month_format, day=date_format, staionnum=rep(rain$place$X[as.numeric(colnames(rain$data)[i])], nrow(rain$data)))
  rain_new <- rbind(rain_new, data_imsi)
}
data_imsi <- c()

rain_new <- rain_new[complete.cases(rain_new),]

#date_index <- c(30, 15, 10, 6, 5, 3, 2, 1)
#omega <- c(pi/6, pi/12, pi/18, pi/30, pi/36, pi/60, 2*pi/179, pi/183)
date_index <- c(30, 15, 10)
omega <- c(pi/6, pi/12, pi/18)
####sampling procedure

#setup parallel backend to use many processors
#cores=detectCores()
#cl <- makeCluster(cores[1]-1) #not to overload your computer
#registerDoParallel(cl)

#finaltable <- list();

result_combined_small <- list()

N <- 100
for(i in 1:N){
  qlf <- function(x,y,alpha=0.998){
    #x,y: data, alpha: quantile, y: estimated quantile, x: real data
    if(is.na(x)==TRUE | is.na(y)==TRUE){
      return(NA)
    }else if(x>y){
      return(alpha*(x-y))
    }else{
      return((1-alpha)*(y-x))
    }
  }
  
  cat(paste("step", i), sep="\n")
  
  rain_training <- rain_new
  #rain_test <- rain_new[test_index,]
  
  trainingset <- list(); predictset <- list(); predictQR <- list();
  
  #cores=detectCores()
  #cl <- makeCluster(cores[1]-1) #not to overload your computer
  #registerDoParallel(cl)
  
  #rainp <- foreach(j=1:length(date_index), .combine="cbind", .packages=c("quantregForest","reshape2", "lubridate", "gbm") ) %dopar% {
  for(j in 2:2){
    if(j==1){
      ## circular statistics를 확인해보자
      #30 days
      trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*rain_training$month), sinmonth=sin(omega[j]*rain_training$month))
      qrf <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.998), ntree=500, nodesize=5, mtry=1)
      ## make prediction data
      predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=unique(trainingset[[j]]$month))
      predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
      predictQR[[j]]  <- predict(qrf, predictset[[j]], what=0.998)
      ## make final result matrix
      predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=predictset[[j]]$month, pred=predictQR[[j]])
      predicttable <- acast(predictQR[[j]], stationnum~month, value.var="pred")
    }else if(j==2){
      ## circular statistics
      f2 <- function(x,y) ifelse(x==2, (pmin(1,(y-1)%/%14))+2*(x-1)+1, (pmin(1,(y-1)%/%15))+2*(x-1)+1 )
      trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*f2(rain_training$month, rain_training$day)), sinmonth=sin(omega[j]*f2(rain_training$month, rain_training$day)))
      #qrf <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.998), ntree=500, nodesize=5, mtry=1)
      qrf <- quantregRanger(rain ~ lon + lat + cosmonth + sinmonth, data = trainingset[[j]], params.ranger = list(mtry = 1, importance="permutation", num.trees=500, min.node.size=5))
      ## make prediction data
      predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:24))
      predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
      ## make final result matrix
      #predictQR[[j]]  <- predict(qrf, predictset[[j]], what=0.998)
      predictQR[[j]]  <- predict(qrf, predictset[[j]], quantiles=c(0.998))
      predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=(predictset[[j]]$month-1)%/%2+1, pred=predictQR[[j]])
      predicttable <- acast(predictQR[[j]], stationnum~month, fun.aggregate=mean)
    }else if(j==3){
      ## circular statistics
      f3 <- function(x,y) (pmin(2,((y-1)%/%10))+3*(x-1)+1)
      trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*f3(rain_training$month, rain_training$day)), sinmonth=sin(omega[j]*f3(rain_training$month, rain_training$day)) )
      #qrf <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.998), ntree=500, nodesize=5, mtry=1)
      qrf <- quantregRanger(rain ~ lon + lat + cosmonth + sinmonth, data = trainingset[[j]], params.ranger = list(mtry = 1, importance="permutation", num.trees=500, min.node.size=5))
      ## make prediction data
      predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:36))
      predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
      ## make final result matrix
      #predictQR[[j]]  <- predict(qrf, predictset[[j]], what=0.998)
      predictQR[[j]]  <- predict(qrf, predictset[[j]], quantiles=c(0.998))
      predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=(predictset[[j]]$month-1)%/%3+1, pred=predictQR[[j]])
      predicttable <- acast(predictQR[[j]], stationnum~month, fun.aggregate=mean)
    }
  }
  result_combined_small[[i]] <- list(r=predicttable,v=qrf$variable.importance, n=qrf$num.trees, n=qrf$mtry, minnodesize=qrf$min.node.size)
  #result_combined_small[[i]] <- list(r=finaltable[[i]]$p, v=finaltable[[i]]$q$variable.importance, n=finaltable[[i]]$q$num.trees, m=finaltable[[i]]$q$mtry, minnodesize=finaltable[[i]]$q$min.node.size)
  qrf <- c()
}

setwd("~/Dropbox/EVA2017/AfterEVA/Data/New")
saveRDS(result_combined_small, "2final100(500trees)mtry1minnodesize5(ranger).RDS")



