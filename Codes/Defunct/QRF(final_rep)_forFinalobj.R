########################################
##Load Packages
########################################
wants <- c("abind", "animation", "automap", "cluster", "ClusterMax", "fields", "forecast", "geoR", "GeoXp", "ggmap", "ggplot2", "gstat", "lubridate", "mapdata", "maps", "maptools", "nlt" , "rgdal", "rgeos", "reshape", "reshape2", "rrcov", "shapefiles", "sp", "SpatialExtremes", "TSA", "psych", "tseries", "wavethresh", "xts")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)
source('~/Dropbox/Github/SNUmultiscale/Codes/Sources/source.R', chdir = TRUE)
#install.packages("reshape2") #for the cast function
library(foreach)
library(doParallel)

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

#plot(rain$place$stations.long[as.numeric(colnames(rain$data))], rain$place$stations.lat[as.numeric(colnames(rain$data))], xlab="lon", ylab="lat", pch=16)
#points(rain$place$stations.long[-as.numeric(colnames(rain$data))], rain$place$stations.lat[-as.numeric(colnames(rain$data))], pch=1, col="red")
#text(rain$place$stations.long, rain$place$stations.lat-0.1, as.character(c(1:40)))
#legend("topleft", legend=c("non-missing", "missing"), pch=c(16,1), col=c("black","red"))

par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(7,5))
for(i in 1:ncol(rain$data)){
  plot(rain$data[,i], type='l')
}
lines(sort(unique(rain_new$lon*rain_new$lat)), monthdata, col="red", cex=2)
#rain_training <- rain$data

#interaction term
#plot(rain_new$lon*rain_new$lat, rain_new$rain)
#monthdata <- rep(0, length(unique(rain_new$lon*rain_new$lat)))
#for(j in 1:length(unique(rain_new$lon*rain_new$lat))){
#  monthdata[j] <- quantile(subset(rain_new[,1], rain_new$lon*rain_new$lat==unique(rain_new$lon*rain_new$lat)[j]), probs=0.998, na.rm=T)
#}
#lines(sort(unique(rain_new$lon*rain_new$lat)), monthdata, col="red", cex=2)
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

rain_new <- rain_new[complete.cases(rain_new),]

#date_index <- c(30, 15, 10, 6, 5, 3, 2, 1)
#omega <- c(pi/6, pi/12, pi/18, pi/30, pi/36, pi/60, 2*pi/179, pi/183)
date_index <- c(30, 15, 10)
omega <- c(pi/6, pi/12, pi/18)
####sampling procedure

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

weighttable <- list();

N <- 1
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
  
  ##97993개
  #set.seed(i)
  #training_index <- sort(sample(c(1:nrow(rain_new)), size=97993))
  #test_index <- c(1:nrow(rain_new))[-training_index]
  
  rain_training <- rain_new
  #rain_test <- rain_new[test_index,]
  
  #plot(rain_new$year, rain_new$rain, xlab="year", ylab="rain")
  #qunatsinmonth <- rep(0, length(sort(unique(rain_new$year))))
  #for(j in 1:length(sort(unique(rain_new$year)))){
  #  qunatsinmonth[j] <- quantile(rain_new$rain[which(rain_new$year==sort(unique(rain_new$year))[j])], probs=0.998, na.rm=T)
  #}
  #lines(sort(unique(rain_new$year)), qunatsinmonth, col="red", cex=2)
  #lm(qunatsinmonth ~sort(unique(rain_new$year)))
  #rq(rain_new$rain[-which(rain_new$year==1972)] ~ rain_new$year[-which(rain_new$year==1972)], tau=0.998)
  #maybe 0.2?
  
  trainingset <- list(); predictset <- list(); qrf <- list(); predictQR <- list(); predicttable <- list(); sqlosstable <- list();
  
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  rainp <- foreach(j=1:length(date_index), .combine="cbind", .packages=c("quantregForest","reshape2", "lubridate", "gbm") ) %dopar% {
    if(j==1){
      ## circular statistics를 확인해보자
      #30 days
      trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*rain_training$month), sinmonth=sin(omega[j]*rain_training$month))
      qrf[[j]] <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.998), ntree=500)
      ## make prediction data
      predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=unique(trainingset[[j]]$month))
      predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
      predictQR[[j]]  <- predict(qrf[[j]], predictset[[j]], what=0.998)
      ## make final result matrix
      predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=predictset[[j]]$month, pred=predictQR[[j]])
      predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, value.var="pred")
      #sqlosstable <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable)=c(1:12); rownames(sqlosstable)=c(1:40)
      #for(k in 1:nrow(rain_test)){
      #  sqlosstable[rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
      #}
      list(p=predicttable[[j]])
    }else if(j==2){
      ## circular statistics
      f2 <- function(x,y) ifelse(x==2, (pmin(1,(y-1)%/%14))+2*(x-1)+1, (pmin(1,(y-1)%/%15))+2*(x-1)+1 )
      trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*f2(rain_training$month, rain_training$day)), sinmonth=sin(omega[j]*f2(rain_training$month, rain_training$day)))
      qrf[[j]] <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.996), ntree=500)
      ## make prediction data
      predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:24))
      predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
      ## make final result matrix
      predictQR[[j]]  <- predict(qrf[[j]], predictset[[j]], what=0.996)
      predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=(predictset[[j]]$month-1)%/%2+1, pred=predictQR[[j]])
      predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, fun.aggregate=max)
      #sqlosstable <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable)=c(1:12); rownames(sqlosstable)=c(1:40)
      #for(k in 1:nrow(rain_test)){
      #  sqlosstable[rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
      #}
      list(p=predicttable[[j]])
    }else if(j==3){
      ## circular statistics
      f3 <- function(x,y) (pmin(2,((y-1)%/%10))+3*(x-1)+1)
      trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*f3(rain_training$month, rain_training$day)), sinmonth=sin(omega[j]*f3(rain_training$month, rain_training$day)) )
      qrf[[j]] <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.994), ntree=500)
      ## make prediction data
      predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:36))
      predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
      ## make final result matrix
      predictQR[[j]]  <- predict(qrf[[j]], predictset[[j]], what=0.994)
      predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=(predictset[[j]]$month-1)%/%3+1, pred=predictQR[[j]])
      predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, fun.aggregate=max)
      #sqlosstable <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable)=c(1:12); rownames(sqlosstable)=c(1:40)
      #for(k in 1:nrow(rain_test)){
      #  sqlosstable[rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
      #}
      list(p=predicttable[[j]])
    }#else if(j==4){
    #  ## circular statistics를 확인해보자
    #  #30 days
    #  trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*rain_training$month), sinmonth=sin(omega[j]*rain_training$month))
    #  qrf[[j]] <- gbm(rain~ lon+lat+cosmonth+sinmonth , data=trainingset[[j]], n.trees = 500, distribution = list(name="quantile", alpha=0.998), cv.folds = 5, shrinkage = 0.01, interaction.depth=4)
    #  ## make prediction data
    #  predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=unique(trainingset[[j]]$month))
    #  predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
    #  ## cross-validation
    #  best.iter <- gbm.perf(qrf[[j]], method="cv", plot.it = TRUE)
    #  ## make final result matrix
    #  predictQR[[j]] <- predict(qrf[[j]], predictset[[j]], best.iter)
    #  predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=predictset[[j]]$month, pred=predictQR[[j]])
    #  predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, value.var="pred")
    #  sqlosstable <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable)=c(1:12); rownames(sqlosstable)=c(1:40)
    #  for(k in 1:nrow(rain_test)){
    #    sqlosstable[rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
    #  }
    #  list(l=sqlosstable,p=predicttable[[j]])
    #}
    ##else if(j==4){
    #  ##for every six days
    #  ## circular statistics
    #  f4 <- function(x,y) (pmin(4,((y-1)%/%6))+5*(x-1)+1)
    #  trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*f4(rain_training$month, rain_training$day)), sinmonth=sin(omega[j]*f4(rain_training$month, rain_training$day)) )
    #  qrf[[j]] <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.99), ntree=500)
    #  ## make prediction data
    #  predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:60))
    #  predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
    #  ## make final result matrix
    #  predictQR[[j]]  <- predict(qrf[[j]], predictset[[j]], what=0.99)
    #  predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=(predictset[[j]]$month-1)%/%5+1, pred=predictQR[[j]])
    #  predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, fun.aggregate=max)
    #  sqlosstable[[j]] <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable[[j]])=c(1:12); rownames(sqlosstable[[j]])=c(1:40)
    #  for(k in 1:nrow(rain_test)){
    #    sqlosstable[[j]][rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
    #  }
    #}else if(j==5){
    #  ##for every five days
    #  ## circular statistics
    #  f5 <- function(x,y) (pmin(5,((y-1)%/%5))+6*(x-1)+1)
    #  trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*f5(rain_training$month, rain_training$day)), sinmonth=sin(omega[j]*f5(rain_training$month, rain_training$day)) )
    #  qrf[[j]] <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.988), ntree=500)
    #  ## make prediction data
    #  predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:72))
    #  predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
    #  ## make final result matrix
    #  predictQR[[j]]  <- predict(qrf[[j]], predictset[[j]], what=0.988)
    #  predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=(predictset[[j]]$month-1)%/%6+1, pred=predictQR[[j]])
    #  predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, fun.aggregate=max)
    #  sqlosstable[[j]] <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable[[j]])=c(1:12); rownames(sqlosstable[[j]])=c(1:40)
    #  for(k in 1:nrow(rain_test)){
    #    sqlosstable[[j]][rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
    #  }
    #}else if(j==6){
    #  ##for every three days
    #  ## circular statistics
    #  f6 <- function(x,y) (pmin(9,((y-1)%/%3))+10*(x-1)+1)
    #  trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*f6(rain_training$month, rain_training$day)), sinmonth=sin(omega[j]*f6(rain_training$month, rain_training$day)) )
    #  qrf[[j]] <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.98), ntree=500)
    #  ## make prediction data
    #  predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:120))
    #  predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
    #  ## make final result matrix
    #  predictQR[[j]]  <- predict(qrf[[j]], predictset[[j]], what=0.98)
    #  predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=(predictset[[j]]$month-1)%/%10+1, pred=predictQR[[j]])
    #  predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, fun.aggregate=max)
    #  sqlosstable[[j]] <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable[[j]])=c(1:12); rownames(sqlosstable[[j]])=c(1:40)
    #  for(k in 1:nrow(rain_test)){
    #    sqlosstable[[j]][rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
    #  }
    #}else if(j==7){
    #  ##for every two days
    #  ## circular statistics
    #  f7 <- function(x,y){
    #    if(x==1){
    #      (min(14,(y-1)%/%2))+15*(x-1)+1
    #    }else if(x==2){
    #      (min(13,(y-1)%/%2))+15*(x-1)+1
    #    }else{
    #      (min(14,(y-1)%/%2))+15*(x-1)
    #    }
    #  }
    #  trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*apply(cbind(x=rain_training$month, y=rain_training$day),1, function(x) f7(x[1],x[2]))), sinmonth=sin(omega[j]*apply(cbind(x=rain_training$month, y=rain_training$day),1, function(x) f7(x[1],x[2]))) )
    #  qrf[[j]] <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.97), ntree=500)
    #  ## make prediction data
    #  predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:179))
    #  predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
    #  ## make final result matrix
    #  predictQR[[j]]  <- predict(qrf[[j]], predictset[[j]], what=0.97)
    #  fmonth <- function(x) (ifelse(x<=29, (x-1)%/%15 + 1, x%/%15 + 1 ))
    #  predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=fmonth(predictset[[j]]$month), pred=predictQR[[j]])
    #  predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, fun.aggregate=max)
    #  sqlosstable[[j]] <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable[[j]])=c(1:12); rownames(sqlosstable[[j]])=c(1:40)
    #  for(k in 1:nrow(rain_test)){
    #    sqlosstable[[j]][rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
    #  }
    #}else if(j==8){
    #  #29, February, 2016
    #  numdate <-  yday(apply(cbind(x=rain_training$month, y=rain_training$day), 1, function(x) paste("2016",formatC(x[1],width=2,flag="0"), formatC(x[2],width=2,flag="0"), sep="-")))
    #  trainingset[[j]] <- cbind(rain_training, cosmonth=cos(omega[j]*numdate), sinmonth=sin(omega[j]*numdate) )
    #  qrf[[j]] <- quantregForest(x=trainingset[[j]][,-c(1,4,5,6,7)], y=trainingset[[j]][,1], what=c(0.94), ntree=500)
    #  ## make prediction data
    #  predictset[[j]] <- expand.grid(stationnum=rain$place$X, month=c(1:366))
    #  predictset[[j]] <- cbind(predictset[[j]], lon=rain$place$stations.long[predictset[[j]]$stationnum], lat=rain$place$stations.lat[predictset[[j]]$stationnum], cosmonth=cos(omega[j]*predictset[[j]]$month), sinmonth=sin(omega[j]*predictset[[j]]$month))
    #  ## make final result matrix
    #  predictQR[[j]]  <- predict(qrf[[j]], predictset[[j]], what=0.94)
    #  #monthnum <- c(31,29,31,30,31,30,31,31,30,31,30,31)
    #  monthnum <- c(31,60,91,121,152,182,213,244,274,305,335,366)
    #  predictQR[[j]] <- data.frame(stationnum=predictset[[j]]$stationnum, month=sapply(predictset[[j]]$month, function(x) which.max(x<=monthnum)), pred=predictQR[[j]])
    #  predicttable[[j]] <- acast(predictQR[[j]], stationnum~month, fun.aggregate=max)
    #  sqlosstable[[j]] <- matrix(NA, nrow=40, ncol=12); colnames(sqlosstable[[j]])=c(1:12); rownames(sqlosstable[[j]])=c(1:40)
    #  for(k in 1:nrow(rain_test)){
    #    sqlosstable[[j]][rain_test$staionnum[k],rain_test$month[k]] <- sum(qlf(rain_test$rain[k], predicttable[[j]][rain_test$staionnum[k],rain_test$month[k]]), na.rm=T)
    #  }
    #}
  }
  #rainptable <- list();
  #rainptable[[1]] <- rainp$l[,c(1:12)]; rainptable[[2]] <- rainp$l[,c(13:24)]; rainptable[[3]] <- rainp$l[,c(25:36)];
  #deno <- Reduce("+", x=lapply(rainptable, function(x) 1/x))
  #weighttable[[i]] <- lapply(rainptable, function(x) 1/x/deno)
  
}



