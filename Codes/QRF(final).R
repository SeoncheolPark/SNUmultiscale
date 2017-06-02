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

plot(rain$place$stations.long[as.numeric(colnames(rain$data))], rain$place$stations.lat[as.numeric(colnames(rain$data))], xlab="lon", ylab="lat", pch=16)
points(rain$place$stations.long[-as.numeric(colnames(rain$data))], rain$place$stations.lat[-as.numeric(colnames(rain$data))], pch=1, col="red")
text(rain$place$stations.long, rain$place$stations.lat-0.1, as.character(c(1:40)))
legend("topleft", legend=c("non-missing", "missing"), pch=c(16,1), col=c("black","red"))

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
  monthdata <- rep(0,12)
  for(j in 1:12){
    monthdata[j] <- quantile(subset(rain$data[,i], as.numeric(format(date(rain$data[,i]), '%m'))==j), probs=0.998, na.rm=T)
  }
  lines(c(1:12), monthdata, col="red", cex=2)
}

library(lomb) #periodogram with missing data
##이 periodogram이 어떻게 작동하고 있는지 체크해봐야 함
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
date_format <- format(sapply( (as.numeric(format(index(rain_training),"%d"))-1)%/%5, function(x) min(x,5)))
new_format <- paste(month_format, date_format, sep="_")
for(i in 1:ncol(rain_training)){
  #data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), cosmonth=cos(pi*as.numeric(month_format)/6), sinmonth=sin(pi*as.numeric(month_format)/6), month=month_format)
  data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), cosmonth=cos(pi*as.numeric(6*(as.numeric(month_format)-1)+as.numeric(date_format))/36), sinmonth=sin(pi*as.numeric(6*(as.numeric(month_format)-1)+as.numeric(date_format))/36), month=new_format)
  rain_training_new <- rbind(rain_training_new, data_imsi)
}
rain_training_new <- rain_training_new[complete.cases(rain_training_new),]



##plotting with predictor variables
par(mar=c(5.1,4.1,4.1,2.1)/1)
par(mfrow=c(1,1))
plot(rain_training_new$lon,rain_training_new$rain, xlab="lon", ylab="rain")
qunatlon <- rep(0, length(sort(unique(rain_training_new$lon))))
for(j in 1:length(sort(unique(rain_training_new$lon)))){
  qunatlon[j] <- quantile(rain_training_new$rain[which(rain_training_new$lon==sort(unique(rain_training_new$lon))[j])], probs=0.998, na.rm=T)
}
lines(sort(unique(rain_training_new$lon)), qunatlon, col="red", cex=2)

plot(rain_training_new$lat,rain_training_new$rain, xlab="lat", ylab="rain")
qunatlat <- rep(0, length(sort(unique(rain_training_new$lat))))
for(j in 1:length(sort(unique(rain_training_new$lat)))){
  qunatlat[j] <- quantile(rain_training_new$rain[which(rain_training_new$lat==sort(unique(rain_training_new$lat))[j])], probs=0.998, na.rm=T)
}
lines(sort(unique(rain_training_new$lat)), qunatlat, col="red", cex=2)

plot(rain_training_new$cosmonth,rain_training_new$rain, xlab="cosmonth", ylab="rain")
qunatcosmonth <- rep(0, length(sort(unique(rain_training_new$cosmonth))))
for(j in 1:length(sort(unique(rain_training_new$cosmonth)))){
  qunatcosmonth[j] <- quantile(rain_training_new$rain[which(rain_training_new$cosmonth==sort(unique(rain_training_new$cosmonth))[j])], probs=0.998, na.rm=T)
}
lines(sort(unique(rain_training_new$cosmonth)), qunatcosmonth, col="red", cex=2)

plot(rain_training_new$sinmonth,rain_training_new$rain, xlab="sinmonth", ylab="rain")
qunatsinmonth <- rep(0, length(sort(unique(rain_training_new$sinmonth))))
for(j in 1:length(sort(unique(rain_training_new$sinmonth)))){
  qunatsinmonth[j] <- quantile(rain_training_new$rain[which(rain_training_new$sinmonth==sort(unique(rain_training_new$sinmonth))[j])], probs=0.998, na.rm=T)
}
lines(sort(unique(rain_training_new$sinmonth)), qunatsinmonth, col="red", cex=2)

plot(rain_training_new$month,rain_training_new$rain, xlab="month", ylab="rain")
qunatmonth <- rep(0, length(sort(unique(rain_training_new$month))))
for(j in 1:length(sort(unique(rain_training_new$month)))){
  qunatmonth[j] <- quantile(rain_training_new$rain[which(rain_training_new$month==sort(unique(rain_training_new$month))[j])], probs=0.998, na.rm=T)
}
lines(as.numeric(sort(unique(rain_training_new$month))), qunatmonth, col="red", cex=2)



## month를 circular variable로 만들어 넣을 수 있을까

## 분위수 회귀 포레스트 quantile regression forest
#qrf <- quantregForest(x=rain_training_new[,-c(1,6)], y=rain_training_new[,1], what=c(0.998), ntree=500)
qrf <- quantregForest(x=rain_training_new[,-c(1,6)], y=rain_training_new[,1], what=c(0.998), ntree=500)
#qrf <- quantregForest(x=Xtrain, y=Ytrain, nodesize=10,sampsize=30)

## make test data
rain_test_new <- c()
month_format_test <- unique(format(index(rain_training),"%m"))
date_format_test <- c("0","1","2","3","4","5")
new_format_test <- paste(month_format_test, date_format_test, sep="_")
for(k in 1:nrow(rain$place)){
  #data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 12), lat=rep(rain$place$stations.lat[k], 12), cosmonth=cos(pi*as.numeric(month_format_test)/6), sinmonth=sin(pi*as.numeric(month_format_test)/6), month=month_format_test)
  data_imsi <- data.frame(lon=rep(rain$place$stations.long[k], 72), lat=rep(rain$place$stations.lat[k], 72), cosmonth=cos(pi*as.numeric(6*(as.numeric(rep(month_format_test,each=6))-1)+as.numeric(date_format_test))/36), sinmonth=sin(pi*as.numeric(6*(as.numeric(rep(month_format_test,each=6))-1)+as.numeric(date_format_test))/36), month=rep(month_format_test,each=6))
  rain_test_new <- rbind(rain_test_new, data_imsi)
}

predictQR  <- predict(qrf, rain_test_new, what=0.998)
#results <- cbind(rain_test_new, predictQR, month=rep(month_format_test, nrow(rain$place)), station.num=rep(rownames(rain$place), each=12))
results <- cbind(rain_test_new, predictQR,station.num=rep(rownames(rain$place), each=72))

#results_matrix <- matrix(results$predictQR, nrow=12, ncol=40, byrow=F)
results_matrix <- matrix(aggregate(results$predictQR, list(0:(length(results$predictQR)-1) %/% 6), FUN=max)[,2], nrow=12, ncol=40, byrow=F)
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
##TRF(Hothorn, 2017)
########################################
#install.packages(c("alabama","mlt", "libcoin", "inum", "partykit", "alr3", "AppliedPredictiveModeling", "mlbench", "quantregForest"))
### install trtf 0.1-0 and partykit 2.0-0 locally
#install.packages("~/Dropbox/EVA2017/Documents/trtf_packages/partykit_2.0-0.tar.gz", repos=NULL, type="source")
#install.packages("~/Dropbox/EVA2017/Documents/trtf_packages/trtf_0.1-0.tar.gz", repos=NULL, type="source")
library("partykit")
library("trtf")
library("lattice")
head(rain_training_new)

Rain <- subset(rain_training_new, complete.cases(rain_training_new))
Rain <- as.data.frame(lapply(Rain, function(x) {
  x <- x[, drop = TRUE]
  if (is.factor(x)) return(as.ordered(x))
  x
}))
response <- "rain"
Rain[[response]] <- as.numeric(Rain[[response]])

ns <- 20
fm <- rain ~ lon + lat + cosmonth + sinmonth
mtry <- ceiling(length(all.vars(fm[[3]])) / 3) #숫자임
var_m <- numeric_var(name="rain", support = quantile(Rain[[response]], prob = c(.1, .9)), 
                     add = range(Rain[[response]]) - quantile(Rain[[response]], prob = c(.1, .9)), 
                     bounds = c(0, Inf)) #Bernstein basis의 numeric 재료

B_m <- Bernstein_basis(var_m, order = 2, ui = "increasing") #Bernstein basis fcts
uc_ctm_Rain <- ctm(B_m, data = Rain, todistr = "Normal") #conditional transformation models
#uc_ctm_Rain <- ctm(B_m, data = Rain, todistr = "MinExtrVal")
uc_mlt_Rain <- mlt(uc_ctm_Rain, data = Rain, scale = FALSE) #mlt: most likely transformations

c_ctm_Rain <- ctm(B_m, data = Rain, todistr = "Normal", shift = fm[c(1, 3)]) 
#c_ctm_Rain <- ctm(B_m, data = Rain, todistr = "MinExtrVal", shift = fm[c(1, 3)])
c_mlt_Rain <- mlt(c_ctm_Rain, data = Rain, scale = TRUE, maxit = 10000)

nmax=Inf

tt_Rain <- trafotree(uc_ctm_Rain, formula = rain~., data = Rain[,-6], 
                      control = ctree_control(mincriterion = .95, minsplit = 2*ns, minbucket = ns, nmax = nmax),
                      mltargs = list(maxit = 10000, scale = TRUE, gtol = 1e-3, trace = FALSE))

tf_Rain <- traforest(uc_ctm_Rain, formula = fm, data = Rain, ntree = 100, 
                      control = ctree_control(mincriterion = 0, minsplit = 2*ns,
                                              minbucket = ns, nmax = nmax), trace = TRUE, mtry = mtry, 
                      mltargs = list(maxit = 10000, scale = TRUE, gtol = 1e-3, trace = FALSE))

qrf_Rain <- quantregForest(x = Rain[, all.vars(fm[[3]])], y = Rain[, all.vars(fm[[2]])], 
                            nodesize = ns, mtry = mtry, ntree = 100, keep.inbag = TRUE)

