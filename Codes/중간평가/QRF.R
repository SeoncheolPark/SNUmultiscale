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

###나의 decision
rain_training <- rain$data["/1985"]
rain_validation <- rain$data["1986/"]

### compute validation quantiles

########################################
##QRF(Meinshausen, 2006)
########################################
library(quantregForest)
library(circular)

## month에 따른 plot 그려보기
par(mar=c(5.1,4.1,4.1,2.1)/10)
par(mfrow=c(7,5))
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
  data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), cosmonth=cos(pi*as.numeric(month_format)/6), sinmonth=sin(pi*as.numeric(month_format)/6), month=month_format)
  rain_training_new <- rbind(rain_training_new, data_imsi)
}
rain_training_new <- rain_training_new[complete.cases(rain_training_new),]

## month를 circular variable로 만들어 넣을 수 있을까

## 분위수 회귀 포레스트 quantile regression forest
qrf <- quantregForest(x=rain_training_new[,-c(1,6)], y=rain_training_new[,1])
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

########################################
##Use thin-plate spline
########################################
for(i in 1:12){
  #Tps: x(독립변수), Y(종속변수), 
  #
  results_matrix[i,] <- Tps(x=rain$place[,-1], Y=results_matrix[i,])$fitted.values
}
########################################
##End of thin-plate spline
########################################
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

#진짜 값?
month_validation_format <- data.frame(month=format(index(rain_validation),"%m"))
rain_validation_month <- cbind(rain_validation, month=month_validation_format$month)
colnames(rain_validation_month) <- c(colnames(rain_validation), "month")

trueresults <- c()
for(i in 1:length(unique(rain_validation_month$month))){
  comp_quantile <- apply(rain_validation_month[rain_validation_month$month==unique(rain_validation_month$month)[i],], 2, function(x) quantile(x, probs=0.998, na.rm=T)) #전체
  trueresults <- rbind(trueresults, comp_quantile)
}

residuals_result <- trueresults[,-length(colnames(trueresults))]-results_matrix[,as.numeric(colnames(trueresults)[-length(colnames(trueresults))])]
rownames(residuals_result) <- c(1:12)

par(mar=c(5.1,4.1,4.1,2.1)/5)
par(mfrow=c(4,3))
for(l in 1:12){
  ind <- rownames(residuals_result)[l]
  quilt.plot(rain$place$stations.long[as.numeric(colnames(residuals_result))], rain$place$stations.lat[as.numeric(colnames(residuals_result))], residuals_result[l,], main=ind, zlim=c(-11.5, 11.5))
}

par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
plot(rep(c(1:12),35), residuals_result, type='n')
for(j in 1:ncol(residuals_result)){
  lines(c(1:12), residuals_result[,j])
}


sqlfloss <- sqlf(P=rain_validation, q=results_matrix[,colnames(results_matrix)%in%colnames(rain_validation)])

par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
plot(rep(c(1:12),35), sqlfloss, type='n')
for(j in 1:ncol(sqlfloss)){
  lines(c(1:12), sqlfloss[,j])
}

########################################
##실제 trend (determined 값)들을 크리깅으로 모델링하고 나머지 residual들을 가지고 qrf를 돌려보자
########################################
Krigs <- Krig(x=rain_training_new[,-c(1,6)], Y=rain_training_new[,1])
Krigs$fitted.values
row.names(unique(rain_training_new[,c("lon","lat","cosmonth","sinmonth")]))
Krigs$fitted.values[!is.na(match(row.names(rain_training_new), row.names(unique(rain_training_new[,c("lon","lat","cosmonth","sinmonth")]))))]
cbind(fitted=Krigs$fitted.values[!is.na(match(row.names(rain_training_new), row.names(unique(rain_training_new[,c("lon","lat","cosmonth","sinmonth")]))))], unique(rain_training_new[,c("lon","lat","cosmonth","sinmonth","month")]))
##residual의 계산(maybe data - fitted)
##residual: quantile과 함께 계산해야 할 듯
##quantile 계산을 어떻게 하는가?

trueresults

matrix(trueresults[,-ncol(trueresults)], ncol=1)

rep(as.numeric(matrix(trueresults[,-ncol(trueresults)], ncol=1)), times=repnum)

repnum <- rle(as.numeric(rain_training_new$month))$lengths #고쳐야 함

residuals <- cbind(rain_training_new, fitted=Krigs$fitted.values, residual=0)

# 분위수 회귀 포레스트 quantile regression forest
qrf_res <- quantregForest(x=residuals[,-c(1,6,7)], y=residuals[,7])
#qrf <- quantregForest(x=Xtrain, y=Ytrain, nodesize=10,sampsize=30)

predictQR_res  <- predict(qrf_res, rain_test_new, what=0.998)
results <- cbind(rain_test_new, predictQR_res, month=rep(month_format_test, nrow(rain$place)), station.num=rep(rownames(rain$place), each=12))

results_matrix <- matrix(results$predictQR_res, nrow=12, ncol=40, byrow=F)
rownames(results_matrix) <- c("12","1","2","3","4","5","6","7","8","9","10","11")
colnames(results_matrix) <- c(1:40)
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
  quilt.plot(results$lon[which(results$month==ind)], results$lat[which(results$month==ind)], log(results$predictQR_res[which(results$month==ind)]+1), main=ind, zlim=c(0,2))
}

#진짜 값?
#month_validation_format <- data.frame(month=format(index(rain_validation),"%m"))
#rain_validation_month <- cbind(rain_validation, month=month_validation_format$month)
#colnames(rain_validation_month) <- c(colnames(rain_validation), "month")
#
#trueresults <- c()
#for(i in 1:length(unique(rain_validation_month$month))){
#  comp_quantile <- apply(rain_validation_month[rain_validation_month$month==unique(rain_validation_month$month)[i],], 2, function(x) quantile(x, probs=0.998, na.rm=T)) #전체
#  trueresults <- rbind(trueresults, comp_quantile)
#}
#
#residuals_result <- trueresults[,-length(colnames(trueresults))]-results_matrix[,as.numeric(colnames(trueresults)[-length(colnames(trueresults))])]
#rownames(residuals_result) <- c(1:12)

par(mar=c(5.1,4.1,4.1,2.1)/5)
par(mfrow=c(4,3))
for(l in 1:12){
  ind <- rownames(residuals_result)[l]
  quilt.plot(rain$place$stations.long[as.numeric(colnames(residuals_result))], rain$place$stations.lat[as.numeric(colnames(residuals_result))], residuals_result[l,], main=ind, zlim=c(-11.5, 11.5))
}

par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
plot(rep(c(1:12),35), residuals_result, type='n')
for(j in 1:ncol(residuals_result)){
  lines(c(1:12), residuals_result[,j])
}


sqlfloss <- sqlf(P=rain_validation, q=results_matrix[,colnames(results_matrix)%in%colnames(rain_validation)])

par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
plot(rep(c(1:12),35), sqlfloss, type='n')
for(j in 1:ncol(sqlfloss)){
  lines(c(1:12), sqlfloss[,j])
}



  
  
  
  
  
  
  
  

################################################
##  Load air-quality data (and preprocessing) ##
################################################
data(airquality)
set.seed(1)
## remove observations with mising values
airquality <- airquality[ !apply(is.na(airquality), 1,any), ]
## number of remining samples
n <- nrow(airquality)
## divide into training and test data
indextrain <- sample(1:n,round(0.6*n),replace=FALSE)
Xtrain     <- airquality[ indextrain,2:6]
Xtest      <- airquality[-indextrain,2:6]
Ytrain     <- airquality[ indextrain,1]
Ytest      <- airquality[-indextrain,1]
################################################
##     compute Quantile Regression Forests    ##
################################################
qrf <- quantregForest(x=Xtrain, y=Ytrain)
qrf <- quantregForest(x=Xtrain, y=Ytrain, nodesize=10,sampsize=30)
## for parallel computation use the nthread option
## qrf <- quantregForest(x=Xtrain, y=Ytrain, nthread=8)
## predict 0.1, 0.5 and 0.9 quantiles for test data
conditionalQuantiles  <- predict(qrf,  Xtest)
print(conditionalQuantiles[1:4,])
## predict 0.1, 0.2,..., 0.9 quantiles for test data
conditionalQuantiles  <- predict(qrf, Xtest, what=0.1*(1:9))
print(conditionalQuantiles[1:4,])
## estimate conditional standard deviation
conditionalSd <- predict(qrf,  Xtest, what=sd)
print(conditionalSd[1:4])
## estimate conditional mean (as in original RF)
conditionalMean <- predict(qrf,  Xtest, what=mean)
print(conditionalMean[1:4])
## sample 10 new observations from conditional distribution at each new sample
newSamples <- predict(qrf, Xtest,what = function(x) sample(x,10,replace=TRUE))
print(newSamples[1:4,])
## get ecdf-function for each new test data point
## (output will be a list with one element per sample)
condEcdf <- predict(qrf,  Xtest, what=ecdf)
condEcdf[[10]](30) ## get the conditional distribution at value 30 for i=10
## or, directly, for all samples at value 30 (returns a vector)
condEcdf30 <- predict(qrf, Xtest, what=function(x) ecdf(x)(30))
print(condEcdf30[1:4])
## to use other functions of the package randomForest, convert class back
class(qrf) <- "randomForest"
importance(qrf) ## importance measure from the standard RF
#####################################
## out-of-bag predictions and sampling
##################################
## for with option keep.inbag=TRUE
qrf <- quantregForest(x=Xtrain, y=Ytrain, keep.inbag=TRUE)
## or use parallel version
## qrf <- quantregForest(x=Xtrain, y=Ytrain, nthread=8)
## get quantiles 
oobQuantiles <- predict( qrf, what= c(0.2,0.5,0.8))
## sample from oob-distribution
oobSample <- predict( qrf, what= function(x) sample(x,1))
