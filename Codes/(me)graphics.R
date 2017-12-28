
library(adaptsmoothst)
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

par(mfrow=c(1,1))
plot(rain$place$stations.long, rain$place$stations.lat, type='n')
text(rain$place$stations.long[as.numeric(C1)], rain$place$stations.lat[as.numeric(C1)], C1)
text(rain$place$stations.long[as.numeric(C2)], rain$place$stations.lat[as.numeric(C2)], C2, col="blue")
text(rain$place$stations.long[c(1,3,14,17,27,31)], rain$place$stations.lat[c(1,3,14,17,27,31)], c("1","3","14","17","27","31"), col="red")
legend("topleft", col=c(1,4,2), pch=c(16,16,16), c("C1","C2","Not Included"))

par(mfrow=c(1,1))
plot(rain$place$stations.long, rain$place$stations.lat, type='n')
text(rain$place$stations.long, rain$place$stations.lat, c(1:40), col="red")
text(rain$place$stations.long[as.numeric(colnames(rain$data))], rain$place$stations.lat[as.numeric(colnames(rain$data))], colnames(rain$data), col="black")
legend("topleft", col=c(1,2), pch=c(16,16), c("Data Available", "Not Available"))


# Example data
tmin <- as.Date("2000-01-01")
tmax <- as.Date("2001-01-01")
tlab <- seq(tmin, tmax, by="month")
lab <- format(tlab,format="%Y-%b")
set.seed(111)
x <- seq(tmin, tmax, , 100)
y <- cumsum(rnorm(100))

# Plot
plot(x, y, t="l", xaxt="n", xlab="")
axis(1, at=tlab, labels=FALSE)
text(x=tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
     labels=lab, srt=45, adj=1, xpd=TRUE)

########################################
##Response variable vs Explanatory variable
########################################
##you should change data location
rain <- readRDS("~/Dropbox/Github/SNUmultiscale/Data/Rain.RDS")
## show spatio-temporal time series
rain_training <- rain$data

## make training data
rain_training_new <- c()
month_format <- format(index(rain_training),"%m")
year_format <- format(index(rain_training), "%Y")
date_format <- format(sapply( (as.numeric(format(index(rain_training),"%d"))-1)%/%5, function(x) min(x,5)))
new_format <- paste(month_format, date_format, sep="_")
for(i in 1:ncol(rain_training)){
  #data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), cosmonth=cos(pi*as.numeric(month_format)/6), sinmonth=sin(pi*as.numeric(month_format)/6), month=month_format)
  data_imsi <- data.frame(rain=as.numeric(rain_training[,i]), lon=rep(rain$place$stations.long[as.numeric(colnames(rain_training)[i])], nrow(rain_training)),lat=rep(rain$place$stations.lat[as.numeric(colnames(rain_training)[i])], nrow(rain_training)), cosmonth=cos(pi*as.numeric(6*(as.numeric(month_format)-1)+as.numeric(date_format))/36), sinmonth=sin(pi*as.numeric(6*(as.numeric(month_format)-1)+as.numeric(date_format))/36), year=year_format, month=month_format, monthday=new_format)
  rain_training_new <- rbind(rain_training_new, data_imsi)
}
rain_training_new <- rain_training_new[complete.cases(rain_training_new),]

png("Scatter.png", height=4.54, width=6.57, units = "in", res=100)
##plotting
par(mar=c(6.1,5.1,3.1,1.1)/1.25)
par(mfrow=c(2,2))

#year
#plot(as.factor(rain_training_new$year),rain_training_new$rain, xlab="year", ylab="precip", main="(a)")
#plot(as.numeric(as.character(rain_training_new$year[rain_training_new$rain>0.2])),rain_training_new$rain[rain_training_new$rain>0.2], xlab="year", ylab="precip", main="(a)")
plot(as.numeric(as.character(rain_training_new$year)),rain_training_new$rain, xlab="Year", ylab="Precip", main="(a)")
quantyear <- rep(0, length(sort(unique(rain_training_new$year))))
for(j in 1:length(sort(unique(rain_training_new$year)))){
  quantyear[j] <- quantile(rain_training_new$rain[which(rain_training_new$year==sort(unique(rain_training_new$year))[j])], probs=0.998, na.rm=T)
}
lines(sort(as.numeric(as.character(unique(rain_training_new$year)))), quantyear, col="red", cex=2, lwd=2)
#lines(sort(as.factor(unique(rain_training_new$year))), quantyear, col="red", cex=2, lwd=2)

#month
#plot(rain_training_new$month,rain_training_new$rain, xlab="month", ylab="precip", main="(b)")
#plot(as.numeric(rain_training_new$month[rain_training_new$rain>0.2]),rain_training_new$rain[rain_training_new$rain>0.2], xlab="month", ylab="precip", main="(b)")
plot(as.numeric(rain_training_new$month),rain_training_new$rain, xlab="Month", ylab="Precip", main="(b)")
quantmon <- rep(0, length(sort(unique(rain_training_new$month))))
for(j in 1:length(sort(unique(rain_training_new$month)))){
  quantmon[j] <- quantile(rain_training_new$rain[which(rain_training_new$month==sort(unique(rain_training_new$month))[j])], probs=0.998, na.rm=T)
}
#lines(sort(as.numeric(unique(rain_training_new$month))), quantmon, col="red", cex=2, lwd=2)
lines(sort(unique(rain_training_new$month)), quantmon, col="red", cex=2, lwd=2)

#longitude
plot(rain_training_new$lon[rain_training_new$rain>0.2],rain_training_new$rain[rain_training_new$rain>0.2], xlab="Longitude", ylab="Precip", main="(c)")
quantlon <- rep(0, length(sort(unique(rain_training_new$lon))))
for(j in 1:length(sort(unique(rain_training_new$lon)))){
  quantlon[j] <- quantile(rain_training_new$rain[which(rain_training_new$lon==sort(unique(rain_training_new$lon))[j])], probs=0.998, na.rm=T)
}
lines(sort(unique(rain_training_new$lon)), quantlon, col="red", cex=2, lwd=2)

#latitude
plot(rain_training_new$lat[rain_training_new$rain>0.2],rain_training_new$rain[rain_training_new$rain>0.2], xlab="Latitude", ylab="Precip", main="(d)")
quantlat <- rep(0, length(sort(unique(rain_training_new$lat))))
for(j in 1:length(sort(unique(rain_training_new$lat)))){
  quantlat[j] <- quantile(rain_training_new$rain[which(rain_training_new$lat==sort(unique(rain_training_new$lat))[j])], probs=0.998, na.rm=T)
}
lines(sort(unique(rain_training_new$lat)), quantlat, col="red", cex=2, lwd=2)
dev.off()
########################################
##Data manipulation Plot
########################################
library(xts)
##you should change data location
rain <- readRDS("~/Dropbox/Github/SNUmultiscale/Data/Rain.RDS")
##1994년 9월 1일부터 크게 줄어듬
par(mar=c(6.1,5.1,3.1,1.1)/1.25)
par(mfrow=c(2,2))
plot(seq(as.Date("1974-01-01"), as.Date("1979-12-31"), "day"), as.numeric(rain$data['1974-01-01/1979-12-31/',27]), xlab="Year", ylab="Precip", main="(a)")
## 31번 station(27번째 줄):
rain$data["/1975-07-22",27] <- NA
plot(seq(as.Date("1974-01-01"), as.Date("1979-12-31"), "day"), as.numeric(rain$data['1974-01-01/1979-12-31/',27]), xlab="Year", ylab="Precip", main="(b)")
plot(seq(as.Date("1977-01-01"), as.Date("1979-12-31"), "day"), as.numeric(rain$data['1977-01-01/1979-12-31/',4]), xlab="Year", ylab="Precip", main="(c)")
## 4번 station: 1977년부터 1979년까지 c(0.91, 0.94, 0.98)인 value들 NA로 제거
rain$data[1462+which(!is.na(match(rain$data["1977-01-01/1979-01-01",4], c(0.91,0.94,0.98)))),4] <- NA
plot(seq(as.Date("1977-01-01"), as.Date("1979-12-31"), "day"), as.numeric(rain$data['1977-01-01/1979-12-31/',4]),xlab="Year", ylab="Precip", main="(d)")


## na check
colSums(rain$data==0, na.rm=TRUE)
colSums(!is.na(rain$data))
colSums(rain$data==0, na.rm=TRUE)/colSums(!is.na(rain$data))

## 문제가 있는 station: 1번 station, 31번 station(27번째 줄), 32번 station(28번째 줄)
## 1번 station: 1987년 9월 2일부터 9월 6일까지
rain$data["1987-09-02/1987-09-06",1] <- NA
## 31번 station(27번째 줄):
rain$data["/1975-07-22",27] <- NA
## 32번 station(28번째 줄):
rain$data["/1995-04-29",28] <- NA

rain_training <- rain$data

########################################
##2번 장소의 July 쪽 출력
########################################
library(lubridate)

par(mar=c(5.1,4.1,4.1,2.1)/1)
par(mfrow=c(1,1))
boxplot(as.numeric(rain_training[,2]) ~ reorder(format(date(rain_training),'%m'),rain_training[,2]), outline = FALSE, main="station 2", ) 
boxplot(as.numeric(rain_training[,1]) ~ reorder(format(date(rain_training),'%m'),rain_training[,1]), outline = FALSE, main="station 1", ) 
boxplot(as.numeric(rain_training[,5]) ~ reorder(format(date(rain_training),'%m'),rain_training[,5]), outline = FALSE, main="station 5", ) 

########################################
##July point.plot
########################################
par(mar=c(5.1,4.1,4.1,2.1)/1)
par(mfrow=c(1,1))
TRUEresultJUL <- TRUEresult[TRUEresult$months=="Jul",]
point.plot(rain$place$stations.long[sort(union(as.numeric(C1),as.numeric(C2)))], rain$place$stations.lat[sort(union(as.numeric(C1),as.numeric(C2)))], TRUEresultJUL$prcp, main="July")
text(rain$place$stations.long[as.numeric(C1)], rain$place$stations.lat[as.numeric(C1)]-0.1, C1)
text(rain$place$stations.long[as.numeric(C2)], rain$place$stations.lat[as.numeric(C2)]-0.1, C2)

par(mar=c(5.1,4.1,4.1,2.1)/1)
par(mfrow=c(1,1))
TRUEresultJUN <- TRUEresult[TRUEresult$months=="Jun",]
point.plot(rain$place$stations.long[sort(union(as.numeric(C1),as.numeric(C2)))], rain$place$stations.lat[sort(union(as.numeric(C1),as.numeric(C2)))], TRUEresultJUN$prcp, main="June")
text(rain$place$stations.long[as.numeric(C1)], rain$place$stations.lat[as.numeric(C1)]-0.1, C1)
text(rain$place$stations.long[as.numeric(C2)], rain$place$stations.lat[as.numeric(C2)]-0.1, C2)

par(mar=c(5.1,4.1,4.1,2.1)/1)
par(mfrow=c(1,1))
TRUEresultAUG <- TRUEresult[TRUEresult$months=="Aug",]
point.plot(rain$place$stations.long[sort(union(as.numeric(C1),as.numeric(C2)))], rain$place$stations.lat[sort(union(as.numeric(C1),as.numeric(C2)))], TRUEresultAUG$prcp, main="August")
text(rain$place$stations.long[as.numeric(C1)], rain$place$stations.lat[as.numeric(C1)]-0.1, C1)
text(rain$place$stations.long[as.numeric(C2)], rain$place$stations.lat[as.numeric(C2)]-0.1, C2)

########################################
##가장 예측력이 높은 결과를 boxplot으로 표현해보기
########################################
VIP <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/middle100(500trees)mtry1minnodesize5(ranger).RDS")
VIPresulta <- c(); VIPresultb <- c()
for(i in 1:length(VIP)){
  VIPresulta <- cbind(VIPresulta, VIP[[i]]$v[,1])
  VIPresultb <- cbind(VIPresultb, VIP[[i]]$v[,2])
}
VIPresulta <- t(VIPresulta) #colnames(VIPresult) <- c("sinmonth","cosmonth","lat","lon")
VIPresultb <- t(VIPresultb)
par(mar=c(5.1,2.1,4.1,0.55)/1)
par(mfrow=c(1,2))
boxplot(VIPresulta, xlab="%IncMSE", main="(a)",cex.axis=0.95)
boxplot(VIPresultb, xlab="IncNodePurity",  main="(b)",cex.axis=0.95)
par(mfrow=c(1,1))
boxplot(VIPresulta, xlab="%IncMSE",cex.axis=0.95)

VIP <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/middle100(500trees)mtry1minnodesize5(ranger).RDS")
VIPresulta <- c();
for(i in 1:length(VIP)){
  VIPresulta <- rbind(VIPresulta, VIP[[i]]$v)
}
par(mar=c(5.1,2.1,4.1,0.55)/1)
par(mfrow=c(1,1))
boxplot(VIPresulta, xlab=expression(hat(I)(X[r])),cex.axis=0.95)


########################################
##추가: 월별로 계산해 보는 건 어떨까
########################################
#workdir <- "~/Dropbox/OnlineLearning/Challenge/Final"
workdir <- "~/Dropbox/EVA2017/AfterEVA/Me/New"
setwd(workdir)

prelem <- read.csv("middlem(mean)500trees1mtry5minnodesize(ranger).csv")

prelem3 <- read.csv("middlem(mean)500trees4mtry5minnodesize(ranger).csv")
#prelem2 <- read.csv("final(mean)500trees1mtry5minnodesize(1127).csv")
#prelem2 <- read.csv("middlem(meangrf)500trees1mtry5minnodesize(1019).csv")
#prelem2 <- read.csv("middlem(meannoncircular)500trees2mtry5minnodesize(1106).csv")
prelem2 <- read.csv("middlem(meannoncircular)500trees1mtry5minnodesize(ranger).csv")

# Score for challenge 1
score1=scoreb1=scorec1=scored1=matrix(0, nrow=length(st), ncol=length(months))
for (i in 1:length(st)) #st: station num
  for (j in 1:12)
  {station<-as.numeric(st[i])
  idx1 <- precip.testm$months == months[j]
  idx2 <- precip.testm$stations.num == station
  monthly<- as.numeric(precip.testm[idx1 & idx2,3])
  score <- apply(as.matrix(monthly),1,ql,y=as.numeric(prelem[as.numeric(prelem[,1])==station,j+1]))
  score1[i,j] <- sum(score,na.rm = TRUE)
  score <- apply(as.matrix(monthly),1,ql,y=benchmark[benchmark$stations==station,j+1])
  scoreb1[i,j] <- sum(score,na.rm = TRUE)
  score <- apply(as.matrix(monthly),1,ql,y=as.numeric(prelem2[as.numeric(prelem2[,1])==station,j+1]))
  scorec1[i,j] <- sum(score,na.rm = TRUE)
  score <- apply(as.matrix(monthly),1,ql,y=as.numeric(prelem3[as.numeric(prelem3[,1])==station,j+1]))
  scored1[i,j] <- sum(score,na.rm = TRUE)
  }
rownames(score1)=rownames(scoreb1)=rownames(scorec1)=rownames(scored1) <- st; colnames(score1)=colnames(scoreb1)=colnames(scorec1)=colnames(scored1) <- months

# Score for challenge 2
score2=scoreb2=scorec2=scored2=matrix(0, nrow=length(stations.test), ncol=length(months))
for (i in 1:length(stations.test))
  for (j in 1:12)
  {station<-as.numeric(stations.test[i])
  idx1 <- precip.testm$months == months[j]
  idx2 <- precip.testm$stations.num == station
  monthly<- as.numeric(precip.testm[idx1 & idx2,3])
  score <- apply(as.matrix(monthly),1,ql,y=as.numeric(prelem[as.numeric(prelem[,1])==station,j+1]))
  score2[i,j] <- sum(score,na.rm = TRUE)
  score <- apply(as.matrix(monthly),1,ql,y=benchmark2[benchmark2$stations==station,j+1])
  scoreb2[i,j] <- sum(score,na.rm = TRUE)
  score <- apply(as.matrix(monthly),1,ql,y=as.numeric(prelem2[as.numeric(prelem2[,1])==station,j+1]))
  scorec2[i,j] <- sum(score,na.rm = TRUE)
  score <- apply(as.matrix(monthly),1,ql,y=as.numeric(prelem3[as.numeric(prelem3[,1])==station,j+1]))
  scored2[i,j] <- sum(score,na.rm = TRUE)
  }

rownames(score2)=rownames(scoreb2)=rownames(scorec2)=rownames(scored2) <- stations.test; colnames(score2)=colnames(scoreb2)=colnames(scorec2)=colnames(scored2) <- months


###### plotting
#print(c(score1,score2))
#Ratios
#print(-c((score1-scoreb1)/scoreb1,(score2-scoreb2)/scoreb2))

plotscore1a <- -(colSums(score1)-colSums(scoreb1))/colSums(scoreb1)
plotscore2a <- -(colSums(score2)-colSums(scoreb2))/colSums(scoreb2)
plotscore1c <- -(colSums(scorec1)-colSums(scoreb1))/colSums(scoreb1)
plotscore2c <- -(colSums(scorec2)-colSums(scoreb2))/colSums(scoreb2)
plotscore1d <- -(colSums(scored1)-colSums(scoreb1))/colSums(scoreb1)
plotscore2d <- -(colSums(scored2)-colSums(scoreb2))/colSums(scoreb2)

#plot(plotscore2a, type='o', pch=16, ylim=range(c(plotscore1a,plotscore1c)))
#lines(plotscore2c, type='o', pch=16, col="blue")
#abline(h=0, lty=2, col="red")

dedata <- cbind(plotscore2a, plotscore2c, plotscore2d)

par(mar=c(5.1,4.1,4.1,2.1)/1)
#mydata <- data.frame(Barplot1=rbinom(5,16,0.6), Barplot2=rbinom(5,16,0.25),
#                     Barplot3=rbinom(5,5,0.25), Barplot4=rbinom(5,16,0.7))
barplot(t(as.matrix(100*dedata)), main="", ylab="Prediction score(%)", beside=TRUE, col=rainbow(3))
legend(x=35,y=70, c("CQRF (mtry=1)","LQRF (mtry=1)","CQRF (mtry=4)"), cex=1, fill=rainbow(3), text.width = 11)






plotscore1a <- -(rowSums(score1)-rowSums(scoreb1))/rowSums(scoreb1)
plotscore2a <- -(rowSums(score2)-rowSums(scoreb2))/rowSums(scoreb2)
plotscore1c <- -(rowSums(scorec1)-rowSums(scoreb1))/rowSums(scoreb1)
plotscore2c <- -(rowSums(scorec2)-rowSums(scoreb2))/rowSums(scoreb2)
plotscore1d <- -(rowSums(scored1)-rowSums(scoreb1))/rowSums(scoreb1)
plotscore2d <- -(rowSums(scored2)-rowSums(scoreb2))/rowSums(scoreb2)

plot(plotscore2a, type='o', pch=16, ylim=range(c(plotscore1a,plotscore1c)))
lines(plotscore2c, type='o', pch=16, col="blue")
abline(h=0, lty=2, col="red")


##station별로 - 패턴 딱히 없음
#par(mfrow=c(1,1))
#par(mar=c(5.1,4.1,4.1,2.1)/1)
#plot(plotscore1a[1,], ylim=range(plotscore1a), type="l")
#for(i in 2:12){
#  lines(plotscore1a[i,])
#}

rain <- readRDS("~/Dropbox/Github/SNUmultiscale/Data/Rain.RDS")


library(plotly)

dfdata <- cbind(rain$place[as.numeric(stations.test),], plotscore2a, plotscore2c, plotscore2d)
#plot_ly(dfdata, x=dfdata[,3], y=dfdata[,2])
par(mfrow=c(1,1))
par(mar=c(4.1,4.1,1.1,1.1)/1)
#plotly_empty(x=dfdata[,3], y=dfdata[,2], xlab="longitude", ylab="latitude")
plot(x=dfdata[,3], y=dfdata[,2], xlab="Longitude", ylab="Latitude", type="n")
library(TeachingDemos)
#library(plotly)
for(i in 1: nrow(dfdata)){
  if(i==9){
    subplot(barplot(height = as.numeric(as.character(unlist(dfdata[(i+1), 4:6], use.names=F))), axes=F, col=rainbow(3), width=0.05, xlim=c(0,0.4), ylim=c(-0.5, 1) ), x=dfdata[(i+1),3], y=dfdata[(i+1),2], size=c(0.6,0.6,0.6))
  }else if(i==10){
    subplot(barplot(height = as.numeric(as.character(unlist(dfdata[(i-1), 4:6], use.names=F))), axes=F, col=rainbow(3), width=0.05, xlim=c(0,0.4), ylim=c(-0.5, 1) ), x=dfdata[(i-1),3], y=dfdata[(i-1),2], size=c(0.6,0.6,0.6))
  }else{
    subplot(barplot(height = as.numeric(as.character(unlist(dfdata[i, 4:6], use.names=F))), axes=F, col=rainbow(3), width=0.05, xlim=c(0,0.4), ylim=c(-0.5, 1) ), x=dfdata[i,3], y=dfdata[i,2], size=c(0.6,0.6,0.6))
  }
}
par(las=1)
library(scales)
rect(xleft=4.0, xright=4.75, ytop=47.25, ybottom=46.75, border=T)
subplot(barplot(height = 100*as.numeric(as.character(unlist(dfdata[26, 4:6], use.names=F))), axes=T, beside = F, col=rainbow(3), width=0.05, xlim=c(0,0.4), ylim=c(-100*1, 100*1), ylab="%",  cex.axis = 0.75), x=4.674, y=46.992, size=c(0.6,0.6,0.6))

legend("topleft", legend=c("CQRF (mtry=1)","LQRF (mtry=1)","CQRF (mtry=4)"), fill=rainbow(3), text.width = 0.75)
xtab = c(rep(0.05, 4), rep(-0.1,3), rep(0.05,27))
ytab = c(rep(-0.05,3), rep(-0.05,1), rep(-0.15,3),rep(0.05,1),rep(-0.1,1), rep(0.05,1),rep(-0.05,24))
text(dfdata$stations.long[-c(5:9, 31)]+xtab[-c(5:9, 31)], dfdata$stations.lat[-c(5:9, 31)]+ytab[-c(5:9, 31)], dfdata$X[-c(5:9, 31)], font=1)
text(dfdata$stations.long[c(5:9, 31)]+xtab[c(5:9, 31)], dfdata$stations.lat[c(5:9, 31)]+ytab[c(5:9, 31)], dfdata$X[c(5:9, 31)], font=4, cex = 1.5)
par(las=0)








library(plotly)

dfdata <- cbind(rain$place[as.numeric(stations.test),], plotscore2a, plotscore2c, plotscore2d)
#plot_ly(dfdata, x=dfdata[,3], y=dfdata[,2])
par(mfrow=c(1,1))
par(mar=c(4.1,4.1,1.1,1.1)/1)
#plotly_empty(x=dfdata[,3], y=dfdata[,2], xlab="longitude", ylab="latitude")
plot(x=dfdata[,3], y=dfdata[,2], xlab="Longitude", ylab="Latitude", xlim=c(0.85, 4.9), ylim=c(46.5, 49.5),  type="n")
library(TeachingDemos)
#library(plotly)
for(i in 1: nrow(dfdata)){
  if(i==9){
    subplot(barplot(height = as.numeric(as.character(unlist(dfdata[(i+1), 4:6], use.names=F))), axes=F, col=rainbow(3), width=0.05, xlim=c(0,0.4), ylim=c(-0.5, 1) ), x=dfdata[(i+1),3], y=dfdata[(i+1),2], size=c(0.6,0.6,0.6))
  }else if(i==10){
    subplot(barplot(height = as.numeric(as.character(unlist(dfdata[(i-1), 4:6], use.names=F))), axes=F, col=rainbow(3), width=0.05, xlim=c(0,0.4), ylim=c(-0.5, 1) ), x=dfdata[(i-1),3], y=dfdata[(i-1),2], size=c(0.6,0.6,0.6))
  }else{
    subplot(barplot(height = as.numeric(as.character(unlist(dfdata[i, 4:6], use.names=F))), axes=F, col=rainbow(3), width=0.05, xlim=c(0,0.4), ylim=c(-0.5, 1) ), x=dfdata[i,3], y=dfdata[i,2], size=c(0.6,0.6,0.6))
  }
}
par(las=1)
library(scales)
rect(xleft=3.85, xright=4.8, ytop=47.15, ybottom=46.55, border=T)
subplot(barplot(height = 100*as.numeric(as.character(unlist(dfdata[26, 4:6], use.names=F))), axes=T, beside = F, col=rainbow(3), width=0.05, xlim=c(0,0.4), ylim=c(-100*1, 100*1), ylab="%",  cex.axis = 0.75), x=4.604, y=46.842, size=c(0.6,0.6,0.6))

legend(x=0.85, y=49.475, legend=c("CQRF (mtry=1)","LQRF (mtry=1)","CQRF (mtry=4)"), fill=rainbow(3), text.width = 0.9)
xtab = c(rep(0.05, 4), rep(-0.1,3), rep(0.05,27))
ytab = c(rep(-0.05,3), rep(-0.05,1), rep(-0.15,3),rep(0.05,1),rep(-0.1,1), rep(0.05,1),rep(-0.05,24))
text(dfdata$stations.long[-c(5:9, 31)]+xtab[-c(5:9, 31)], dfdata$stations.lat[-c(5:9, 31)]+ytab[-c(5:9, 31)], dfdata$X[-c(5:9, 31)], font=1, cex = 0.8)
text(dfdata$stations.long[c(5:9, 31)]+xtab[c(5:9, 31)], dfdata$stations.lat[c(5:9, 31)]+ytab[c(5:9, 31)], dfdata$X[c(5:9, 31)], font=4, cex = 1)
par(las=0)




