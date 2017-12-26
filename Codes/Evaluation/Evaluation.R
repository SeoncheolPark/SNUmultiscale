getwd()
workdir <- "~/Dropbox/EVA2017/AfterEVA"
setwd(workdir)

library(lubridate)
library(chron)


# Create the benchmark, the monthly maxima

precip.sample<-read.csv("precip_sample.csv", row.names=1)


precip.sample$dts<-dates(as.character(precip.sample$dts),format="m/d/y")
months<-months(precip.sample$dts)
precip.m<-data.frame(precip.sample,months)
stations<-levels(as.factor(precip.m$stations.num))
months<- levels(as.factor(precip.m$months))
nst<-length(stations)
monthlymax=matrix(nrow=nst,ncol=12)

for (i in 1:nst)
  for (j in 1:12)
  {station<-as.numeric(stations[i])
  idx1 <- precip.m$months == months[j] #이부분 다름
  idx2 <- precip.m$stations.num == station
  monthly<- precip.m[idx1 & idx2,]
  monthlymax[i,j]=max(monthly$prcp,na.rm = TRUE)}

monthlymax<-data.frame(stations,monthlymax)

# Create the test sample

precip.test<-read.csv("precip_test.csv", row.names=1)
precip.test$dts<-dates(as.character(precip.test$dts),format="m/d/y")
months<-months(precip.test$dts)

# Clean the test sample by removing suspicious 0 on station 7(추가됨)

dts7<-precip.test$dts[precip.test$stations.num==7 & precip.test$prcp>0]
is.na(precip.test$prcp[precip.test$stations.num==7])<-(precip.test$dts[precip.test$stations.num==7]>max(dts7))
precip.testm<-data.frame(precip.test,months)
stations.test<-levels(as.factor(precip.test$stations.num))
months<- levels(as.factor(precip.m$months))

#plot(precip.test$prcp[precip.test$stations.num=="7"])
#plot(precip.testm$prcp[precip.test$stations.num=="7"])

# Random subsample test

set.seed(1)
levels(years(precip.test$dts))
subsamp<-sample(1996:2015,5)
precip.test1<-precip.testm[years(precip.test$dts)%in%subsamp,]

# Common stations in the training and test

id2 <- rep(0, length(stations))
for (i in 1: length(stations.test))
{id2 <-(stations==stations.test[i])+id2}
id3<-which(id2==1)
st<-stations[id2==1]
benchmark<-monthlymax[id3,]

# Benchmark 2

mmax<-colMeans(as.matrix(monthlymax[,2:13]))
id=rep(0,length(stations.test))
for (i in 1:length(stations))
{id=(stations.test==stations[i])+id}
stations.new<-stations.test[id==0]
repmax<-rep(mmax,length(stations.new))
benchmark.new<-data.frame(stations=stations.new,matrix(repmax,nrow=length(stations.new),byrow = TRUE))
benchmark2<-rbind(benchmark,benchmark.new)

# Evaluation



# Quantile loss

ql <- function(x,y,alpha=0.998)
{alpha*(x-y)*(x>y)+(1-alpha)*(y-x)*(y>=x)}

# Score for benchmark

scoreb1=0
for (i in 1:length(st))
  for (j in 1:12)
  {station<-as.numeric(st[i])
  idx1 <- precip.testm$months == months[j]
  idx2 <- precip.testm$stations.num == station
  monthly<- as.numeric(precip.testm[idx1 & idx2,3])
  score <- apply(as.matrix(monthly),1,ql,y=benchmark[benchmark$stations==station,j+1])
  scoreb1 <- scoreb1+sum(score,na.rm = TRUE)
  }



# Score for benchmark 2

scoreb2=0
for (i in 1:length(stations.test))
  for (j in 1:12)
  {station<-as.numeric(stations.test[i])
  idx1 <- precip.testm$months == months[j]
  idx2 <- precip.testm$stations.num == station
  monthly<- as.numeric(precip.testm[idx1 & idx2,3])
  score <- apply(as.matrix(monthly),1,ql,y=benchmark2[benchmark2$stations==station,j+1])
  scoreb2 <- scoreb2+sum(score,na.rm = TRUE)
  }

print(c(scoreb1,scoreb2))

# write.table(benchmark,file="benchmark1.txt",sep='&',row.names = FALSE,eol='\\')
# write.table(benchmark2,file="benchmark2.txt",sep='&',row.names = FALSE,eol='\\')
# View(benchmark2)

# Score for preleminary

#workdir <- "~/Dropbox/OnlineLearning/Challenge/Final"
workdir <- "~/Dropbox/EVA2017/AfterEVA/Me/New"
setwd(workdir)

prelem <- read.csv("middlem(mean)500trees1mtry20minnodesize(ranger).csv")
#prelem<-read.table("Huser.csv",sep = "",row.names = NULL)
prelem <- read.csv("SNU.csv")
#prelem[,c(2:13)] <- b2

#View(prelem)

# Score for challenge 1

score1=0
for (i in 1:length(st))
  for (j in 1:12)
  {station<-as.numeric(st[i])
  idx1 <- precip.testm$months == months[j]
  idx2 <- precip.testm$stations.num == station
  monthly<- as.numeric(precip.testm[idx1 & idx2,3])
  score <- apply(as.matrix(monthly),1,ql,y=as.numeric(prelem[as.numeric(prelem[,1])==station,j+1]))
  score1 <- score1+sum(score,na.rm = TRUE)
  }


# Score for challenge 2

score2=0
for (i in 1:length(stations.test))
  for (j in 1:12)
  {station<-as.numeric(stations.test[i])
  idx1 <- precip.testm$months == months[j]
  idx2 <- precip.testm$stations.num == station
  monthly<- as.numeric(precip.testm[idx1 & idx2,3])
  score <- apply(as.matrix(monthly),1,ql,y=as.numeric(prelem[as.numeric(prelem[,1])==station,j+1]))
  score2 <- score2+sum(score,na.rm = TRUE)
  }

if(is.infinite(as.matrix(prelem)) || is.na(prelem)){
  score1 <- Inf
  score2 <- Inf}

if(!identical(sort(as.integer(prelem[,1])), sort(as.integer(stations.test)))){
  score1 <- Inf
  score2 <- Inf}

print(c(score1,score2))

#Ratios


print(-c((score1-scoreb1)/scoreb1,(score2-scoreb2)/scoreb2))


