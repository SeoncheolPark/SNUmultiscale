library(robustbase) #for colMedians function

middle <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/middle100(500trees)mtry1minnodesize20(ranger).RDS")
#middle <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/middle100(1000treesgrf).RDS")

##중간 1 자료 만들기
a1 <- matrix(0, nrow=nrow(middle[[1]]$r), ncol=ncol(middle[[1]]$r)); a2 <- matrix(0, nrow=nrow(middle[[1]]$r), ncol=ncol(middle[[1]]$r))
colnames(a1)=colnames(a2)=c(1:12); rownames(a1)=rownames(a2)=rownames(middle[[1]]$r)
for(j in 1:nrow(middle[[1]]$r)){
  middlematrix_s1 <-  matrix(0, nrow=length(middle), ncol=ncol(middle[[1]]$r))
  for(i in 1:nrow(middlematrix_s1)){
    middlematrix_s1[i,] <- middle[[i]]$r[j,]
  }
  a1[j,] <- colMedians(middlematrix_s1); a2[j,] <- colMeans(middlematrix_s1)
}
setwd("~/Dropbox/EVA2017/AfterEVA/Me/New")
write.csv(a2, "middlem(mean)500trees1mtry20minnodesize(ranger).csv")
#write.csv(a1, "middlem(mediangrf)500trees4mtry20minnodesize(efron).csv")

##기말류 weight matrix 만들기
wfinal01 <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/weightmodel1final100(500trees)mtry4minnodesize20.RDS")
wfinal02 <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/weightmodel2final100(500trees)mtry4minnodesize20.RDS")
wfinal03 <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/weightmodel3final100(500trees)mtry4minnodesize20.RDS")
middlenew=middlenew2=middlenew3=numNA <- matrix(0, nrow=40, ncol=12); 
for(i in 1:100){
  #middlenew <- matrix(mapply(sum,middlenew, (1/wfinal01[[i]]$l)/((1/wfinal01[[i]]$l)+(1/wfinal02[[i]]$l)+(1/wfinal03[[i]]$l)), na.rm=TRUE), ncol=12)
  #middlenew2 <- matrix(mapply(sum,middlenew2, (1/wfinal02[[i]]$l)/((1/wfinal01[[i]]$l)+(1/wfinal02[[i]]$l)+(1/wfinal03[[i]]$l)), na.rm=TRUE), ncol=12)
  #middlenew3 <- matrix(mapply(sum,middlenew3, (1/wfinal03[[i]]$l)/((1/wfinal01[[i]]$l)+(1/wfinal02[[i]]$l)+(1/wfinal03[[i]]$l)), na.rm=TRUE), ncol=12)
  middlenew <- matrix(mapply(sum,middlenew, wfinal01[[i]]$l, na.rm=TRUE), ncol=12)
  middlenew2 <- matrix(mapply(sum,middlenew2, wfinal02[[i]]$l, na.rm=TRUE), ncol=12)
  middlenew3 <- matrix(mapply(sum,middlenew3, wfinal03[[i]]$l, na.rm=TRUE), ncol=12)
  numNA <- matrix(mapply(sum,numNA, is.na(wfinal01[[i]]$l), na.rm=TRUE), ncol=12)
}
A <- middlenew/(100-numNA); A[is.nan(A)] <- mean(middlenew/(100-numNA), na.rm=T)
B <- middlenew2/(100-numNA); B[is.nan(B)] <- mean(middlenew2/(100-numNA), na.rm=T)
C <- middlenew3/(100-numNA); C[is.nan(C)] <- mean(middlenew3/(100-numNA), na.rm=T)
D <- (1/A)/((1/A)+(1/B)+(1/C))
E <- (1/B)/((1/A)+(1/B)+(1/C))
G <- (1/C)/((1/A)+(1/B)+(1/C))
weightmatrix <- cbind(D,E,G); colnames(weightmatrix) <- rep(c(1:12),3)
saveRDS(weightmatrix,"~/Dropbox/EVA2017/AfterEVA/Data/New/wmatrix/weightfinal100(500trees)mtry4minnodesize20.RDS")


##기말류 자료 만들기
weightmatrix <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/wmatrix/weightfinal100(500trees)mtry4minnodesize20.RDS") #wmatrix 폴더에서 불러올 것
final1 <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/1final100(500trees)mtry4minnodesize20.RDS")
final2 <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/2final100(500trees)mtry4minnodesize20.RDS")
final3 <- readRDS("~/Dropbox/EVA2017/AfterEVA/Data/New/3final100(500trees)mtry4minnodesize20.RDS")
#finalmatrix_s2 <- matrix(0, nrow=length(final2), ncol=ncol(final2[[1]]))
#finalmatrix_s3 <- matrix(0, nrow=length(final3), ncol=ncol(final3[[1]]))
#for(i in 1:nrow(finalmatrix_s2)){
#  finalmatrix_s2[i,] <- final2[[i]][2,]
#  finalmatrix_s3[i,] <- final3[[i]][2,]
#}
#colnames(finalmatrix_s2)=colnames(finalmatrix_s3) <- c(1:12)
#finalfinalmatrix_s1 <- middlematrix_s1*weightmatrix[1,c(1:12)] + finalmatrix_s2*weightmatrix[1,c(13:24)] + finalmatrix_s3*weightmatrix[1,c(25:36)]

b1 <- matrix(0, nrow=nrow(final1[[1]]$r), ncol=ncol(final1[[1]]$r)); b2 <- matrix(0, nrow=nrow(final1[[1]]$r), ncol=ncol(final1[[1]]$r))
colnames(b1)=colnames(b2)=c(1:12); rownames(b1)=rownames(b2)=rownames(final1[[1]]$r)
for(j in 1:nrow(final1[[1]]$r)){
  finalmatrix_s1 <- matrix(0, nrow=length(final1), ncol=ncol(final1[[1]]$r))
  finalmatrix_s2 <- matrix(0, nrow=length(final2), ncol=ncol(final2[[1]]$r))
  finalmatrix_s3 <- matrix(0, nrow=length(final3), ncol=ncol(final3[[1]]$r))
  for(i in 1:nrow(finalmatrix_s1)){
    finalmatrix_s1[i,] <- final1[[i]]$r[j,]
    finalmatrix_s2[i,] <- final2[[i]]$r[j,]
    finalmatrix_s3[i,] <- final3[[i]]$r[j,]
  }
  finalfinalmatrix_s1 <- finalmatrix_s1*weightmatrix[1,c(1:12)] + finalmatrix_s2*weightmatrix[1,c(13:24)] + finalmatrix_s3*weightmatrix[1,c(25:36)]
  b1[j,] <- colMedians(finalfinalmatrix_s1); b2[j,] <- colMeans(finalfinalmatrix_s1)
}
selectrow <- c(2,  4,  5,  6, 11, 12, 13, 15, 16, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 32, 33, 34, 35, 36, 38, 39, 40,  7,  8,  9, 10, 37)
b1 <- b1[selectrow,]; b2 <- b2[selectrow,]
setwd("~/Dropbox/EVA2017/AfterEVA/Me/New")
write.csv(b2, "final(mean)500trees4mtry20minnodesize(1201).csv")
#write.csv(b1, "final(median)500trees1mtry5minnodesize(1127).csv")