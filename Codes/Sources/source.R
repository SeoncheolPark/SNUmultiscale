library(xts)
########################################
##Loss function
########################################
##qlf: quantile loss function, 문서의 l에 해당
qlf <- function(x,y,alpha=0.998){
  #x,y: data, alpha: quantile
  if(is.na(x)==TRUE | is.na(y)==TRUE){
    return(NA)
  }else if(x>y){
    return(alpha*(x-y))
  }else{
    return((1-alpha)*(y-x))
  }
}

##sqlf: sum of quantile losses over the stations and months
sqlf <- function(P, q){
  #q: 특정 장소의 특정 달의 내가 예측한 quantile값: 12*장소꼴
  #P: test data이며 (시계열*장소) 형태로 받겠음, 즉 결국 xts꼴 데이터여야 함
  monthindex <- data.frame(month=format(index(P),"%m"))
  #output: matrix이며 row는 1월부터 12월, column은 관찰장소들이 되어야 함
  dailyloss <- c()
  for(i in 1:nrow(P)){
    dailyloss <- rbind(dailyloss, mapply(function(x,y) qlf(x,y), P[i,], q[as.numeric(format(index(P[i,]),"%m")),]))
  }
  monthlyloss <- matrix(0, nrow=12, ncol=(ncol(dailyloss)))
  colnames(monthlyloss) <- colnames(P); rownames(monthlyloss) <- unique(monthindex$month)
  for(j in 1:length(unique(monthindex$month))){
    monthlyloss[j,] <- colSums(dailyloss[which(monthindex==as.character(unique(monthindex$month)[j])),], na.rm=TRUE) 
  }
  monthlyloss
}
