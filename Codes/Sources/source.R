########################################
##Loss function
########################################
##qlf: quantile loss function
qlf <- function(x,y,alpha=0.998){
  #x,y: data, alpha: quantile
  if(x>y){
    return(alpha*(x-y))
  }else{
    return((1-alpha)*(y-x))
  }
}

##slf: sum of quantile losses on the special month
slf <- function(){
  
}

##sqlf: sum of quantile losses over the stations and months
sqlf1 <- function(){
  
}

sqlf2 <- function(){
  
}

sqlf <- function(sqlf1, sqlf2){
  return(sqlf1+sqlf2)
}