#' sample
#'
#' @param n = number of sample
#' @examples lottosample(5)
#' (k<-sort(num_per()[as.numeric(data[1,1:6])],dec=T))
#' lottosample(n=5,keep=k[1])
#' @return
#' @export
lottosample<-function(n,keep=NULL,drop=NULL,hold=NULL,a=NULL,write=F){
  if(length(hold)==0){for(i in 1:n){a<-rbind(a,sort(sample(1:45,6,prob=del(keep=keep,drop=drop))))}
  }else {for(i in 1:n){a<-rbind(a,sort(c(hold,sample(1:45,(6-length(hold)),prob=del(keep=keep,drop=unique(c(hold,drop)))))))}
  }
  output<-cbind(a,data[1,7]+1)
  colnames(output)<-c('V1',"V2","V3","V4","V5","V6","n")

  if(write==T)write.csv(output,'lotto.csv')

  return(output)}
#lottosample(100)

#setwd('C:/Users/qkdrk/Desktop/새 폴더')
#lottosample(1000,write=T)
