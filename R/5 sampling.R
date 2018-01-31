#' sample
#'
#' @param n = number of sample
#' @examples lottosample(5)
#' (k<-sort(num_per()[as.numeric(data[1,1:6])],dec=T))
#' lottosample(n=5,keep=k[1])
#' @return
#' @export
lottosample<-function(n,keep=NULL,drop=NULL,hold=NULL,a=NULL){
  if(length(hold)==0){for(i in 1:n){a<-rbind(a,sort(sample(1:45,6,prob=del(keep=keep,drop=drop))))}
  }else {for(i in 1:n){a<-rbind(a,sort(c(hold,sample(1:45,(6-length(hold)),prob=del(keep=keep,drop=unique(c(hold,drop)))))))}
  }
  return(a)}



#repsample(10000,count=3)
