#' sample
#'
#' @param n = number of sample
#' @examples lottosample(5)
#' (k<-sort(num_per()[as.numeric(data[1,1:6])],dec=T))
#' lottosample(n=5,keep=k[1])
#' @return
#' @export
lottosample<-function(n,keep=NULL,drop=NULL,a=NULL,hold=NULL){
if(length(hold)==0)
{for(i in 1:n){b<-sort(sample(1:45,6,prob=del(keep=keep,drop=drop)))
a<-rbind(a,b)
print(b)}}else {for(i in 1:n){b<-sort(c(hold,sample(1:45,6-length(hold),prob=del(keep=keep,drop=unique(c(hold,drop))))))
  a<-rbind(a,b)
  print(b)}}
  return(a)}

lottosample(hold=3,drop=3,n=50)
