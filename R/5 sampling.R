#' sample
#'
#' @param n = number of sample
#' @examples lottosample(5)
#' (k<-sort(num_per()[as.numeric(data[1,1:6])],dec=T))
#' lottosample(n=5,keep=k[1])
#' @return
#' @export
lottosample<-function(n,keep=NULL,drop=NULL,a=list()){
for(i in 1:n){a[[i]]<-sort(sample(1:45,6,prob=del(keep=keep,drop=drop)))
print(a[[i]])}
  return(a)}

