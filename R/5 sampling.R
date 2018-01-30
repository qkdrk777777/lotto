#' sample
#'
#' @param n = number of sample
#' @examples lottosample(5)
#' @return
#' @export
lottosample<-function(n,keep=NULL,drop=NULL){
for(i in 1:n)  print(sort(sample(1:45,6,prob=del(keep=keep,drop=drop))))}
