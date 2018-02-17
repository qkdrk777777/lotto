#' lotto
#'
#' @param n = The number of times the sample is pulled
#' @examples lot(1000)
#' @return
#' @export
lot<-function(n,a=NULL,head=10,write=F){
b<-a
num<-num_per()/num_per(data=data_update2()[1:100,])
if(!require(progress))install.packages('progress')
Sys.sleep(0.01)
library(progress)
pb<-progress_bar$new(total=n)
for(i in 1:n){
    a<-c(sort(sample(1:45,6,prob=round(num_per()*num,10))),data[1,7]+1,1)
  if(length(b)==0)b<-rbind(b,a)else{
for(j in 1:nrow(b)-1){
if(setequal(b[j,1:6],a[1:6]))b[j,8]<-b[j,8]+1}
b<-rbind(b,a)}
pb$tick()
Sys.sleep(0.001)
}
for(i in c(6:1,8)){if(i==8) b<-b[order(b[,i],decreasing = T),] else b<-b[order(b[,i]),]}
rownames(b)<-1:nrow(b)
colnames(b)<-c('x1',"x2","x3","x4","x5","x6","times",'count')
if(length(head)!=0)output<-head(b,head) else output<-b
if(write==T)write.csv(output,'lotto.csv')
return(output)
}

