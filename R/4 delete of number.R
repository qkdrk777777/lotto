#' delete of number
#'
#' @return
#' @export
del<-function(n=100,tol=1,keep=NULL,drop=NULL){
  per<-num_per()/num_per(data=data_update2()[1:n,])
  (del_num<-as.numeric(names(per[per<tol])))
  temp<-c(setdiff(del_num,keep),drop)
  temp<-temp[order(temp)]
  num<-num_per()
num[temp]<-0
return(num)}

