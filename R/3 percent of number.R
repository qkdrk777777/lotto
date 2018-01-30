#' Probability of a number
#'
#' @export
num_per<-function(data2=data_update2(),num_per=NULL){
  for(i in 1:45)num_per<-c(num_per,sum(data2[,i])/(6*nrow(data2)))
  names(num_per)=1:45
  num_per}



#library(roxygen2)
#library(devtools)
#devtools::document()
