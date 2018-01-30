#' generate lottodata
#'
#' @return lotto data
#' @examples
#' devtools::install_github("qkdrk777777/DUcj")
#'
#' data<-data_update()
#' @export
data_update<-function(){
  library(DUcj)
  package(rvest)
  package(stringr)
url2<-'https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query=%EB%A1%9C%EB%98%90&oquery=%EB%A1%9C%EB%98%90&tqi=TmrKidpVuFdsssc0EvVssssssUd-075322'
line2<-read_html(url2,encoding="UTF-8")
p1<-html_nodes(line2,css='._lotto-btn-current em')%>%html_text()
lastRaffle<-as.numeric(str_replace_all(p1,"[^0-9]",""))


#data<-read.csv("D:/packages/lotto/lotto.csv",header=T,row.names = 1)
if(data[1,7]==nrow(data)){
if(data[1,7]!=lastRaffle)
  {data2<-numdata(a,b,first=data[1,7],last=lastRaffle)
  out<-rbind(data2,data)}
  else{out<-data}
}else {out<-numdata(a,b,last=lastRaffle) }
#write.csv(out,"D:/packages/lotto/lotto.csv")
out
}
#라이브러리에 데이터 저장하는법
#devtools::use_data(data, internal = TRUE)
