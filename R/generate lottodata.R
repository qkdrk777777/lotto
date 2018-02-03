#' generate lottodata
#'
#' @param t = url of list form
#' @param css = Nodes to select. Supply one of css or xpath depending on whether you want to use a css or xpath 1.0 selector.
#' @return
#' @examples
#' a='https://search.naver.com/search.naver?sm=tab_drt&where=nexearch&query='
#' b='%ED%9A%8C%EB%A1%9C%EB%98%90'
#' url<-'https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query=%EB%A1%9C%EB%98%90&oquery=%EB%A1%9C%EB%98%90&tqi=TmrKidpVuFdsssc0EvVssssssUd-075322'
#'line<-read_html(url,encoding="UTF-8")
#'p1<-html_nodes(line,css='._lotto-btn-current em')%>%html_text()
#'last=as.numeric(substr(p1,1,3))
#' numdata(a,b,last=last)
#' @export
numdata<-function(a,b,first=1,last,css='.num_box',encoding='UTF-8',num=list(),output=list(),k=1)
{ package('XML');  package('stringr');  package('rvest')
  for(i in last:first){

    url<-paste0(a,i,b)
    lines<-read_html(url,encoding=encoding)
    keep<-html_nodes(lines,css=css)%>% html_text()

    output[[k]]<-c(as.numeric(strsplit(str_trim(keep),split=" ")[[1]][1:6]),i)
    k=k+1
    Sys.sleep(1)}
  output<-matrix(unlist(output),ncol=7,byrow=T)
  colnames(output)=c('x1','x2','x3','x4','x5','x6','times')
  rownames(output)=output[,7]
  return(output)}
#numdata(a,b,first=790,last=792)


