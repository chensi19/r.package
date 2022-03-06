#' Title
#'
#' @param g
#' @param h precision
#' @param b0
#' @param b1
#' @return
#' @export
#'
#' @examples
bisec<-function(g,h=.05,b0,b1){
  f<-function(x,h=h){
    (g(x+h)-g(x-h))/2/h
  }
  eps<-.Machine$double.eps
  r<-seq(b0,b1,length=3)
  y<-mapply(f,x=r,h=h)
  if(y[1]*y[3]>0){
    stop("f dose not have opposite sign at end points")
  }
  it<-0
  while(it<1000&abs(y[2])>eps){
    it<-it+1
    if(y[1]*y[2]<0){
      r[3]<-r[2]
      y[3]<-y[2]
    }else{
      r[1]<-r[2]
      y[1]<-y[2]
    }
    r[2]<-(r[1]+r[3])/2
    y[2]<-f(r[2],h=h)
    print(c(r[2],y[2],y[3]-y[2]))
  }
  return(c(root=r[2],est=y[2],tol=y[3]-y[2]))
}
