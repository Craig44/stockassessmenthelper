#' Generate a trace plot, used for MCMC convergence criteria.
#'
#' @param x a numeric vector to plot the trace for.
#' @param ylim y-axis upper and lower value
#' @param m   what interval to calculate the running mean, quantiles over
#' @param p not sure what this does.
#' @return Trace plot
#' @export


plot.trace <- function(x,ylim=NULL,m=100,type="p",xlab="Iterations",ylab="",title=""){
  
  if(is.null(ylim)) ylim=range(x)
  plot(x,pch=" ",ylim=ylim,type = type,xlab=xlab,ylab=ylab,las=1)
  if(type=="p") points(x,pch=20,col="gray",)
  if(type=="l") lines(x,pch=20,col="gray",)   
  
  j1<-m%/%2
  j2<-length(x)-j1
  nn<-NULL
  for(j in (j1+1):j2) nn<-rbind(nn,c(j, mean(x[(j-j1):(j+j1)])))
  
  lines(nn[,1],nn[,2],lwd=2,col="red")
  
  if(type=="p") {
    nn<-NULL
    r<-seq(j1,length(x),50)
    for (j in 1:length(r)) nn <- rbind(nn,c(r[j],quantile(x[1:r[j]],0.05)))
    lines(nn[,1],nn[,2],lwd=1,col="blue")

    nn<-NULL
    r<-seq(j1,length(x),50)
    for (j in 1:length(r)) nn <- rbind(nn,c(r[j],quantile(x[1:r[j]],0.95)))
    lines(nn[,1],nn[,2],lwd=1,col="blue")

    nn<-NULL
    r<-seq(j1,length(x),50)
    for (j in 1:length(r)) nn <- rbind(nn,c(r[j],quantile(x[1:r[j]],0.5)))
    lines(nn[,1],nn[,2],lwd=1,col="blue")
    
  }
  mtext(title,line=0.1,adj=0.5,cex = 1)
}
