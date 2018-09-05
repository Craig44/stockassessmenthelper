#' Utility functions for generic maths, mostly ignoring NA's
#'
#' @author Craig Marsh
#' @export
#'

geometric.mean <- function(x) exp(sum(log(x))/length(x))

#' @export
Mean = function(...,na.rm = T) {median(..., na.rm = na.rm)}

#' @export
Median = function(...,na.rm = T) {median(..., na.rm = na.rm)}

#' @export
Sum = function(...,na.rm = T) {sum(..., na.rm = na.rm)}

#' @export
Var = function(...,na.rm = T) {var(..., na.rm = na.rm)}

#' @export
Sd = function(...,na.rm = T) {sd(..., na.rm = na.rm)}

#' @export
pow = function(x,exponent) {return(x^exponent)}

#' @export
normalise = function(vector,mean=1){
  this_mean<-mean(vector,na.rm=TRUE)
  nvector<-(vector/this_mean)*mean
  return(nvector)
}