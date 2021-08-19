#' Utility functions for generic maths, mostly the same function but ignoring NA's
#'
#' @author Craig Marsh
#' @export
#'
geometric.mean <- function(x) base::exp(base::sum(log(x))/base::length(x))

#' @export
Mean = function(...,na.rm = T) {base::median(..., na.rm = na.rm)}

#' @export
Median = function(...,na.rm = T) {base::median(..., na.rm = na.rm)}

#' @export
Sum = function(...,na.rm = T) {base::sum(..., na.rm = na.rm)}

#' @export
Var = function(...,na.rm = T) {base::var(..., na.rm = na.rm)}

#' @export
Sd = function(...,na.rm = T) {base::sd(..., na.rm = na.rm)}

#' pow exponential function, helpful when debugging C++ functions
#' @export
#' 
pow = function(x,exponent) {return(x^exponent)}

#' @export
normalise = function(vector, scalar = 1){
  this_mean <- mean(vector,na.rm=TRUE)
  nvector <- (vector/this_mean)*scalar
  return(nvector)
}