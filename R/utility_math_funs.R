#' Utility functions for generic maths, mostly the same function but ignoring NA's
#'
#' calculate geometric mean when you have na's in vector
#' @param x vecor of strictly positive values
#' @return geometric mean with na's ignored
#' @export
#'
geometric.mean <- function(x) {
  if(any(x<=0))
    stop("cannot calcualte geometric.mean() with values of x less than equal to zero")
  exp(sum(log(x))/length(x))
}

#' overloading function to calculate arithmic mean when you have na's in vector
#' @param ... same parameters as mean(...)
#' @param na.rm	 a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return mean with na's ignored
#' @export
Mean = function(...,na.rm = T) {mean(..., na.rm = na.rm)}

#' overloading function to calculate median when you have na's in vector
#' @param ... same parameters as median(...)
#' @param na.rm	 a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return median with na's ignored
#' @export
Median = function(...,na.rm = T) {stats::median(..., na.rm = na.rm)}

#' overloading function to calculate sum when you have na's in vector
#' @param ... same parameters as sum(...)
#' @param na.rm	 a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return sum with na's ignored
#' @export
Sum = function(...,na.rm = T) {sum(..., na.rm = na.rm)}

#' overloading function to calculate variance when you have na's in vector
#' @param ... same parameters as var(...)
#' @param na.rm	 a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return variance with na's ignored
#' @export
Var = function(...,na.rm = T) {stats::var(..., na.rm = na.rm)}
#' overloading function to calculate standard deviation when you have na's in vector
#' @param ... same parameters as sd(...)
#' @param na.rm	 a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return standard deviation with na's ignored
#' @importFrom stats plogis qlogis rnorm
#' @export
Sd = function(...,na.rm = T) {stats::sd(..., na.rm = na.rm)}

#' pow exponential function, helpful when debugging C++ functions
#' @param x base value
#' @param exponent the exponent to apply to base value 
#' @return x^exponent
#' @export
#' 
pow = function(x,exponent) {return(x^exponent)}

#' scale each element by the arithmic mean
#' @details na's will be ignored
#' @param vector numeric vector to normalise
#' @param scalar multiplier to change average values
#' @param na.rm	 a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return vector of normalised values to have mean = scalar
#' @export
normalise = function(vector, scalar = 1, na.rm = T){
  this_mean <- Mean(vector,na.rm = na.rm)
  nvector <- (vector/this_mean)*scalar
  return(nvector)
}