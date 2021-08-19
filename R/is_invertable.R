#' is_invertable test whether a matrix is invertable, used to test for hessian or covariance is available
#'
#' @author Craig Marsh
#' @export
#' @param m matrix to be tested
#' @return boolean true if invertable and false If not
#' 
is_invertable <- function(m) {
  class(try(solve(m),silent=T))=="matrix"
}

