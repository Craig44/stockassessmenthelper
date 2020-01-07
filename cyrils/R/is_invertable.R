#' an asks if a matrix is invertable
#'
#' @author Craig Marsh
#' @export
#'
is_invertable = function(m) {class(try(solve(m),silent=T))=="matrix"}

