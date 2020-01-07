#' @title Colors
#' @param an integer g number of colors you want returned
#' @export
#' @return g color charaters.
Colors <- function(g){
  d <- 360/g
  h <- cumsum(c(15, rep(d,g - 1)))
  hcl(h = h, c = 100, l = 65)
}