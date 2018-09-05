#' Utility function for calculating the julian date.
#'
#' @author Craig Marsh
#' @keywords internal
#'

julian.sp <- function(m, d, y, origin.m=1,origin.d=1,origin.y=1960)
{
  only.origin <- all(missing(m), missing(d), missing(y))
  if(only.origin) m <- d <- y <- NULL # return days since origin
        origin. <- c(origin.m , origin.d , origin.y )
  nms <- names(d)
  max.len <- max(length(m), length(d), length(y)) #
  m <- c(origin.[1], rep(m, length = max.len))
  d <- c(origin.[2], rep(d, length = max.len))
  y <- c(origin.[3], rep(y, length = max.len))  #
  y <- y + ifelse(m > 2, 0, -1)
  m <- m + ifelse(m > 2, -3, 9)
  c <- y %/% 100
  ya <- y - 100 * c
  out <- (146097 * c) %/% 4 + (1461 * ya) %/% 4 + (153 * m + 2) %/%5 + d +1721119 #
  if(!only.origin) {
    if(all(origin. == 0)) out <- out[-1] else out <- out[-1] -out[1]  # orig according to S algorithm
  }
  names(out) <- nms
  out
}