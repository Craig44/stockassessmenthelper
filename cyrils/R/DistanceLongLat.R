#' Distance between two 2 coodinates
#' @author Ian Doonan
#' @export

DistanceLongLat <- function(long1, long2, lat1, lat2, metres = F)
{
# Inputs start=(long1,lat1) and end=(long2,lat2) in decimal degrees
# OR assumes that locator is used to define exactly 2 points
#
# Assumes longitude numbers are positive and that numbers > 180 are WESTINGS
# Assumes lattitude negative numbers are SOUTHINGS
# Outputs nautical miles if metres=F, else distance in metres

if(missing(long1) | missing(long2)| missing(lat1) | missing(lat2)) {
  cat("Using function \"locator(2)\" to locate end points\n")
    x <- nz.locator(2)
    long1 <- x$x[1]
    long2 <- x$x[2]
    lat1 <- x$y[1]
    lat2 <- x$y[2]
    print(unlist(c(long1,long2,lat1,lat2)))
  }
  long1 <- (long1 * pi)/180
  long2 <- (long2 * pi)/180
  lat1 <- ifelse(lat1 > 180, 360 - lat1,  - lat1)
  lat2 <- ifelse(lat2 > 180, 360 - lat2,  - lat2)
  lat1 <- (lat1 * pi)/180
  lat2 <- (lat2 * pi)/180
  d <- 2 * asin(sqrt((sin((lat1 - lat2)/2))^2 + cos(lat1) * cos(lat2) * (
    sin((long1 - long2)/2))^2))
  nm <- (d * 180 * 60)/pi
  if(metres)
    nm <- nm * 1.852 * 1000
  return(nm)

}
