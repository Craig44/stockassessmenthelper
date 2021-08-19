#' deg2decdeg 
#' @description convert from degrees degrees:minutes:seconds -> decimal degrees
#' @param x <scalar> format degrees:minutes:seconds
#' @return dec_degrees
#' @example
#' deg2decdeg(paste(-43,50,23,sep = ":"))
#' @export
deg2decdeg = function(x) {
  sp_x = strsplit(x, split = ":")
  class(sp_x[[1]]) = "numeric"
  return( (abs(sp_x[[1]][1]) + (sp_x[[1]][2] / 60) + (sp_x[[1]][3] / 3600)) * ifelse(sp_x[[1]][1] < 0, -1, 1))
}
## vectorise the function
deg2decdeg_v = Vectorize(FUN = deg2decdeg)

#' decdeg2deg 
#' @description convert from  decimal degrees -> degrees degrees:minutes:seconds
#' @param x <scalar> format decimal degrees
#' @return degrees:minutes:seconds
#' @example
#' deg2decdeg(paste(-43,50,23,sep = ":"))
#' @export
decdeg2deg <- function(x) {
  x1 = abs(x)
  deg = floor(x1)
  min =  floor((x1 - deg) * 60)
  sec = floor(((x1 - deg) * 60 - min) * 60)
  return(paste(deg * ifelse(x < 0, -1, 1), min, sec, sep = ":"))
}
#' decdeg2deg_v
#' @description vectorised function of decdeg2deg
decdeg2deg_v = Vectorize(FUN = decdeg2deg)

#' long2UTM 
#' @description returns a zone for UTM based on longitude. Expects long can be either [-180, 180] or [0,360]
#' @param long <scalar> longitude, to get UTM zone
#' @return UTM zone
#' @export 
long2UTM <- function(long) {
  ## Function to get the UTM zone for a given longitude
  (floor((long + 180)/6) %% 60) + 1
}

#' LongLatToUTM 
#' @description takes a data frame whith lat and long in decimal degree format. Then
#' returns a spatial dataframe that is in UTM format.
#' @param df <data.frame> that has at least two columns labelled long lat
#' @param zone <integer> zone for UTM.
#' @return SpatialDataFrame
#' @importFrom sp CRS spTransform
#' @export
LongLatToUTM <- function(df, zone = NULL){
  if(!all(c("lat","long") %in% colnames(df)))
    stop("LongLatToUTM() df, requires the columns 'lat' & 'long' to be present.")
  ## Args: df, data frame must have x and y columns. Should be from same UTM zone.
  ## Create a spatial dataframe
  coordinates(df) <- ~ long + lat
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84")  
  
  ## Get zones for from a range of longitudes of data.
  ## Take the most represenative zone of the data.
  if (is.null(zone)) {
    longs = seq(from = min(df$long, na.rm = T), to = max(df$long, na.rm = T), by = 0.1)
    zones <- long2UTM(longs)
    tab_zones = table(zones)
    zone = as.numeric(names(tab_zones)[which.max(tab_zones)])
  }
  
  if (length(unique(zone)) > 1) {
    ## take the zone with the most 
    stop("values from different UTM zones")
  }
  
  ## Change CRS of the spatial data frame and convert to data frame
  res <- spTransform(df, CRS(paste0("+proj=utm +zone=", zone, "+datum=WGS84")))
  return(res)
}


#' DistanceLongLat
#' @description calculate distance between a set of coordinates (as the crow flies)
#' @param long1 longitude first point decimal degrees > 180 are WESTINGS
#' @param long2 longitude second point decimal degrees > 180 are WESTINGS
#' @param lat1 lattitude first point decimal degrees < 0 Southings
#' @param lat2 lattitude second point decimal degrees < 0 Southings
#' @param metres bool, F = nautical miles, T = metres
#' @return a distance
#' @export
DistanceLongLat<-function(long1, long2, lat1, lat2, metres = F) {
  # Inputs start=(long1,lat1) and end=(long2,lat2) in decimal degrees
  # OR assumes that locator is used to define exactly 2 points
  #
  # Assumes longitude numbers are positive and that numbers > 180 are WESTINGS
  # Assumes lattitude negative numbers are SOUTHINGS
  # Outputs nautical miles if metres=F, else distance in metres
  if(missing(long1) | missing(long2) | missing(lat1) | missing(lat2)) {
    cat("Using function \"locator(2)\" to locate end points\n")
    x <- nz.locator(2)
    long1 <- x$x[1]
    long2 <- x$x[2]
    lat1 <- x$y[1]
    lat2 <- x$y[2]
    print(unlist(c(long1, long2, lat1, lat2)))
  }
  long1 <- (long1 * pi)/180
  long2 <- (long2 * pi)/180
  lat1 <- ifelse(lat1 > 180, 360 - lat1,  - lat1)
  lat2 <- ifelse(lat2 > 180, 360 - lat2,  - lat2)
  lat1 <- (lat1 * pi)/180
  lat2 <- (lat2 * pi)/180
  d <- 2 * asin(sqrt((sin((lat1 - lat2)/2))^2 + cos(lat1) * cos(lat2) * (sin((long1 - long2)/2))^2))
  nm <- (d * 180 * 60)/pi
  if(metres)
    nm <- nm * 1.852 * 1000
  return(nm)
}