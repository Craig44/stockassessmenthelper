#' deg2decdeg 
#' @description convert from degrees degrees:minutes:seconds -> decimal degrees
#' @param x <scalar> format degrees:minutes:seconds
#' @return dec_degrees
#' @examples
#' \dontrun{
#' deg2decdeg(paste(-43,50,23,sep = ":"))
#' }
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
#' @examples
#' \dontrun{
#' deg2decdeg(paste(-43,50,23,sep = ":"))
#' }
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
#' @importFrom sp CRS spTransform coordinates proj4string
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
