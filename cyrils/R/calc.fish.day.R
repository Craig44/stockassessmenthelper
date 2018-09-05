#' Calculate the fishing day from the start of a month, in New Zealand this is the 1 October.
#'
#' @param x date to calculate fishing day on, of class numeric in the formal YYYYMMDD
#' @return fishing day in the year
#' @export

calc.fish.day <- function(x,origin.m=10){
    CLASS <- class(x)
    if(any(CLASS %in% c("Date", "date", "dates", "chron", "POSIXlt", "POSIXct"))){
         stop("expecting numeric format '20010102' (numeric)")
    } else if(CLASS =="numeric"){
        day <- x %% 100
        x <- x %/% 100
        month <- x %% 100
        year <- x %/% 100
        fyear <- year + (month >= origin.m)
        fday <- julian.sp(month,day,year)-julian.sp(origin.m,1,fyear-1)
    } else {
        stop("x is not a valid object")
    }    
    return(fday)
}
