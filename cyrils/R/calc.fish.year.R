#' Calculate the fishing year, in New Zealand this is the 1 October.
#'
#' @param x date to calculate fishing year on, of class numeric in the formal YYYYMMDD
#' @param origin.m origin month for the fishing year, in New Zealand this is October = 10
#' @param out.class the format of the returned year
#' @return fishing year
#' @export

calc.fish.year <- function(x, origin.m=10,out.class="numeric") {
    if( !(out.class %in% c("factor","numeric", "character")) ){
        stop("not a valid out.class")
    }
    CLASS <- class(x)
    if(any(CLASS %in% c("Date", "date", "dates", "chron", "POSIXlt", "POSIXct"))){
       OBJ <- as.POSIXlt(x)
       MONTHS <- as.numeric(format(OBJ, format=c("%m")))
       YEARS <- as.numeric(format(OBJ, format=c("%Y")))
       OUT <- ifelse(MONTHS >= origin.m, YEARS + 1, YEARS)
       LEVELS <- sort(unique(OUT))       
    } else if(CLASS=="numeric"){   # var should be the name of the field which is numeric,i.e. 20010102  
       DAYS <- x %% 100
       x <- x %/% 100
       MONTHS <- x %% 100
       YEARS <- x %/% 100
       OUT <- YEARS + (MONTHS >=origin.m)
       LEVELS <- sort(unique(OUT))                  
    } else {  
        stop("x is not a valid object")
    }
    OUT <- switch(out.class,"factor" = factor(as.character(OUT), levels=LEVELS),
                            "numeric" = OUT,
                            "character" = as.character(OUT))

    return(OUT)    
}