#' Utility function for calculating the julian date an alternative to julian.sp.
#'
#' @author Craig Marsh
#' @keywords internal
#'

julian.sp2 <- function(x, origin=19600101)
{
    # x should be the name of the field which is numeric,i.e. 20010102
    day <- x %% 100
    x <- x %/% 100
    month <- x %% 100
    year <- x %/% 100

    origin.d <- origin %% 100
    origin <- origin %/% 100
    origin.m <- origin %% 100
    origin.y <- origin %/% 100

    julian.sp(month,day,year,origin.m,origin.d,origin.y)
}