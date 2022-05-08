#' constant_sel constant selectivity
#' @param C values
#' @param bins could be age or length
#' @return constant selectivty
#' @export
constant_sel<- function(C, bins) {
  if(C<0| C>1) {
    warning("C must be bound between 0 and 1")
  }
  return(rep(C, length(bins)))
}

#' knife_sel Knife Edge selectivity
#' @param E age at inflection
#' @param bins could be age or length
#' @return selectivty for all ages
#' @export
knife_sel<- function(E, bins) {
  result <- ifelse(bins < E, 0, 1)
  return(result)
}

#' logis_sel Logistic selectivity
#' @export
#' @param bins could be age or length
#' @param a50 bin which equates to selectivy being at 0.5
#' @param ato95 bins between the selectivty 0.5 and 0.95
#' @return selectivity for each age.
logis_sel<- function(bins, a50, ato95) {
  1/(1+19^((a50-bins)/ato95)) 
}

#' inv_logis_sel Inverse Logisitic selectivity
#' @export
#' @param bins could be age or length
#' @param a50 bin which equates to selectivy being at 0.5
#' @param ato95 bins between the selectivty 0.5 and 0.95
#' @return selectivity for each age.
inv_logis_sel<- function(bins, a50, ato95) {
  1 - 1/(1+19^((a50 - bins)/ato95)) 
}

#' exp_sel Exponential selectivity
#' @export
#' @param bins could be age or length
#' @param lambda rate decrease
#' @return selectivity for each age.
exp_sel <- function(bins,lambda) {
  exp(-bins*lambda)
}


#' d_norm_sel Double normal selectivity
#' @export
#' @param bins could be age or length
#' @param mu mean value selectivity at one
#' @param sig_l sigma for the left hand curve
#' @param sig_r sigma for the right hand curve
#' @return selectivity for each age
d_norm_sel<- function(bins, mu, sig_l,sig_r) {
  store<- vector()
  for( i in 1:length(bins)) {
    if( bins[i] <= mu) {
        store[i]<- 2^-((bins[i]-mu)/sig_l)^2
    } else {
        store[i]<- 2^-((bins[i]-mu)/sig_r)^2
    }
  }
  return(store)
}


#' d_exp_sel Double Exponential selectivity
#' @export
#' @param bins could be age or length
#' @param x_1 reference bin for the left hand point
#' @param x_2 reference bin for the right hand point
#' @param x_0 reference bin for middle point
#' @param y_1 selectivity at left hand bin
#' @param y_2 selectivity at right hand bin
#' @param y_0 selectivty at middle bin
d_exp_sel <- function(bins, x_1, x_2, x_0, y_0, y_1, y_2)
{
  store<- vector()
  for( i in 1:length(bins)) {
    if( bins[i] <= x_0) {
      store[i]<- min(1, y_0*(y_1/y_0)^((bins[i]-x_0)/(x_1-x_0)))
    } else {
      store[i]<- min(1, y_0*(y_2/y_0)^((bins[i]-x_0)/(x_2-x_0)))
    }
  }
  return(store)
}


