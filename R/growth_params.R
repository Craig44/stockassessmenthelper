#' vonbert applies the Von Bertalanffy age-length relationship
#' @param age take an age look in globe scope for the rest of parameters
#' @param L_inf asympototic length
#' @param K growth rate parameter
#' @param t0 age for length = 0
#' @export
#' @return mean length at age
vonbert <- function(age,K,L_inf,t0) {
  return(L_inf * (1-exp(-K*(age -t0))))
}

#' basic_weight calculate weight for a given length based on allometric length weight relationship with bias correction
#' bias correction follows CASAL which is applied in age-structured models.
#' @param a a parameter in growht function
#' @param b exponent
#' @param l length
#' @param cv cv from the age-length distribution
#' @param distribution of age-length distribution
#' @export
#' @return weight, units will depend on a and b
basic_weight = function(a, b, l, cv, distribution) {
  bias_correct = 1;
  if(distribution %in% c("normal","lognormal")) {
    bias_correct = (1.0 + cv * cv)^(b * (b - 1.0) / 2.0)
  }
  return((a*l^b)*bias_correct)
}

#' schnute 
#' @description general age-length growth formula
#' @export
#' @param y1 lenght at reference age t1
#' @param y2 length at reference age t2
#' @param t1 reference age for y1
#' @param t2 reference age for y2
#' @param a schnute growth param
#' @param b schnute growth param
#' @param age age to calculate mean length for
#' @return return teh mean length at age for the Schnute growth equation
schnute <- function(y1,y2,t1,t2,a,b,age){
  
  if(!(a == 0) & !(b == 0)) {
    (y1^b + (y2^b - y1^b) * ((1-exp(-a*(age-t1)))/(1-exp(-a*(t2-t1)))))^(1/b)
    
  } else if(!(a == 0) & b == 0){
    
    (y1*exp(log(y2/y1) * ((1-exp(-a*(age-t1)))/(1-exp(-a*(t2-t1))))))
    
  } else if(a == 0 & !(b == 0)){
    
    ((y1^b + (y2^b - y1^b)* (age-t1)/(t2-t1))^(1/b))
    
  } else {
    (y1*exp(log(y2/y1)*(age-t1)/(t2-t1)))
  }
}

#' basic 
#' @description allometric Length weight relationship
#' @param len length 
#' @param a multiplier
#' @param b exponent
#' @export
basic<- function(a,b,len) {
  a*len^b
}

#' stretch 
#' @description does a CV linear interpolation by age used in CASAL to define cv by age
#' @param cv1 is the cv for min_age
#' @param cv2 is the cv for max_age
#' @param min_age min age to calculate cvs for
#' @param max_age max age to calculate cvs for
#' @return vector of cvs from min_age to max_age
#' @export
stretch <- function(cv1, cv2, min_age, max_age) {
  return_vec<- vector()
  for( i in min_age:max_age) {
    return_vec[i] = cv1 + (cv2-cv1) * (i-min_age)/(max_age -min_age)
  }
  return(return_vec)
}

#' cv_by_length  
#' @description does a length based interpolation of cv for the growth function. This is also done in CASAL when by_length = T.
#' @param cv1 cv for first mean length
#' @param cv2 cv for last mean length
#' @param mean_length_at_age mean length at age from a growth function or empirical
#' @return vector of cvs for each mean length
#' @export
cv_by_length <- function(cv1,cv2,mean_length_at_age) {
  n_bins = length(mean_length_at_age)
  return((mean_length_at_age - mean_length_at_age[1])*(cv2 - cv1)/(mean_length_at_age[n_bins]- mean_length_at_age[1]) + cv1)
}

#' transpose_VB
#' @details transpose the traditional VB formulation into a linear increment model based on the
#' paper \insertCite{francis1988maximum}{stockassessmenthelper}
#' @importFrom Rdpack reprompt
#' @param L1 length at reference 1, corresponds to g1
#' @param L2 length at reference 2, corresponds to g2
#' @param Linf mean asympote length
#' @param k VB growth rate
#' @param t0 VB age at length 0
#' @param L1 length at reference 1
#' @return vector<g1, g2> the linear growth increment model
#' @export
#' @examples 
#'\dontrun{
#' transpose_VB(L1 = 50, L2 = 100, Linf = 155.9, k = 0.116, t0 = 2.495)
#'}
#' @references
#' \insertAllCited{}
#'
#'
transpose_VB <- function(L1, L2, Linf, k, t0) {
  g2 = ((Linf-(L2+t0))*((L1+t0)-(L2+t0))*(exp(-k)-1))/((L2+t0)-(L1+t0))
  g1 = g2+(((L1+t0)-(L2+t0))*(exp(-k)-1)) 
  return(c(g1,g2))
}

#' linear_growth_increment_model
#' @details Growth increment model based on the transposed VB formulation based on \insertCite{francis1988maximum}{stockassessmenthelper}
#' @param length_mid_points vector of lengths to calculate growth increment
#' @param L1 length at reference 1, corresponds to g1
#' @param L2 length at reference 2, corresponds to g2
#' @param g1 growth rate at L1
#' @param g2 growth rate at L2
#' @return vector mean growth increment for each length bins
#' @export
#' @references
#' \insertAllCited{}
#'
linear_growth_increment_model <- function(length_mid_points, L1, L2, g1, g2) {
  length_incr = g1 + (g2 - g1) * (length_mid_points - L1) / (L2 - L1);
  return(length_incr)
}


