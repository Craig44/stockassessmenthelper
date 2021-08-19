#' vonbert applies the Von Bertalanffy age-length relationship
#' @param age take an age look in globe scope for the rest of parameters
#' @param L_inf asympototic length
#' @param k growth rate parameter
#' @param t0 age for length = 0
#' @export
#' @return mean length at age
vonbert <- function(age,K,L_inf,t0) {
  return(L_inf * (1-exp(-K*(age -t0))))
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

#' stretch does a CV linear interpolation by age used in CASAL to define cv by age
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

#' cv_by_length  does a length based interpolation of cv for the growth function. This is also done in CASAL when by_length = T.
#' @param cv1 cv for first mean length
#' @param cv2 cv for last mean length
#' @param mean_length_at_age mean length at age from a growth function or empirical
#' @return vector of cvs for each mean length
#' @export
cv_by_length <- function(cv1,cv2,mean_length_at_age) {
  n_bins = length(mean_length_at_age)
  return((mean_length_at_age - mean_length_at_age[1])*(cv2 - cv1)/(mean_length_at_age[n_bins]- mean_length_at_age[1]) + cv1)
}