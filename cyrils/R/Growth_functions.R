#' @title Von Bert L-W relationship
#' @parm take an age look in globe scope for the rest of parameters
#' @param the usual VB params
#' @export
#' @return mean length at age
VB<- function(age,K,L_inf,t0)
{
return(L_inf * (1-exp(-K*(age -t0))))
}


#' @title schnute
#' @description return teh mean length at age for the Schnute growth equation
#' @export
#' @return the mean length at age
schnute <- function(y1,y2,t1,t2,a,b,age){

if(!(a == 0) & !(b == 0))
{
 (y1^b + (y2^b - y1^b) * ((1-exp(-a*(age-t1)))/(1-exp(-a*(t2-t1)))))^(1/b)

} else if(!(a == 0) & b == 0){
  
 (y1*exp(log(y2/y1) * ((1-exp(-a*(age-t1)))/(1-exp(-a*(t2-t1))))))
  
} else if(a == 0 & !(b == 0)){
  
  ((y1^b + (y2^b - y1^b)* (age-t1)/(t2-t1))^(1/b))
  
} else {
  
  (y1*exp(log(y2/y1)*(age-t1)/(t2-t1)))
  
}}
  
#' Look at the CV linear interpolation by age
#' From CASAL it is called Stretch
#' @param - cv1 is the cv for min_age
#' @param - cv2 is the cv for max_age
#' @export
stretch <- function(cv1, cv2, min_age, max_age)
{
  return_vec<- vector()
  for( i in min_age:max_age)
  {
  return_vec[i] = cv1 + (cv2-cv1) * (i-min_age)/(max_age -min_age)
  }
return(return_vec)
}

#' @title by_length is what CASAL uses to calculate cv by length and not age.
#' @param cvs for min age and max age
#' @export
by_length <- function(cv1,cv2,age, age_min,max_age)
{
 return((VB(age,K,L_inf,t0) - VB(min_age,K,L_inf,t0))*(cv2 - cv1)/(VB(max_age,K,L_inf,t0)- VB(min_age,K,L_inf,t0)) + cv1)
}

## Dave Fournier's exponential version of the cv interpolation
by_length_exp <- function(cv1,cv2,age, age_min,max_age)
{
 return((VB(age,K,L_inf,t0) - VB(min_age,K,L_inf,t0))*(cv2 - cv1)/(VB(max_age,K,L_inf,t0)- VB(min_age,K,L_inf,t0)) + cv1)
}