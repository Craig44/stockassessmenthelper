## Casal Ogives

#' @export
cons<- function(C)
{
  if(C<0| C>1)
  {
    warning("C must be bound between 0 and 1")
  }
  return(C)
}

## Knife Edge
#' @export

knife<- function(X,E)
{
  store<- vector()
  for( i in 1:length(X))
  {
  if( X[i] < E)
  {store[i]<- 0} else {
   store[i]<- 1}
  }
  return(store)
  }

## Logistic
#' @export

logis<- function(X,a50,a95)
{
 1/(1+19^((a50-X)/a95)) 
}

## Inverse Logisitic
#' @export

inv_logis<- function(X,a50,a95)
{
  1- 1/(1+19^((a50-X)/a95)) 
}

## Exponential
#' @export

Exp<- function(X,lambda)
{
  exp(-X*lambda)
}


## Double normal
#' @export

d_norm<- function(X,mu, sig_l,sig_r)
{
  store<- vector()
  for( i in 1:length(X))
  {
    if( X[i] <= mu)
    {store[i]<- 2^-((X[i]-mu)/sig_l)^2} else {
      store[i]<- 2^-((X[i]-mu)/sig_r)^2}
  }
  return(store)
}


## Double Exponential
#' @export

d_exp <- function(X,x_1,x_2,x_0,y_0,y_1,y_2)
{
  store<- vector()
  for( i in 1:length(X))
  {
    if( X[i] <= x_0)
    {store[i]<- min(1,y_0*(y_1/y_0)^((X[i]-x_0)/(x_1-x_0)))} else {
      store[i]<- min(1, y_0*(y_2/y_0)^((X[i]-x_0)/(x_2-x_0)))}
  }
  return(store)
}


