#' r_lnorm Generates 
#' @description iid lognormal draws with expectation (note not the same as the mu parameter) and sigma in the same manner as CASAL
#' @param N <int>: number of draws
#' @param expectation <double>: The expectation of the distribution, this is not the mu parameter because for the lognormal distribution the expectation is not the expectation parameter.
#' @param cv <double>: The coefficent of variation of the distribution
#' @param sigma <double>: The standard deviation of the distribution (in log space)
#' @export

r_lnorm = function(N, expectation,cv = NULL, sigma = NULL) {
  ## cannot specify both cv and sigma
  if (is.null(cv) & is.null(sigma)) {
    stop("you must input either the cv or sigma parameter")
  }
  if (!is.null(cv) & !is.null(sigma)) {
    stop("you cammot input both the cv and sigma parameter, only specify one of them")   
  }
  if (!is.null(cv)) {
    sigma = sqrt(log(cv*cv + 1.0));
  }
  log_mu = log(expectation) - (sigma * sigma) / 2.0;
  X = rnorm(N,log_mu,sigma)
  return(exp(X))
}

#' log_cv Calculate the CV of the lognormal distribution based on \deqn{cv = \sqrt{e^{\sigma^2} - 1}}
#'
#' @param sigma The standard deviation of the lognormal distribution
#' @return The the cv
#' @export
log_cv = function(sigma) {
  cv = sqrt(exp(sigma^2) - 1)
  return(cv)
}

#' log_sigma 
#' @description Calculate the sigma of the lognormal distribution based on \deqn{\sigma = \sqrt{log(cv^2 + 1)}}
#' @param cv The CV (note this is in proportion not percentage) of the lognormal distribution
#' @return The the sigma
#' @export

log_sigma = function(cv) {
  sigma = sqrt(log(cv^2 + 1))
  return(sigma)
}

#' lognormal_CI 
#' @description Calculate upper and lower bounds for the lognormal distribution based with expectation and cv
#' @param cv The standard deviation of the lognormal distribution
#' @param expectation The expectation of the distribution, this is not the mu parameter because for the lognormal distribution the expectation is not the mu parameter.
#' @param CI level of confidence (units are proportions not percentage i.e. 0.95 for 95 CI)
#' @export
#' @return a list with upper and lower elements 
lognormal_CI <- function(expectation, cv ,CI = 0.95) {
  sigma = log_sigma(cv)
  mu = (log(expectation) - 0.5*(sigma^2))
  zscore = abs(qnorm((1 - CI)/2))
  U_CI = exp(mu + zscore * sigma)
  L_CI = exp(mu - zscore * sigma)  
  return(list("upper" = U_CI, "lower" = L_CI))
}

#' lnorm_prior 
#' @description calculate the log-likelihood contribution for the log-normal prior in CASAL
#' @param X <double>: value/'s to calculate the prior for
#' @param expectation <double>: The expectation of the distribution, this is not the mu parameter because for the lognormal distribution the expectation is not the expectation parameter.
#' @param sigma <double>: The standard deviation of the distribution (in log space)
#' @param negative <bool>: Return the negative log-likelihod if True else if FALSE return just the log-likelihood
#' @export
lnorm_prior <- function(X,expectation,sigma, negative = TRUE) {
  store_vec = vector();
  for (i in 1:length(X)) {
    store_vec[i] = log(X[i]) + log(sigma) + 0.5 * (log(X[i]/ expectation) / sigma + sigma * 0.5)^2
  }
  if(!negative) {
    store_vec = -1*store_vec;
  }
  return(store_vec)
}