#' Generates iid lognormal draws with mean (mu) and sigma in the same manner as CASAL
#' @param N <int>: number of draws
#' @param Expectation <double>: The Expectation of the distribution, this is not the mu parameter because for the lognormal distribution the expectation is not the mean parameter.
#' @param cv <double>: The coeficent of variation of the distribution
#' @param sigma <double>: The standard deviation of the distribution (in log space)
#' @export

r_lnorm = function(N, Expectation,cv = NULL, sigma = NULL) {
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
  log_mu = log(Expectation) - (sigma * sigma) / 2.0;
  X = rnorm(N,log_mu,sigma)
  return(exp(X))
}

#' Calculate the CV of the lognormal distribution based on \deqn{cv = \sqrt{e^{\sigma^2} - 1}}
#'
#' @param sigma The standard deviation of the lognormal distribution
#' @return The the cv
#' @export
log_cv = function(sigma) {
  cv = sqrt(exp(sigma^2) - 1)
  return(cv)
}

#' Calculate the sigma of the lognormal distribution based on \deqn{\sigma = \sqrt{log(cv^2 + 1)}}
#'
#' @param cv The CV (Coeffecient of Variation) of the lognormal distribution
#' @return The the sigma
#' @export

log_sigma = function(cv) {
  sigma = sqrt(log(cv^2 + 1))
  return(sigma)
}

#' Calculate the CV of the lognormal distribution based on \deqn{cv = \sqrt{e^{\sigma^2} - 1}}
#'
#' @param sigma The standard deviation of the lognormal distribution
#' @return The the cv
#' @export
lognormal_CI = function(Mean, cv ,CI = 0.95) {
  sigma = log_sigma(cv)
  mu = (log(Mean) - 0.5*(sigma^2))
  zscore = abs(qnorm((1 - CI)/2))
  U_CI = exp(mu + zscore * sigma)
  L_CI = exp(mu - zscore * sigma)  
  return(list("upper" = U_CI, "lower" = L_CI))
}

#' calculate the log-likelihood contribution for the log-normal prior in CASAL
#' @param X <double>: value/'s to calculate the prior for
#' @param Expectation <double>: The Expectation of the distribution, this is not the mu parameter because for the lognormal distribution the expectation is not the mean parameter.
#' @param sigma <double>: The standard deviation of the distribution (in log space)
#' @param negative <bool>: Return the negative log-likelihod if True else if FALSE return just the log-likelihood
#' @export
lnorm_prior = function(X,Expectation,sigma, negative = TRUE) {
  store_vec = vector();
  for (i in 1:length(X)) {
    store_vec[i] = log(X[i]) + log(sigma) + 0.5 * (log(X[i]/ Expectation) / sigma + sigma * 0.5)^2
  }
  if(!negative) {
    store_vec = -1*store_vec;
  }
 return(store_vec)
}
