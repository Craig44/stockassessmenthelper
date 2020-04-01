#' logit logistic transformation
#' @author C.Marsh
#' @param u 
#' @return transformed value
#' @export
logit = function(u) {return(log(u / (1 - u)))};

#' ub_free convert y(-Inf, Ub] -> x(-Inf, Inf)
#' @param y value to transform
#' @param Ub upper bound of value
#' @return unconstrained value
#' @export
ub_free = function(y, Ub) {
  return(log(Ub - y))
}
#' ub_constrain convert x(-Inf, Inf) -> y(-Inf, Ub]
#' @param x value to transform
#' @param Ub upper bound of value
#' @return constrained (natural space) value
#' @export
ub_constrain = function(x, Ub) {
  return(Ub - exp(x))
}
#' lb_constrain convert y(Lb, Inf] -> x(-Inf, Inf)
#' @param x value to transform
#' @param Lb Lower bound of value
#' @return unconstrained value
#' @export
lb_free = function(y, Lb) {
  return(log(y - Lb));
}
#' lb_constrain convert x(-Inf, Inf) -> y(Lb, Inf]
#' @param x value to transform
#' @param Lb Lower bound of value
#' @return constrained (natural space) value
#' @export
lb_constrain = function(x, Lb) {
  return(exp(x) + Lb);
}
#' lub_free convert y(Lb, Ub] -> x(-Inf, Inf)
#' @param y value to transform
#' @param Lb Lower bound of value
#' @param Ub Upper bound of value
#' @return unconstrained value
#' @export
lub_free = function(y, Lb, Ub) {
  ## identity
  if ((Lb == -Inf) & (Ub == Inf))
    return(y)
  if (Lb == -Inf)
    return(ub_free(y, Ub));
  if (Ub == Inf)
    return(lb_free(y, Lb));
  return(logit((y - Lb) / (Ub - Lb)));
}
#' lub_constrain convert x(-Inf, Inf) -> y(Lb, Ub] 
#' @param x value to transform
#' @param Lb Lower bound of value
#' @param Ub Upper bound of value
#' @return unconstrained value
#' @export
lub_constrain = function(x, Lb, Ub) {
  ## identity
  if ((Lb == -Inf) & (Ub == Inf))
    return(x)
  if (Lb == -Inf)
    return(ub_constrain(x, Ub));
  if (Ub == Inf)
    return(lb_constrain(x, Lb));
  inv_logit_x = NULL;
  if (x > 0) {
    inv_logit_x = 1.0 / (1.0 + exp(-x));
    ## Prevent x from reaching one unless it really really should.
    if ((x < Inf) && (inv_logit_x == 1))
      inv_logit_x = 1 - 1e-15;
  } else {
    inv_logit_x = 1.0 - 1.0 / (1.0 + exp(x));
    ## Prevent x from reaching zero unless it really really should.
    if ((x > -Inf) && (inv_logit_x == 0))
      inv_logit_x = 1e-15;
  }
  return(Lb + (Ub - Lb) * inv_logit_x);
}

#' @title draw_from_radius
#' description Stan starts initial values by drawing random values within a radius (+/-2) around zero
#' this script will have a fun funs, that will draw a range of paraemters from thiis, that you
#' can feed into your model to run deterministically to identify problem paraemters when you 
#' @param n number of starting values
#' @param lower_bound Lower bound of parameter
#' @param upper_bound Upper bound of parameter
#' @param minus_init_radius lower bound of radius to draw unconstrained starting values
#' @param plus_init_radius upper bound of radius to draw unconstrained starting values
#' @return start_values for model
#' @export
draw_from_radius = function(n = 10, lower_bound = -Inf, upper_bound = Inf, minus_init_radius = -2, plus_init_radius = 2) {
  ## draw from radius - runif~[minus_init_radius, plus_init_radius]
  y_vals = runif(n, minus_init_radius, plus_init_radius)
  ## transform to natural space.
  x_vals = sapply(y_vals, FUN = lub_constrain,  lower_bound, upper_bound)

  return(x_vals);
}

