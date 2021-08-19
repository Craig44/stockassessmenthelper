#' rmvnorm_prec 
#' @description simulates parameters from the joint precision matrix derived from a TMB objects
#' @param mu vector of MLE both fixed and random effect parameters
#' @param prec precision matrix, derived from sdreport(obj, getJointPrecision = T)
#' @param n.sims integer number of simulations
#' @param random_seed integer seed
#' @export
#' @importFrom Matrix Cholesky solve
rmvnorm_prec <- function(mu, prec, n.sims, random_seed ) {
  set.seed( random_seed )
  z = matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
  L = Cholesky(prec, super=TRUE)
  z = solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z = solve(L, z, system = "Pt") ## z = Pt    %*% z
  z = as.matrix(z)
  return(mu + z)
}


#' get_tmb_fixed_effects
#' @description return MLE estaimtes for fixed effects
#' @param obj An optimised list that has been build by MakeAdFun
#' @export
get_tmb_fixed_effects <- function(obj) {
  if (length(obj$env$random) == 0) {
    return(obj$env$last.par.best)
  }
  return(obj$env$last.par.best[-obj$env$random])
}

#' check_tmb_convergence
#' @author C.Marsh
#' @description use TMB object to check gradients and if the hessian is definite positive
#' @param obj An optimised list that has been build by MakeAdFun
#' @param delta Gradient threshold for defining a converged model
#' @importFrom TMB sdreport
#' @export 
check_tmb_convergence <- function(obj, delta = 0.001) {
  best_fixed_eff_pars = get_tmb_fixed_effects(obj)
  grads = tryCatch( expr = obj$gr(best_fixed_eff_pars));
  if (inherits(grads, "error"))
    return("Could not get gradients with an error. generally happens from unconverged models please check")
  if (any(is.na(grads)))
    return("Found gradients with NaN, conclusion = unconverged model")
  labs = names(obj$par)
  if(max(abs(grads)) > delta)
    return(paste0("param with label = ", labs[which.max(abs(grads) > delta)], " has gradient > ", delta, " |grad| = ", round(max(abs(grads)), 5)))
  hess = optimHess(best_fixed_eff_pars, fn = obj$fn, gr = obj$gr)
  sdr = sdreport(obj, getJointPrecision = TRUE)
  if (is.character(try(chol(sdr$cov.fixed), silent = TRUE)))
    return("Covariance of fixed effect no positive Definitive")
  if ("jointPrecision" %in% names(sdr)) {
    if (is.character(try(chol(sdr$jointPrecision), silent = TRUE)))
      return("Joint Precision not positive Definitive")
  }
  return("No evidence of non convergence")
}

#' fix_pars 
#' @author C.Marsh
#' @description TMB helper function this function returns a list of factors used in the map argument of the MakeADFun function
#' @param par_list a named list that you give to the par argument in the MakeADFun
#' @param pars_to_exclude a vector of strings with names of parmeters you want to FIX in the objective object.
#' @param vec_pars_to_adjust a vector string of parameter labels that we want to exclude certain elements.
#' @param vec_elements_to_exclude a list with number of elements = length(vec_pars_to_adjust). each list element 
#' contains a vector of elements that we want to exclude from estimation.
#' @return a list of factors used in the MakeADFun function
#' @export
fix_pars <- function(par_list, pars_to_exclude, vec_pars_to_adjust = NULL, vec_elements_to_exclude = NULL) {
  if (!any(pars_to_exclude %in% names(par_list))) {
    stop(paste0("The parameters ", paste(pars_to_exclude[!pars_to_exclude %in% names(par_list)],collapse = " ")," in exclusion parameters could not be found in the 'par_list', please sort this out"))
  }
  pars = names(par_list)
  mapped_pars = list();
  tailor_vectors = FALSE
  if (!is.null(vec_pars_to_adjust)) {
    tailor_vectors = TRUE
    if (!all(vec_pars_to_adjust %in% pars_to_exclude))
      stop("parmaeters noted in vec_pars_to_adjust, need to also be in pars_to_exclude")
  }
  param_factor = 1;
  for(i in 1:length(pars)) {
    if (pars[i] %in% pars_to_exclude) {
      params_in_this_par = par_list[[pars[i]]];
      if (tailor_vectors & (pars[i] %in% vec_pars_to_adjust)) {
        include_element_index = c(1:length(params_in_this_par))[-vec_elements_to_exclude]
        params_vals = factor(rep(NA, length(params_in_this_par)), levels = factor(param_factor:(param_factor + length(include_element_index) - 1)))
        params_vals[include_element_index] = factor(param_factor:(param_factor + length(include_element_index) - 1))#, levels = factor(include_element_index))
        param_factor = param_factor + length(include_element_index)
        mapped_pars[[pars[i]]] = params_vals;
      } else {
        mapped_pars[[pars[i]]] = rep(factor(NA),length(params_in_this_par));
      }
    } else {
      params_in_this_par = par_list[[pars[i]]];
      params_vals = factor(param_factor:(param_factor + length(params_in_this_par) - 1))
      param_factor = param_factor + length(params_in_this_par)
      mapped_pars[[pars[i]]] = params_vals
    }
  }
  return(mapped_pars);
}

#' eigen_decomp_covariance 
#' @description Do an eigen decomposition to look at poorly estimated parameters from MLE fit
#' @param covariance_matrix symetric covariance matrix
#' @param param_labels vector of param labels (optional)
#' @param delta a cut off value for 'poorly' defined parameters.
#' @return data frame of eiegen values for the matrix and index of good and bad parameters based on delta
#' @export
eigen_decomp_covariance <- function(covariance_matrix, param_labels = NULL, delta = .Machine$double.eps) {
  ## check covariance is invertable
  if (!isSymmetric(covariance_matrix))
    stop("covariance matrix is not symetric something is wrong here.")
  ## check positive semi defintie matrix
  if(class(try(solve(covariance_matrix),silent=T)) != "matrix")
    stop("covariance not invertible")
  ## calculate hessian
  hess = solve(covariance_matrix)
  ## eigen decomposition
  Eig = eigen(hess)
  WhichBad = which(Eig$values < sqrt(delta))
  df = NULL;
  if (is.null(param_labels)) 
    param_labels = as.character(1:ncol(covariance_matrix))
  
  
  if (length(WhichBad) == 0) {    
    message( "All parameters are identifiable" )
  } else {
    # Check for parameters
    RowMax = apply( Eig$vectors[,WhichBad,drop=FALSE], MARGIN=1, FUN=function(vec){max(abs(vec))} )
    df = data.frame("Param"=param_labels, "eigenvalues", Eig$values ,"Param_check"=ifelse(RowMax>0.1, "Bad","OK"))
  }
  return(df)
}
