#' TMB helper function
#' @author C.Marsh
#' @date 5/9/2018
#' @description this function returns a list of factors used in the map argument of the MakeADFun function
#' @param par_list a named list that you give to the par argument in the MakeADFun
#' @param pars_to_exclude a vector of strings with names of parmeters you want to FIX in the objective object.
#' @return a list of factors used in the MakeADFun function
#' @export
fix_pars = function(par_list, pars_to_exclude) {
  if (!any(pars_to_exclude %in% names(par_list))) {
    stop(paste0("The parameters ", paste(pars_to_exclude[!pars_to_exclude %in% names(par_list)],collapse = " ")," in exclusion parameters could not be found in the 'par_list', please sort this out"))
  }
  pars = names(par_list)
  mapped_pars = list();
  param_factor = 1;
  for(i in 1:length(pars)) {
    if (pars[i] %in% pars_to_exclude) {
      params_in_this_par = par_list[[pars[i]]];
      mapped_pars[[pars[i]]] = rep(factor(NA),length(params_in_this_par));
    } else {
      params_in_this_par = par_list[[pars[i]]];
      params_vals = factor(param_factor:(param_factor + length(params_in_this_par) - 1))
      param_factor = param_factor + length(params_in_this_par)
      mapped_pars[[pars[i]]] = params_vals
    }
  }
  return(mapped_pars);
}
