#' sim_pars 
#' @description simulate random start parameters using a uniform random variable for CASAL mdoels
#' @param dash_o_file_name file name of a parameter file created from casal -r -o > par.file > run.out
#' @param csl_path the path to where csl files are and the dash_o_file_name
#' @param plus_minus_for_bounds this sets the parameter space used to simulate starting values from uniform distribution LB = param_val - param_val x plus_minus_for_bounds and UB = param_val + param_val x plus_minus_for_bounds
#' @param n_pars number of simulated parameters (rows)
#' @param index_of_pars_fixed if parameters are fixed by forcing lower_bound == upper_bound you will need to tell these functions this, i.e. index_of_pars_fixed = c(1,30) means the first and thirtith parameters area fixed and don't need to be simulated.
#' @importFrom casal extract.free.parameters.from.table
#' @export
#' @examples
#' \dontrun{
#' csl_dir = "../csl"
#' sim_pars(dash_o_file_name = "par.out", csl_path = csl_dir, plus_minus_for_bounds= 0.5, n_pars = 10, index_of_pars_fixed = c(29:31))
#' # then run the following casal command
#' # casal -e -f K12014_ -i sim_pars.out > test_est.out   
#' multi_obj = extract.multiple.objective.functions(file = "test_est.out", path = csl_dir)
#'}
#' @return will create a par file called sim_pars.out at csl_path will also return a matrix of the simulated value
sim_pars = function(dash_o_file_name, csl_path,  plus_minus_for_bounds = 0.1, n_pars = 10, index_of_pars_fixed = NULL) {
  dash_o_file_name_w_path = normalizePath(file.path(csl_path, dash_o_file_name))
  dash_o_file = extract.free.parameters.from.table(dash_o_file_name_w_path)
  delta = plus_minus_for_bounds * dash_o_file[1,]
  LB = as.numeric(ifelse(dash_o_file[1,] >= 0, dash_o_file[1,] - delta, dash_o_file[1,] + delta))
  UB = as.numeric(ifelse(dash_o_file[1,] >= 0, dash_o_file[1,] + delta, dash_o_file[1,] - delta))

  sim_pars = matrix(nrow = n_pars, ncol = length(LB))
  header = readLines(dash_o_file_name_w_path)[1]
  cat(header, file = normalizePath(file.path(csl_path, "sim_pars.out")))
  
  for(i in 1:n_pars) {
    vals = runif(length(LB), LB, UB)
    if(!is.null(index_of_pars_fixed)) {
      vals[index_of_pars_fixed] = as.numeric(dash_o_file[1,index_of_pars_fixed])
    }
    cat("\n", file = normalizePath(file.path(csl_path, "sim_pars.out")), append = T)
    cat(vals, file = normalizePath(file.path(csl_path, "sim_pars.out")), append = T)
    sim_pars[i,] =  vals
  }
  return(sim_pars)
}

