#'
#' plot_CASAL_profile
#' 
#'
#'
#' @description This function
#' @param file file name from profiled outptu.
#' @param path path for profiled output
#' @param objective_function_components either specify 'all' which will plot all likelihood contributions, or a vector of characters for the contirbutions you want. If not sure how to specify these see ask_for_obj_labels 
#' @param ask_for_obj_labels boolean if True the function will print the values available for objective_function_components
#' @param element an integer for when the profiled_param is a vector param.
#' @param likelihood_cut_off ylim for likelihood to remove large values obscuring the trend of the profile
#' @return ggplot and data frame
#' @export
#' @importFrom reshape2 melt
#' @examples
#' \dontrun{
#' 
#' setwd("profileplots")
#' ## ask for liklihood components
#' plot_CASAL_profile(file = '2021_profile_R0.casal', path = getwd(), profiled_param, objective_function_components = "all", element = NULL, ask_for_obj_labels = T, likelihood_cut_off = 100, custom_xlim = c(0,2e7))
#' ## now plot it
#' my_plot = plot_CASAL_profile(file = '2021_profile_R0.casal', path = getwd(), profiled_param = "initialization.R0", objective_function_components = c("BP_SPUE", "REC_CPUE", "PS_AGE","REC_AGE", "ST_AGE", "prior_on_initialization.R0"), element = NULL, ask_for_obj_labels = FALSE, likelihood_cut_off = 100, custom_xlim = c(0,2e7))
#' my_plot = plot_CASAL_profile(file = '2021_profile_rec_ato95.casal', path = getwd(), profiled_param = "selectivity[Sel_REC].all", objective_function_components = c("BP_SPUE", "REC_CPUE", "PS_AGE","REC_AGE", "ST_AGE", "prior_on_initialization.R0"), element = 1, ask_for_obj_labels = FALSE, likelihood_cut_off = 100)
#' }
#' 
plot_CASAL_profile <- function(file, path, profiled_param, objective_function_components = "all", element = NULL, ask_for_obj_labels = FALSE, likelihood_cut_off = 100, custom_xlim = NULL) {
  # check file and path are valid.
  objs = extract.multiple.objective.functions(file = file, path = path)
  if(ask_for_obj_labels)
    return(rownames(objs))

  params = extract.multiple.free.params(file = file, path = path)
  n_profiles = length(params)
  ## get the parameter vaules
  if(!profiled_param %in% names(params[[1]])) 
    stop(cat("the parameter ", profiled_param, " not in the profiles params list object needs to be one of \n", paste( names(params[[1]]), collapse = "\n")))
  vector_param = length(params[[1]][[profiled_param]]) > 1
  if(vector_param & is.null(element)) 
    stop(cat("the parameter ", profiled_param, " is a vector parameter. You need to specify the element using the parameter element parameter i.e. 1 for the first parameter"))
  this_param = NULL
  for(i in 1:n_profiles) {
    if(vector_param) {
      this_param[i] = params[[i]][[profiled_param]][element]
    } else {
      this_param[i] = params[[i]][[profiled_param]]
    }
  }
  if(length(this_param) != ncol(objs)) 
    stop("number of parameters from profile are inconsistent with the objectives values. THis is a bug in the code talk to developer (if they aren't busy =)")

  ## find the MPD run
  mpd_ndx = which.min(colSums(objs))
  mpd_value = this_param[mpd_ndx]
  ## now format the data for ggplot
  colnames(objs) =  this_param
  original_rownames = rownames(objs)
  objs = rbind(objs, colSums(objs))
  rownames(objs) = c(original_rownames, "total")
  
  ## Rescale so min = 0
  row_mins = apply(objs, 1, min)
  objs = sweep(objs, MARGIN = 1, STATS = row_mins, FUN = "-")
  melted_obs = melt(objs)
  colnames(melted_obs) = c("Contribution", "param", "value")
  
  
  vars_of_interest = rownames(objs)
  if(length(objective_function_components) > 1 || objective_function_components != "all") {
    if(!all(objective_function_components %in%  rownames(objs))) 
      stop(cat("the objective_function_components are not consistent with CASAL's printed values. The options available are 'all', or a subset of \n", paste( rownames(objs), collapse = "\n")))
    vars_of_interest = objective_function_components
  }
  vars_of_interest = c(vars_of_interest, "total")
  
  melted_obs$value[melted_obs$value > likelihood_cut_off] = likelihood_cut_off
  ### The plot
  if(is.null(custom_xlim)) 
    custom_xlim = range(this_param)
  plt = ggplot(data = melted_obs %>% filter(Contribution %in% vars_of_interest), aes(x = param, y = value)) +
    geom_area(stat = "identity", col = "#56B4E9", fill = "#56B4E9") +
    xlim(custom_xlim) +
    geom_vline(data = NULL, xintercept = mpd_value, color="black", size=1, lty = 2) +
    xlab(profiled_param) +
    ylab("obj - min(obj)") +
    theme_bw() +
    facet_wrap(~Contribution, ncol = 1, scale = "fixed")
    
  print(plt)
  return(melted_obs)
}


