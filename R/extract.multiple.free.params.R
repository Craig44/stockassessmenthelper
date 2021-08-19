#' extract.multiple.free.params
#' @details extracts a list of model free parameters that have output from a CASAL model using -i format, or a profile -p. This function assumes you are reporting free parameters (I think this is the default)
#' @param file character of filename
#' @param path where the file can be found
#' @export
#' @examples 
#' \dontrun{
#' Add example usage
#' }
#' @return a list of lists, each element of the top level list is for each -i run
extract.multiple.free.params <- function (file, path = "")  {
  string.match <- function(x, y) return((1:length(y))[casal.regexpr(x,  y) > 0])
  if (missing(path)) 
    path <- ""
  filename <- casal::casal.make.filename(path = path, file = file)
  file <- casal::casal.convert.to.lines(filename)
  start.lines <- string.match("Start extracting output from here", file)
  n.params <- length(start.lines)
  end.lines <- c(start.lines[-1] - 1, length(file))
  all_params = NULL;
  for (j in 1:n.params) {
    params <- list()
    subfile <- file[(start.lines[j]):(end.lines[j])]
    subfile <- casal:::casal.get.lines(subfile, clip.to = "Parameter values :")
    subfile <- casal:::casal.get.lines(subfile, clip.from = "In a format suitable for -i :") ## this may not be true for all models
    
    param_value_positions <- string.match("current value", subfile)
    this_set_of_params = list()
    for(p in 1:length(param_value_positions)) {
      current_vals = casal:::casal.string.to.vector.of.numbers(casal:::casal.remove.first.words(subfile[param_value_positions[p]], 2))
      param_lab = subfile[param_value_positions[p] - 1]
      ## deal with labels for vector params
      if (casal:::casal.regexp.in(param_lab, "parameters")) {
        param_lab <- casal.remove.last.words(param_lab, 1)
      }
      this_set_of_params[[param_lab]] = current_vals
    }
    all_params[[j]] = this_set_of_params
  }
  return(all_params)
}
