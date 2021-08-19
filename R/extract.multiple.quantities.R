#' extract.multiple.quantities
#' @details extracts a list of model quantities that have output from a CASAL model using -i format
#' @param file character of filename
#' @param path where the file can be found
#' @examples 
#' \dontrun{
#' file = "test_est.out"
#' csl_dir = "../csl"
#' }
#' @return a list of lists, each element of the top level list is for each -i run
#' 
extract.multiple.quantities = function (file, path = "")  {
  string.match <- function(x, y) return((1:length(y))[casal.regexpr(x,  y) > 0])
  if (missing(path)) 
    path <- ""
  filename <- casal::casal.make.filename(path = path, file = file)
  file <- casal::casal.convert.to.lines(filename)
  start.lines <- string.match("Start extracting output from here", file)
  n.dqs <- length(start.lines)
  end.lines <- c(start.lines[-1] - 1, length(file))
  all_dqs = list();
  for (j in 1:n.dqs) {
    quantities <- list()
    subfile <- file[(start.lines[j]):(end.lines[j])]
    subfile <- casal.get.lines(subfile, clip.to = "Output quantities start here")
    subfile <- casal.get.lines(subfile, clip.from = "Output quantities finish here")
    
    while (1) {
      if (length(subfile) == 0) 
        break
      header <- casal.remove.first.words(casal.get.lines(subfile, 
                                                         contains = "\\*")[1], 1)
      subfile <- casal.get.lines(subfile, clip.to.match = "\\*")
      temp <- casal.get.lines(subfile, clip.from.match = "\\*")
      if (length(temp) == 1 && length(casal.string.to.vector.of.words(temp[1])) == 
          1) {
        quantities[[header]] <- casal.string.to.vector.of.numbers(temp[1])[1]
      }
      else {
        quantities[[header]] <- casal.make.list(temp)
      }
      if (!casal.regexp.in(subfile, "\\*")) 
        break
    }
    all_dqs[[j]] = quantities
  }
  return(all_dqs)
}
