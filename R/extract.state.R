#' extract.state extract State at the end of year
#' @export
#' @param path path to the filename 
#' @param file casal output filename
#' @return list of AFs
extract.state = function (file, path = "")  {
  states <- list()
  if (missing(path)) 
    path <- ""
  filename <- casal.make.filename(path = path, file = file)
  file <- casal.convert.to.lines(filename)
  line_ndx = casal.regexpr("State at the end of year",  file)
  if (all(line_ndx < 0)) {
    cat(paste("No output data found in file '", filename, 
              "'", sep = ""))
    return(NA)
  }
  file <- casal.get.lines(file, from = which(line_ndx >= 0)[1], to = which(line_ndx >= 0)[sum(line_ndx >= 0)])
  
  while (1) {
    if (length(file) == 0) 
      break
    header <- casal.get.lines(file, contains = "State at the end of year")[1]
    file <- casal.get.lines(file, clip.to.match = "State at the end of year")
    year = substring(header, first = 26, last = 29)
    end_of_section = which(casal.regexpr("SSBs:",  file) >= 0)[1]
    temp <- file[2:(end_of_section - 1)]
    mat_header = casal.string.to.vector.of.words(temp[1])
	  deal_with_tag = FALSE
	  if("tag" %in% mat_header)
	    deal_with_tag = TRUE
    row_labs = NULL
    Mat = matrix(ncol = length(mat_header)-2 - ifelse(deal_with_tag, 1, 0), nrow = length(temp) - 1)
    for(i in 2:(length(temp))) {
      this_vec = casal.string.to.vector.of.words(temp[i])
      Mat[i - 1, ] = as.numeric(this_vec[ifelse(deal_with_tag, 4, 3):length(mat_header)])
      row_labs = c(row_labs, paste0(this_vec[1], "_", this_vec[2], ifelse(deal_with_tag, paste0("_",this_vec[3]), "")))
    }
    rownames(Mat) = row_labs
    states[[year]] <- Mat
    
    
    if (!casal.regexp.in(file, "State at the end of year")) 
      break
  }
  return(states)
}

