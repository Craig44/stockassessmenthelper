
#' extract.step.AFs State at the end of year and a step
#' @export
#' @param path path to the filename 
#' @param file casal output filename
#' @return list of AFs
extract.step.AFs = function (file, step = 1, path = "")  {
  states <- list()
  if (missing(path)) 
    path <- ""
  filename <- casal.make.filename(path = path, file = file)
  file <- casal.convert.to.lines(filename)
  line_ndx = casal.regexpr(paste0("State at the end of step ",step," in year"),  file)
  if (all(line_ndx < 0)) {
    cat(paste("No output data found in file '", filename, 
              "'", sep = ""))
    return(NA)
  }
  file <- casal.get.lines(file, from = which(line_ndx >= 0)[1], to = which(line_ndx >= 0)[sum(line_ndx >= 0)])
  
  while (1) {
    if (length(file) == 0) 
      break
    header <- casal.get.lines(file, contains = paste0("State at the end of step ",step," in year"))[1]
    file <- casal.get.lines(file, clip.to.match = paste0("State at the end of step ",step," in year"))
    year = substring(header, first = 36, last = 39)
    end_of_section = which(casal.regexpr("SSBs:",  file) >= 0)[1]
    temp <- file[2:(end_of_section - 1)]
    mat_header = casal.string.to.vector.of.words(temp[1])
    deal_with_tag = FALSE
    deal_with_sex = FALSE
    if("tag" %in% mat_header)
      deal_with_tag = TRUE
    if("sex" %in% mat_header)
      deal_with_sex = TRUE
    row_labs = NULL
    Mat = matrix(ncol = length(mat_header)-ifelse(deal_with_sex, 1, 0) - ifelse(deal_with_tag, 1, 0), nrow = length(temp) - 1)
    for(i in 2:(length(temp))) {
      this_vec = casal.string.to.vector.of.words(temp[i])
      start_col = sum(deal_with_tag + deal_with_sex) + 1
      Mat[i - 1, ] = as.numeric(this_vec[start_col:length(mat_header)])
      row_labs = c(row_labs, paste0(this_vec[1:(start_col - 1)], collapse = "_"))
    }
    rownames(Mat) = row_labs
    states[[year]] <- Mat
    
    
    if (!casal.regexp.in(file, paste0("State at the end of step ",step," in year"))) 
      break
  }
  return(states)
}