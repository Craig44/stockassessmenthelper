#' compare_AF between an abm output and CASAL model run
#' @export
#' @param cas_AFs a list object that has been generated from the function extract.AFs() or extract.step.AFs()
#' @param abm_output an ABM model output
#' @param abm report label abm_report_lab
#' @param year_of_interest 
#' @param region assumes regions are consistently labelled between Casal model and ABM
#' @param plotit whether you want this function to create a ggplot or return a long form data frame
#' @importFrom ggplot2 ggplot ggtitle geom_line scale_y_continuous
compare_AF = function(cas_AFs, abm_output = abm, abm_report_lab = "age_freq_step1", year_of_interest, region, plotit = F, ylim = NULL) {
  this_cas = cas_AFs[[as.character(year_of_interest)]]
  labs = rownames(this_cas)
  area = Reduce(c, lapply(strsplit(labs, split = "_"), FUN = function(x){x[2]}))
  stock = Reduce(c, lapply(strsplit(labs, split = "_"), FUN = function(x){x[1]}))
  this_cas_AF = colSums(this_cas[area %in% region, ])
  this_abm = get(x = abm_report_lab, abm_output)
  this_abm_AF = this_abm[[as.character(year_of_interest)]]$values[get_reg_ndx(region), ]
  df = data.frame(Numbers = c(as.numeric(this_abm_AF), this_cas_AF), age = as.numeric(rep(colnames(this_abm_AF),  2)), model = c(rep(c("ABM", "CASAL"), each = length(this_abm_AF))))
  if(plotit) {
    plt = ggplot(df, aes(x = age, y = Numbers, col = model, linetype = model)) +
      geom_line(size = 1.5) +
      ggtitle(paste0(region,"-", year_of_interest)) 
    if(!is.null(ylim))
      plt + scale_y_continuous(limits = ylim)
    
    
    print(plt)
  } else {
    return(df)
  }
}
