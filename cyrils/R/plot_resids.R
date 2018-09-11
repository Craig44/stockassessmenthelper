#' plot.resids A function that generates a plot of residuals for compositional data.
#'
#' @author C.Marsh
#' @param resids_matrix a matrix[years,bins] that are standardised residuals from compositional data
#' @param bin_labels numeric vector of values for the columns assumed either age or length
#' @param years numeric vector of years for each row.
#' @return a ggplot image.
#' @export
plot_resids = function(resids_matrix, bin_labels, years, show_legend = TRUE) {
  if (nrow(resids_matrix) != length(years)) {
    stop(paste0("rows of matrix = '", nrow(resids_matrix),"' and number of years supplied = '",length(years),"' these must be the same"))
  }
  if (ncol(resids_matrix) != length(bin_labels)) {
    stop(paste0("cols of matrix = '", ncol(resids_matrix),"' and number of bins supplied = '",length(bin_labels),"' these must be the same"))
  }
  
  df = data.frame(resids_matrix)

  df$years = years
  colnames(df) = c(bin_labels,"year")
  df_melted = melt(df, id = "year")
  df_melted$variable = as.numeric(as.character(df_melted$variable))
  df_melted$year = as.character(df_melted$year)
  
  gg_image = ggplot(df_melted, aes(x = year, y = variable)) +
    geom_point(aes(size = abs(value), shape = ifelse(abs(value) <= 2, "less than 2","greater than 2"), colour = ifelse(value < 0,"negative","positive"))) + 
    scale_colour_manual(name = 'Sign', values = setNames(c('red','black'),c("negative", "positive"))) +
    scale_shape_manual(name = 'Outliers', values = setNames(c(19,17),c("less than 2", "greater than 2"))) +
    scale_size_continuous(name = "Size",breaks = c(0,0.5,1,1.5,2,7)) +  # turn off size legend
    xlab("Years") +
    ylab("bins") 
  
  if (!show_legend)
    gg_image = gg_image + theme(legend.position="none")
    
  print(gg_image)
  return(gg_image)
}
