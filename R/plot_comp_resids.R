#' plot_comp_resids 
#' @description A function that generates a plot of residuals for compositional data.
#' @author C.Marsh
#' @param resids_matrix a matrix[years,bins] that are standardised residuals from compositional data, rownames(resids_matrix) = years (y-axis), colnames(resids_matrix) = ages/length
#' @return a ggplot bubble plot
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_point scale_colour_manual scale_fill_manual scale_size_continuous theme
#' @examples 
#' \dontrun{
#' mpd = extract.mpd(path = system.file("extdata",package = "stockassessmenthelper"), file = "estimate.log")
#' colnames(mpd$fits$chatTANage$pearson.resids) = as.numeric(substring(colnames(mpd$fits$chatTANage$pearson.resids), first = 2))
#' plt = plot_comp_resids(resids_matrix = as.matrix(mpd$fits$chatTANage$pearson.resids), show_legend  = T)
#' }
#' @export
plot_comp_resids = function(resids_matrix, show_legend = TRUE) {
  df_melted = melt(resids_matrix)
  df_melted$year = df_melted$Var1
  df_melted$variable = df_melted$Var2
  df_melted$outliers = ifelse(abs(df_melted$value) >= 2, "Outliers (>2)", ifelse(df_melted$value <= 0,"Negative", "Positive"));
  df_melted$sign = ifelse(df_melted$value <= 0, "Negative", "Positive")
  df_melted$plotted_var = df_melted$value
  ## truncate for plotting
  df_melted$plotted_var[df_melted$plotted_var < -2] = -2
  df_melted$plotted_var[df_melted$plotted_var > 2] = 2
  
  gg_image = ggplot(df_melted, aes(x = as.integer(variable), y = year, size =  abs(plotted_var), col = sign, fill = outliers)) +
    geom_point(shape=21, stroke = 2) + 
    scale_colour_manual(name = 'Sign', values = c("Positive"="#00BFC4","Negative"="#F8766D")) +
    scale_fill_manual(name="Outliers",values=c("Outliers (>2)"="gray60","Positive"="#00BFC4","Negative"="#F8766D")) +
    scale_size_continuous(breaks = c(0,0.5,1,1.5,2), labels = c("0","0.5","1","1.5","2>"), name = "Scale") + 
    ylab("Years") +
    xlab("bins") 
  
  if (!show_legend)
    gg_image = gg_image + theme(legend.position="none")
  
  print(gg_image)
  return(gg_image)
}