#' @title run_reweight
#' @description A function for automatically runnning compositional reweighting code. It will overwrite the estimation.csl file so please read the details before running the function
#' @details before running this code you will need to save the original estimation.csl as this function will overwrite the est_csl_file file.
#' Applies the Method.TA1.8 method in francis
#' 
#' @param csl_path directory path to where csl files are and where estimation will occur
#' @param est_csl_file label for the estimation csl file i.e. estimation.csl
#' @param comp_obs vector of strings that relate to block labels for the composition observations, that are consistent with csl file definitions i.e. EN_age
#' @param est_file estimated output from the casal estimation run
#' @param additional_casal_pars string of additional casal parameters i.e. "-q" to run quiet or "f my_stock_" if you have odd labeled csl files 
#' @param tolerance difference from 1 to which we don't reweight the observation class
#' @param iters number of reweighting iterations to run, for you to manually tweak.
#' @param min_years <int> minimum years that are needed to weight this data.
#' @return nothing, this function writes to files and estimates, you will want to look at the estimated output after to see the effects
#' @export
#' @importFrom casal extract.mpd
#' @examples
#' \dontrun{
#' path = csl_path = "../csl"
#' file = est_csl_file = "K12014_estimation.csl"
#' est_file = "est_upd.out"
#' comp_obs = c("REC_AGE", "SN_AGE", "PS_AGE", "ST_AGE")
#' additional_casal_pars = "-f K12014_"
#' tolerance = 0.001
#' iters = 5
#' }

run_reweight <- function(csl_path, est_csl_file, comp_obs, est_file, additional_casal_pars, tolerance = 0.01, iters = 5, min_years = 3) {
  current_wd = getwd()
  csl_path = normalizePath(csl_path)
  est_out = extract.mpd(file = est_file, path = csl_path)
  #plot(est_out$quantities$SSBs$year, est_out$quantities$SSBs$SSB, type ="l", lwd = 3)
  
  for(k in 1:iters) {
    est_out = extract.mpd(file = est_file, path = csl_path)
    #lines(est_out$quantities$SSBs$year, est_out$quantities$SSBs$SSB, col = "red", lwd = 3)
    est_csl = extract.csl.file(file = est_csl_file, path = csl_path)
    weights = rep(1.0, length(comp_obs))
    names(weights) = comp_obs
    ## loop over each observation
    for(i in 1:length(comp_obs)) {
      # find the observation in estimate output
      ndx = which(grepl(pattern = comp_obs[i],  x = names(est_out$fits)))
      if (length(ndx) != 1)
        stop(paste0("couldn't find unique observation ", comp_obs[i], " from estimated output file ", est_file))
      # find the weight if enough years
      if(length(est_out$fits[[ndx]]$year) < min_years) {
        next;
      }
      bin_labs = as.numeric(substring(colnames(est_out$fits[[ndx]]$obs), first = 2))
      weight = Method.TA1.8(bin_lab = bin_labs, observed = t(est_out$fits[[ndx]]$obs), expected = t(est_out$fits[[ndx]]$fits), error_value = est_out$fits[[ndx]]$'error.value'[,1], plot.it = F)
      weights[i] = weight
      
      if(weight < 0.2) 
        weight = 0.2
      if(weight > 5) 
        weight = weight
      if(abs(weight - 1) > tolerance) {
        # find the observation in estimation.csl
        ndx = which(grepl(pattern =comp_obs[i],  x = names(est_csl)))
        if (length(ndx) != 1)
          stop(paste0("couldn't find unique observation ", comp_obs[i], " from csl output file ", est_csl_file))
        this_ob = est_csl[[ndx]]
        error_ndx = which(grepl(pattern = "N_", names(this_ob)))
        for(j in 1:length(error_ndx)) {
          this_ob[[error_ndx[j]]] = as.numeric(this_ob[[error_ndx[j]]]) * weight
        }
        est_csl[[ndx]] = this_ob
      }
    }
    ## write estimation csl file out 
    write.csl.file(object = est_csl, file = est_csl_file, path = csl_path)
    print(paste0("k = ", k))
    print(weights)
    ## run Casal
    setwd(csl_path)
    system2(command = "casal",  args = paste0("-e ", additional_casal_pars), stdout = est_file, stderr = "err.out")
    setwd(current_wd)
  }
}


#' @title Method.TA1.8
#' @description
#' This function is useful for deciding on the data weights of one or more at-age or at-length data sets with assumed multinomial error structure in a stock assessment. Can produce a diagnostic plot if the analysis is for a single data set
#'
#' @author Chris Francis
#' @param bin_lab vector of bin labels, either ages or length mid-point
#' @param observed matrix of composition (internally resacaled in each year to be proportions) with rows = bins (ages or lengths) and cols years
#' @param expected matrix of composition (internally resacaled in each year to be proportions) with rows = bins (ages or lengths) and cols years
#' @param error_value vector of effective sample sizes
#' @param plot.it If TRUE, plot the index and the smoothed fit. Otherwise, return a dataframe of the year, index, smoothed fitted value, and cv)
#'
#' @return Outputs a mutiplier, w, so that N2y = w x N1y, where N1y and N2y are the stage-1 and stage-2 multinomial sample sizes for the data set in year y.
#'
#' @note Method TA1.8 is described in Appendix A of the following paper Francis, R.I.C.C. (2011). Data weighting in statistical fisheries stock assessment models.
#' Canadian Journal of Fisheries and Aquatic Sciences 68: 1124-1138. (With corrections to the equation in Francis R.I.C.C. (2011) Corrigendum: Data weighting in statistical fisheries stock assessment models.
#' @export

Method.TA1.8 <- function(bin_lab, observed, expected, error_value, plot.it = FALSE) {
  ## rescale to sum = 1
  Obs <- sweep(observed, 2, colSums(observed), "/")
  Exp <- sweep(expected, 2, colSums(expected), "/")
  Nassumed <- error_value
  Ry = Sy = c()
  
  ##aa <- as.numeric(strsplit(dimnames(Obs)[[2]],split = "\\["), nchar(dimnames(Obs)[[2]])))
  My <- cbind(Obs = apply(Obs, 2, function(x) sum(bin_lab * x)),
              Exp = apply(Exp, 2, function(x) sum(bin_lab * x)))
  Ry <- c(Ry, My[, "Obs"] - My[, "Exp"])
  Sy <- c(Sy, sqrt(apply(Exp, 2, function(x) sum(x * bin_lab^2)) - My[, "Exp"]^2))
  
  
  wj <- 1/var(Ry * sqrt(Nassumed)/Sy, na.rm = T)
  
  if (plot.it) {
    ses <- Sy/sqrt(Nassumed)
    Obs.bnds <- My[, "Obs"] + cbind(-2 * ses, 2 * ses)
    
    if (is.null(ylim)) {
      ylim <- range(Obs.bnds)
    }
    if (is.null(xlim)) {
      xlim <- range(years)
    }
    
    plot(years, My[, "Obs"], type = "n", ylab = "", xlab = "", xlim = xlim, ylim = ylim, las = 1)
    points(years, My[, "Obs"], pch = "x", col = 3)
    segments(years, Obs.bnds[, 1], years, Obs.bnds[, 2], col = 3)
    lines(years, My[, "Exp"], col = "red")
  }
  
  return (wj)
}
