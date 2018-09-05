#' uses francis method of calculating CV's for an index with no CI's or in most cases with unrealistically small Standard errors
#' from the large dataset's used in these analysis. This function has been modified based on suggestions by the PAUA assessment review
#' Butterworth et.al. (2015)

#' @author Dan Fu
#' @export
CV.for.CPUE = function (year, cpue, f=0, df=0, plot.it = TRUE)  {
  CV.for.lognormal.error <- function(yobs, yfit) {
    lognormal.neg.log.likl <- function(cv, obs, fit) {
      sd <- sqrt(log(1 + cv^2))
      neg.log.likl <- sum(log(sd) + 0.5 * (0.5 * sd + log(obs/fit)/sd)^2)
      neg.log.likl
    }
    est.cv <- optimize(lognormal.neg.log.likl, c(0.01, 0.9), 
        obs = yobs, fit = yfit)$minimum
    return(est.cv)
  }
  n = length(year)
  if(f==0 && df==0) {
    stop("f==0 && df==0")
  } else if (f!=0 && df!=0) {
    stop("f!=0 && df!=0")
  }
    
  if (f!=0) 
    fit = lowess(year,cpue,f=f)
  else
    fit <- smooth.spline(year, cpue, df = df)
  CV <- CV.for.lognormal.error(cpue, fit$y)
  if (plot.it) {
    plot(year, cpue, pch = "x", ylim = c(0, 1.05 * max(c(cpue, fit$y))), yaxs = "i", xlab = "", ylab = "")
    lines(fit)
    return(CV)
  }
  if(f!=0)
    return(data.frame(year = year, cpue = cpue, fit = fit$y, cv = rep(CV, length(year))))
  else
    return(data.frame(year = year, cpue = cpue, fit = fit$y, cv = rep(CV, length(year)), cv2= rep(CV*n/(n-df), length(year))))
  
  
}