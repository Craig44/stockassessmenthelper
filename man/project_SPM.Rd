% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surplus_production_funs.R
\name{project_SPM}
\alias{project_SPM}
\title{project_SPM}
\usage{
project_SPM(
  fixed_effect_estimates = vector(),
  covariance_estimates = matrix(),
  proj_years,
  m,
  n_sims,
  B_Y = list()
)
}
\arguments{
\item{fixed_effect_estimates}{<vector> MLE estimates}

\item{covariance_estimates}{<matrix> MLE standard errors, must match order of parameters fixed_effect_estimates}

\item{proj_years}{int number of projection years}

\item{m}{Shape parameter for the surplus production function}

\item{n_sims}{int number random draws}

\item{B_Y}{list 'mu' MLE derived quantity and 'sd'}
}
\value{
Returns a list of model quantiteis and observations that can be used as an operating model
}
\description{
Given an estimated model, will project forward proj_years and simulate data. Uncertainty comes from estimated parameters, as well as the process error which is assumed to be estimated
}
