% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surplus_production_funs.R
\name{surplus_production_model_sigma}
\alias{surplus_production_model_sigma}
\title{surplus_production_model_sigma}
\usage{
surplus_production_model_sigma(
  seed = 123,
  n_t,
  B1,
  r,
  k,
  catches,
  obs_std,
  catchability,
  observation_likelihood = 1,
  process_error = NULL,
  process_std = NULL
)
}
\arguments{
\item{seed}{integer random number seed}

\item{n_t}{<integer> number of time-steps to run the model}

\item{B1}{<real> starting exploited biomass}

\item{r}{<real> intrinsic growth parameter.}

\item{k}{<real> carrying capacity parameter.}

\item{catches}{<vector> observed catches, defines number of years, so if you have a period of no catches set values of 0, number of years = length(catches+ 1)}

\item{obs_std}{<real> Standard deviation which is used to generate simulated observations.}

\item{catchability}{<vector> a scalar to generate the relative index observation}

\item{observation_likelihood}{<int> likelihood type, 1 = lognormal, 2 = normal, 3 = ...}

\item{process_error}{(optional) time specific time deviations (epsilon_t,p) for each time step}

\item{process_std}{(optional) standard deviation for process deviations corrections}
}
\value{
Returns a list of model quantities and observations that can be used as an operating model
}
\description{
simulates a surplus production model can be used as an operating model, Different from the above uses a standard deviation for process error instead of CV
}
