% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lognormal_funs.R
\name{r_lnorm}
\alias{r_lnorm}
\title{r_lnorm Generates}
\usage{
r_lnorm(N, expectation, cv = NULL, sigma = NULL)
}
\arguments{
\item{N}{<int>: number of draws}

\item{expectation}{<double>: The expectation of the distribution, this is not the mu parameter because for the lognormal distribution the expectation is not the expectation parameter.}

\item{cv}{<double>: The coefficent of variation of the distribution}

\item{sigma}{<double>: The standard deviation of the distribution (in log space)}
}
\description{
iid lognormal draws with expectation (note not the same as the mu parameter) and sigma in the same manner as CASAL
}
