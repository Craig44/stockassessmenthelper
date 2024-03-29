% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth_params.R
\name{linear_growth_increment_model}
\alias{linear_growth_increment_model}
\title{linear_growth_increment_model}
\usage{
linear_growth_increment_model(length_mid_points, L1, L2, g1, g2)
}
\arguments{
\item{length_mid_points}{vector of lengths to calculate growth increment}

\item{L1}{length at reference 1, corresponds to g1}

\item{L2}{length at reference 2, corresponds to g2}

\item{g1}{growth rate at L1}

\item{g2}{growth rate at L2}
}
\value{
vector mean growth increment for each length bins
}
\description{
linear_growth_increment_model
}
\details{
Growth increment model based on the transposed VB formulation based on \insertCite{francis1988maximum}{stockassessmenthelper}
}
\references{
\insertAllCited{}
}
