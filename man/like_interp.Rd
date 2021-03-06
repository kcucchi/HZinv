% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_likelihood.R
\name{like_interp}
\alias{like_interp}
\title{This file contains functions to calculate likelihoods.}
\usage{
like_interp(df_eqGrid_like, df_eq_eval)
}
\arguments{
\item{df_eqGrid_like}{the dataframe containing
the likelihood (column like) evaluated for
combinations of alpha_e (column alpha_e)
and kappa_e (column kappa_e)}

\item{df_eq_eval}{the dataframe containing
combinations of alpha_e and kappa_e where to evaluate
the likelihood}
}
\value{
the df_eq_eval dataframe
containing a new column with likelihoods
}
\description{
Interpolate the likelihood from the grid
}

