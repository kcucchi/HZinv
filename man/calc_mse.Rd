% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_likelihood.R
\name{calc_mse}
\alias{calc_mse}
\title{Calculates the mse
between measurements and simulations}
\usage{
calc_mse(df_all, date_min = NULL, center = F)
}
\arguments{
\item{df_all}{the dataframe containing measurements (in meas)
and simulations (in columns starting with sim)}

\item{date_min}{date specifying start of rmse calculation, before this date
values will be discarded (given as POSIXct object)}

\item{center}{boolean indicating whether to center timeseries before
calculating the rmse}
}
\value{
a dataframe containing mse values for each simulation
}
\description{
Calculates the mse
between measurements and simulations
}

