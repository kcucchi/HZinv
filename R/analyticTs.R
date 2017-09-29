

#' Applies the analytical formula to calculate temperature at one depth, time combination
#'
#' @param param the vector of parameters
#' @param t (in s) time at which to calculate the temperature
#' @param z (in m) a vector of depths at which to calculate the temperature
#' Depths need to be between 0 and maximum depth.
#' @return temperature at depth z and time t for parameters param
#' @export
calculateSynthetic_timeDepth <- function(param,t,z){

  # following notations in Luce 2013

  # define parameters
  rho_mTimesc_m <-
    as.numeric(param['n'] * param['rho_w'] * param['c_w'] +
                 (1-param['n']) * param['rho_s'] * param['c_s'])
  lambda_m = HZinv::calculate_lambda_m(param = param)
  param_eq <- HZinv::calculate_eq(param = param)

  # Darcy with positive flux downwards
  q <- param['permeability'] * param['rho_w'] * param['g'] / param['mu_w'] *
    param['dH'] / param['z_bottom']

  # areally averaged rate of heat movement
  v_t <-  as.numeric(q * param['rho_w'] * param['c_w'] / rho_mTimesc_m)
  omega <- 2 * pi / param['period_T']
  alpha <- as.numeric(sqrt ( v_t^4 + (4 * omega * param_eq['kappa_e'])^2 ))

  a = 1/(2*param_eq['kappa_e']) * (sqrt((alpha + v_t^2)/2) - v_t)
  b = 1/(2*param_eq['kappa_e']) * sqrt((alpha - v_t^2)/2)

  res <- data.frame(t=numeric(0),
                    z=numeric(0),
                    temperature=numeric(0))

  for(i in 1:length(z)){
    res <- rbind(res,
                 data.frame(t=t,
                            z=z[i],
                            temperature = param['T_mu'] +
                              param['A'] * exp(a*z[i]) * cos(omega * t + b * z[i])))
  }

  return(res)

}

#' Applies the analytical formula to calculate temperature time series at one depth
#'
#' @param param the vector of parameters
#' @param z (in m) a vector of depths at which to calculate the temperature
#' @param nbDays (in days) length of output time series
#' @param Deltat (in s) time resolution of output time series
#' @return temperature time series at depth z
#' @export
calculateSyntheticTs <- function(param,z,nbDays=3){

  # generate vector of time values
  # starts on 01/01/2010
  # default timezone is "GMT", which in R means "UTC" for some reason
  # UTC does not have daylight saving times, that's what we want
  df_t <- data.frame(
    t_time=seq.POSIXt(from = ISOdate(year = 2000,month = 01,day = 01, hour = 0),
                      to = ISOdate(year = 2000,month = 01,day = 01+nbDays),
                      by = param['sampling_period']))

  # calculate time vector in seconds
  df_t$t_seconds <-
    as.numeric(difftime(
      time1 = df_t$t_time,
      time2 = df_t$t_time[1], # time at origin
      units = "secs"))

  # call calculateSynthetic_timeDepth to calculate result
  res <-
    calculateSynthetic_timeDepth(param = param,
                                 t = df_t$t_seconds,
                                 z = z)

  # join POSIXct vector to res dataframe
  res <- dplyr::left_join(x = res,
                          y = df_t,
                          by = c("t"="t_seconds"))

  # add metaparameters for vector of depths
  # first define name of indices
  string_z_idx <- paste0('z_',c('top',1:(length(z)-2),'bottom'))
  # turn the names into a factor
  factor_z_idx <-
    factor(x = string_z_idx,
           levels = string_z_idx)
  # this dataframe does the correspondance
  # between the depths and the name of the indices
  df_depth2idx <-
    data.frame(z = sort(z,decreasing = T), # vector of depths in decreasing order
               z_idx = factor_z_idx) # factor of depths indices

  # join to result dataframe
  res <- dplyr::left_join(x = res,
                          y = df_depth2idx,
                          by = "z")

  return(res[,c('t','t_time','z','z_idx','temperature')])

}

