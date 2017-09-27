

#' Applies the analytical formula to calculate temperature at one depth, time combination
#'
#' @param param the vector of parameters
#' @param t time at which to calculate the temperature
#' @param z a vector of depths at which to calculate the temperature.
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
  omega <- 2 * pi / param['period']
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
#' @param z depth at which to calculate the temperature
#' @param nbDays length of output time series, in days
#' @param Deltat time resolution of output time series
#' @return temperature time series at depth z
#' @export
calculateSyntheticTs <- function(param,z,nbDays=3,Deltat=15*60){

  t_seconds = seq(from=0,
                  to=nbDays*param['period'],
                  by=Deltat)


  return(calculateSynthetic_timeDepth(param = param,
                                      t = t_seconds,
                                      z = z))

}

