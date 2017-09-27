

#' Calculate equivalent thermal conductivity from vector of parameters according to the wood formula
#'
#' @param param the vector of parameters
#' @return the equivelent thermal conductivity
#' @export
calculate_lambda_m <- function(param){

  return ( ( param$n * sqrt(param$lambda_w) +
               (1 - param$n) * sqrt(param$lambda_s))^2)

}

#' Calculate equivalent reduced parameters from vector of parameters
#'
#' @param param the vector of parameters
#' @return the equivalent reduced parameters
#' @export
calculate_eq <- function(param){

  lambda_m <- calculate_lambda_m(param)

  rho_m_c_m <-
    param$n * param$rho_w * param$c_w +
    (1 - param$n) * param$rho_s * param$c_s

  kappa_e <- lambda_m / (rho_m_c_m)

  alpha_e <- rho_w * c_w / rho_m_c_m *
    param$permeability * param$rho_w * param$g / param$mu_w

  return(list(kappa_e = kappa_e,
              alpha_e = alpha_e))

}


#' Calculate Peclet number from vector of parameters
#'
#' @param param the vector of parameters
#' @return the Peclet number
#' @export
calculatePeclet <- function(param){
  param_eq <- calculate_eq(param)

  return( calculatePeclet_eq(param_eq = param_eq,param = param) )
}


#' Calculate Peclet number from vector of equivalent parameters
#'
#' @param param the vector of parameters
#' @return the Peclet number
#' @export
calculatePeclet_eq <- function(param_eq,param){
  return( abs ( param_eq$alpha_e / param_eq$kappa_e * param$dH) )
}
