

#' Calculate equivalent thermal conductivity from vector of parameters according to the wood formula
#'
#' @param param the vector of parameters
#' @return the equivelent thermal conductivity
#' @export
calculate_lambda_m <- function(param){

  res <- param['n'] * sqrt(param['lambda_w']) +
    (1 - param['n']) * sqrt(param['lambda_s'])^2

  return ( c(lambda_m = as.numeric(res)))

}

#' Calculate equivalent reduced parameters from vector of parameters
#'
#' @param param the vector of parameters
#' @return the equivalent reduced parameters
#' @export
calculate_eq <- function(param){

  lambda_m <- calculate_lambda_m(param)

  rho_m_c_m <-
    as.numeric(param['n'] * param['rho_w'] * param['c_w'] +
                 (1 - param['n']) * param['rho_s'] * param['c_s'])

  kappa_e <- as.numeric(lambda_m / (rho_m_c_m))

  alpha_e <- as.numeric(param['rho_w'] * param['c_w'] / rho_m_c_m *
    param['permeability'] * param['rho_w'] * param['g'] / param['mu_w'])

  return(c(kappa_e = kappa_e,
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
  res <- abs ( param_eq['alpha_e'] / param_eq['kappa_e'] * param['dH'])
  return( as.numeric(res) )
}


#' Calculate table of correspondance between z and z_idx
#' from the vector of parameters
#'
#' @param param the vector of parameters
#' @return the table of correspondance
#' @export
get_level_z_fromParam <- function(param){

  # get names of fields starting with "z_"
  get_z_idx <- names(param)[grep(pattern = '^z_',x = names(param))]

  # define the vector of levels (i.e. sort get_z_idx)
  level_z_idx <- c("z_top",paste0("z_",1:(length(get_z_idx)-2)),"z_bottom")

  factor_z_idx <- factor(get_z_idx,levels = level_z_idx)

  # result is correspondance between
  res <- data.frame(z = param[get_z_idx],
             z_idx = factor_z_idx)

  return(res)

}


