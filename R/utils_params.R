

#' Calculate equivalent thermal conductivity from vector of parameters according to the wood formula
#'
#' @param param the vector of parameters
#' @return the equivelent thermal conductivity
#' @export
calc_lambda_m <- function(param){

  res <- param[['n']] * sqrt(param[['lambda_w']]) +
    (1 - param[['n']]) * sqrt(param[['lambda_s']])^2

  return (as.numeric(res))

}

#' Calculate equivalent reduced parameters from vector of parameters
#'
#' @param param the vector of parameters
#' @return the equivalent reduced parameters
#' @export
calc_eq <- function(param){

  lambda_m <- calc_lambda_m(param)

  rho_m_c_m <-
    param[['n']] * param[['rho_w']] * param[['c_w']] +
    (1 - param[['n']]) * param[['rho_s']] * param[['c_s']]

  kappa_e <- lambda_m / rho_m_c_m

  alpha_e <- param[['rho_w']] * param[['c_w']] / rho_m_c_m *
    param[['permeability']] * param[['rho_w']] * param[['g']] / param[['mu_w']]

  return(c(kappa_e = kappa_e,
           alpha_e = alpha_e))

}

#' Calculate range of reduced parameters from vector of parameters
#'
#' @param param the vector of parameters
#' @return the range of reduced parameters
#' @export
calc_range_eq <- function(param){

  # brute force - calculate all combinations

  # create all combinations
  df_all_comb <-
    expand.grid(
      n = c(param[['n_min']],
            param[['n_max']]),
      permeability = 10^c(param[['permeability_log10_min']],
                          param[['permeability_log10_max']]),
      lambda_s = c(param[['lambda_s_min']],
                   param[['lambda_s_max']]),
      rho_s = c(param[['rho_s_min']],
                param[['rho_s_max']]),
      c_s = c(param[['c_s_min']],
              param[['c_s_max']])
    )

  # for all combinations calculate reduced parameters
  # create fields for reduced parameters
  df_all_comb$kappa_e <- NA; df_all_comb$alpha_e <- NA
  # apply calc_eq on each row
  for(i in 1:nrow(df_all_comb)){
    # save row i as named vector
    comb_i <- as.vector(df_all_comb[i,])
    # replace values in param vector
    param_i <- replace(x = param,list = names(comb_i),values = comb_i)
    # calculate reduced parameters
    param_eq <- calc_eq(param = param_i)
    # fill in in corresponding fields in df_all_comb
    df_all_comb$kappa_e[i] <- param_eq[['kappa_e']]
    df_all_comb$alpha_e[i] <- param_eq[['alpha_e']]
  }

  # result are max and min values
  return(c(kappa_e_min = min(df_all_comb$kappa_e),
           kappa_e_max = max(df_all_comb$kappa_e),
           alpha_e_min = min(df_all_comb$alpha_e),
           alpha_e_max = max(df_all_comb$alpha_e)))

}


#' Calculate Peclet number from vector of parameters
#'
#' @param param the vector of parameters
#' @return the Peclet number
#' @export
calc_Peclet <- function(param){

  param_eq <- calc_eq(param)

  return( calc_Peclet_eq(param_eq = param_eq,param = param) )
}


#' Calculate Peclet number from vector of equivalent parameters
#'
#' @param param the vector of parameters
#' @return the Peclet number
#' @export
calc_Peclet_eq <- function(param_eq,param){
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


