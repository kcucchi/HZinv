
#'Generates sequence of values at center of intervals
#'
#'@param n length of the desired vector
#'@param val_min minimum value
#'@param val_max maximum value
#'@param isLog10 generates on the log10 scale
#'@return the sequence of values
#'@export
gen_atCenter <- function(n,val_min,val_max,isLog10=F){

  if(isLog10){val_min <- log10(val_min);val_max <- log10(val_max)}

  val_delta <- (val_max - val_min) / n

  res <- seq(from = val_min + val_delta/2,
             to = val_max - val_delta/2,
             by = val_delta)

  if(isLog10){res <- 10^res}

  return(res)

}


#' Generates combinations of physical parameters from ranges
#' at center of grids
#'
#'@param param the vector of parameters
#'@param nb_lambda_s the number of lambda_s values
#'@param nb_permeability the number of permeability values
#'@param nb_rho_s the number of rho_s values
#'@param nb_c_s the number of c_s values
#'@param nb_n the number of n values
#'@return a dataframe of combinations of parameters
#'@export
gen_comb_phy_atCenter <- function(param,
                                  nb_lambda_s=5, nb_permeability=10,
                                  nb_rho_s=2, nb_c_s = 2, nb_n = 2){

  res <-
    data.frame(expand.grid(
      lambda_s =
        HZinv::gen_atCenter(
          val_min = param[['lambda_s_min']],
          val_max = param[['lambda_s_max']],
          n = nb_lambda_s),
      rho_s =
        HZinv::gen_atCenter(
          val_min = param[['rho_s_min']],
          val_max = param[['rho_s_max']],
          n = nb_rho_s),
      c_s =
        HZinv::gen_atCenter(
          val_min = param[['c_s_min']],
          val_max = param[['c_s_max']],
          n = nb_c_s),
      n =
        HZinv::gen_atCenter(
          val_min = param[['n_min']],
          val_max = param[['n_max']],
          n = nb_n),
      permeability =
        HZinv::gen_atCenter(
          val_min = param[['permeability_min']],
          val_max = param[['permeability_max']],
          n = nb_permeability,
          isLog10 = T)
    ))

  # add idx for later in case
  res$idx_phy <- paste0('phy_',1:nrow(res))

  return(res)

}


#' Generates combinations of physical parameters from ranges
#' at border of grids (ie covering the entire space)
#'
#'@param param the vector of parameters
#'@param nb_lambda_s the number of lambda_s values
#'@param nb_permeability the number of permeability values
#'@param nb_rho_s the number of rho_s values
#'@param nb_c_s the number of c_s values
#'@param nb_n the number of n values
#'@return a dataframe of combinations of parameters
#'@export
gen_comb_phy_atBorder <- function(param,
                                  nb_lambda_s=5, nb_permeability=10,
                                  nb_rho_s=2, nb_c_s = 2, nb_n = 2){

  res <-
    data.frame(expand.grid(
      lambda_s =
        seq(
          from = param[['lambda_s_min']],
          to = param[['lambda_s_max']],
          length.out = nb_lambda_s),
      rho_s =
        seq(
          from = param[['rho_s_min']],
          to = param[['rho_s_max']],
          length.out = nb_rho_s),
      c_s =
        seq(
          from = param[['c_s_min']],
          to = param[['c_s_max']],
          length.out = nb_c_s),
      n =
        seq(
          from = param[['n_min']],
          to = param[['n_max']],
          length.out = nb_n),
      permeability =
        emdbook::lseq(
          from = param[['permeability_min']],
          to = param[['permeability_max']],
          length.out = nb_permeability)
    ))

  # add idx for later in case
  res$idx_phy <- paste0('phy_',1:nrow(res))

  return(res)

}
