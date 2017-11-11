

#' Calculate equivalent thermal conductivity from vector of parameters according
#' to the wood formula
#'
#' @param param the vector of parameters
#' @return the same vector of parameters param with updated lambda_m field
#' @export
calc_lambda_m <- function(param){

  in_parenthesis <-
    param[['n']] * sqrt(param[['lambda_w']]) +
    (1 - param[['n']]) * sqrt(param[['lambda_s']])

  param['lambda_m'] <- in_parenthesis^2

  return(param)

}

#' Calculate rho_m * c_m from vector of parameters
#'
#' @param param the vector of parameters
#' @return the same vector of parameters param with updated lambda_m field
#' @export
calc_rho_m_c_m <- function(param){

  param['rho_m_c_m'] <-
    param[['n']] * param[['rho_w']] * param[['c_w']] +
    (1 - param[['n']]) * param[['rho_s']] * param[['c_s']]

  return(param)

}

#' Calculate reduced parameters from physical parameters
#'
#' @param param the vector of parameters
#' @return the same vector of parameters param with updated rho_m_c_m field
#' @export
calc_eq <- function(param){

  # complete kappa_e field
  param['kappa_e'] <- param[['lambda_m']] / param[['rho_m_c_m']]

  # complete alpha_e field
  param[['alpha_e']] <-
    param[['rho_w']] * param[['c_w']] / param[['rho_m_c_m']] *
    param[['permeability']] * param[['rho_w']] * param[['g']] / param[['mu_w']]

  return(param)

}

#' Updates the definition of lambda_m, rho_m_c_m and reduced parameters
#' @param param the vector of parameters
#' @return param the updated vector of parameters
#' @export
update_all_eq <- function(param){

  # update definition of lambda_m
  param <- calc_lambda_m(param = param)

  # update definition of rho_m_c_m
  param <- calc_rho_m_c_m(param = param)

  # update definition of equivalent parameters
  param <- calc_eq(param = param)

  # return result
  return(param)
}

#'
#' Inverse operation: calculate lambda_s from vector of parameters
#' @param param the vector of parameters
#' @return param with updated lambda_s field
#' @export
calc_inv_lambda_s <- function(param){

  # update rho_m_c_m value in case
  param <- calc_rho_m_c_m(param = param)

  # calculate numerator of fraction
  num <-
    sqrt(param[['kappa_e']] * param[['rho_m_c_m']]) -
    param[['n']] * sqrt(param[["lambda_w"]])

  frac <- num /  (1 - param[['n']])

  param[['lambda_s']] <- frac^2

  return(param)

}

#'
#' Inverse operation: calculate permeability from vector of parameters
#' @param param the vector of parameters
#' @return param with updated permeability field
#' @export
calc_inv_permeability <- function(param){

  # update rho_m_c_m value in case
  param <- calc_rho_m_c_m(param = param)

  # calculate first ratio
  ratio1 <-
    param[['rho_w']] * param[['c_w']] / param[['rho_m_c_m']]

  ratio2 <-
    param[['rho_w']] * param[['g']] / param[['mu_w']]

  param[['permeability']] <- param[['alpha_e']] / ratio1 / ratio2

  return(param)

}

#'
#' Check consistency of physical and reduced parameters in param
#'
#'@param param the vector of parameters
#'@return boolean indicating whether
#'the definition of equivalent parameters is consistent
#'@export
check_consistency <- function(param){

  # these are the fields that we want to check
  str_check <- c('lambda_m','rho_m_c_m','kappa_e','alpha_e')

  # calculate vector with updated parameters
  param_updated <- update_all_eq(param = param)

  # check that equivalent fields are equal for both vectors
  isEqual <- logical(length(str_check)); names(isEqual) <- str_check

  # check all fields
  # allow tolerance in error of 10^-1
  for(i in 1:length(isEqual)){
    rel_error_i <-
      abs((param[[str_check[i]]] - param_updated[[str_check[i]]]) /
            param[[str_check[i]]] )
    isEqual[i] <- (rel_error_i <= 10^-3)

  }

  if(all(isEqual)){
    return(T)
  }else{
    print(paste0('Check the following fields: ',str_check[which(!isEqual)]))
    return(F)
  }

}

#'
#'Inverse operation: Update both lambda_s and permeability from param
#'@param the vector of parameters
#'@return the vector of parameters with updated lambda_s and permeability values
#'@export
calc_inv <- function(param){

  # update value for lambda_s
  param <- calc_inv_lambda_s(param = param)

  # update the value of lambda_m
  # because lambda_s changed
  param <- calc_lambda_m(param = param)

  # update value for permeability
  param <- calc_inv_permeability(param = param)

  # check consistency
  isConsistent <- check_consistency(param)

  # if(!isConsistent){stop("There is an error in the calculations somewhere... oops :'( ")}

  return(param)

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
    param_eq <- update_all_eq(param = param_i)
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

#' Calculate flux from vector of parameters
#'
#' @param param the vector of parameters
#' @return the flow value (in m/s)
#' @export
calc_q <- function(param){

  # term in front of fraction
  const <- param[['permeability']] * param[['rho_w']] * param[['g']] / param[['mu_w']]

  # hydraulic head gradient
  frac_dH_dz <- param[['dH']] / param[['z_bottom']]

  q <- - const * frac_dH_dz

  return( q )
}


#' Calculate Peclet number from vector of parameters
#'
#' @param param the vector of parameters
#' @return the Peclet number
#' @export
calc_Peclet <- function(param){

  param <- calc_eq(param)

  return( calc_Peclet_eq(param = param) )
}


#' Calculate Peclet number from vector of equivalent parameters
#'
#' @param param the vector of parameters
#' @return the Peclet number
#' @export
calc_Peclet_eq <- function(param){
  res <- abs ( param[['alpha_e']] / param[['kappa_e']] * param[['dH']])
  return( as.numeric(res) )
}

#' Add equivalent parameters to dataframe of physical parameters
#'
#' @param param the vector of parameters
#' @param df_phys the dataframe containing physical parameter values
#' @return the dataframe of physical values with fields alpha_e and kappa_e
#' @export
calc_eq_fromPhyDf <- function(param,df_phys){

  # initialize fields
  df_phys$alpha_e <- rep(0,nrow(df_phys))
  df_phys$kappa_e <- rep(0,nrow(df_phys))

  # loop over rows
  # add values of reduced parameters
  for(i in 1:nrow(df_phys)){

    cat(paste0('\n',i,' / ',nrow(df_phys)))

    # create vector of parameters with replaced physical values
    param_i <- replace(x = param,
                       list = names_phy,
                       values = df_phys[i,names_phy])

    # calculate reduced parameters
    param_i <- HZinv::update_all_eq(param = param_i)

    # fill values in physical parameters dataset
    df_phys[i,names_red] <- unlist(param_i[names_red])
  }

  return(df_phys)

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
  res <- data.frame(z = as.numeric(param[get_z_idx]),
                    z_idx = factor_z_idx)

  return(res)

}


