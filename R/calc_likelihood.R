#'
#' This file contains functions to calculate likelihoods.
#'


#'
#' Interpolate the likelihood from the grid
#'
#' @param df_eqGrid_like the dataframe containing
#' the likelihood (column like) evaluated for
#' combinations of alpha_e (column alpha_e)
#' and kappa_e (column kappa_e)
#' @param df_eq_eval the dataframe containing
#' combinations of alpha_e and kappa_e where to evaluate
#' the likelihood
#' @return the df_eq_eval dataframe
#' containing a new column with likelihoods
#' @export
like_interp <- function(df_eqGrid_like,df_eq_eval){

  # prepare for interpolation
  # recast df_eqGrid_like into a list for fields::interp.surface
  obj_grid <-
    reshape2::acast(data = df_eqGrid_like,
                    formula = alpha_e ~ kappa_e,
                    value.var = 'like')

  # make interpolation
  df_eq_eval$like <-
    fields::interp.surface(
      obj = list(x = as.numeric(rownames(obj_grid)),
                 y = as.numeric(colnames(obj_grid)),
                 z = obj_grid),
      loc = df_eq_eval[,names_red])

  # return result
  return(df_eq_eval)

}


#'
#' Calculates the likelihood
#'
#' @param df_eq_comb the dataframe containing combinations of
#' alpha_e, kappa_e and sim_idx
#' @param df_all the dataframe containing measurements (in meas)
#' and simulations (in columns starting with sim).
#' sim_idx is the key linking
#' a set of reduced parameters in df_eq_comb
#' to corresponding columns of df_all
#' check that the names of columns in df_all
#' are the same than contents of the sim_idx field.
#' @param date_min date specifying start of rmse calculation, before this date
#'   values will be discarded (given as POSIXct object)
#'@param center boolean indicating whether to center timeseries before
#'  calculating the rmse
#' @param param the vector parameters
#' @return the dataframe df_eq_comb with corresponding likelihood
#' (in like). Also returns mse and logLike.
#' @export
calc_like <- function(df_eq_comb,df_all,param,date_min=NULL,center=F){

  # check that the names of columns in df_all
  # are the same than contents of the sim_idx field.

  # calculate mse
  df_mse <- HZinv::calc_mse(df_all,date_min = date_min,center=center)

  # join to vector of reduced parameters
  df_eq_comb <-
    dplyr::left_join(x = df_eq_comb,
                     y = df_mse,
                     by = "sim_idx")

  df_eq_comb$logLike <- -1/(2 * (param[['sd_err']]^2)) * df_eq_comb$mse

  df_eq_comb$like <- exp(df_eq_comb$logLike)

  return(df_eq_comb)

}


#'
#'Calculates the rmse between measurements and simulations
#'
#'@param df_all the dataframe containing measurements (in meas) and simulations
#'  (in columns starting with sim)
#'@param date_min date specifying start of rmse calculation, before this date
#'  values will be discarded (given as POSIXct object)
#'@param center boolean indicating whether to center timeseries before
#'  calculating the rmse
#'@return a dataframe containing rmse values for each simulation
#'@export
calc_rmse <- function(df_all,date_min=NULL,center=F){

  if(!is.null(date_min)){

    cat('\ndiscarding ',sum(df_all$dates < date_min),
        '/', nrow(df_all), 'data points')

    df_all <- subset(x = df_all,
                     subset = df_all$dates >= date_min)

  }

  if(center){
    df_all <- HZinv::calc_center(df_all = df_all)
  }

  # calculate rmse from columns
  vect_rmse <-
    unlist(lapply(X = df_all[grep(pattern = '^sim',x = names(df_all))],
                  FUN = function(col) sqrt(mean((col - df_all$meas)^2))  ))

  df_rmse <- data.frame(sim_idx = names(vect_rmse),
                        rmse = as.numeric(vect_rmse),
                        stringsAsFactors = F)

  return(df_rmse)

}


#'
#' Calculates the mse
#' between measurements and simulations
#'
#' @param df_all the dataframe containing measurements (in meas)
#' and simulations (in columns starting with sim)
#' @param date_min date specifying start of rmse calculation, before this date
#'   values will be discarded (given as POSIXct object)
#'@param center boolean indicating whether to center timeseries before
#'  calculating the rmse
#' @return a dataframe containing mse values for each simulation
#' @export
calc_mse <- function(df_all,date_min=NULL,center=F){

  if(!is.null(date_min)){

    cat('\ndiscarding ',sum(df_all$dates < date_min),
        '/', nrow(df_all), 'data points')

    df_all <- subset(x = df_all,
                     subset = df_all$dates >= date_min)

  }

  if(center){
    df_all <- HZinv::calc_center(df_all = df_all)
  }

  # calculate rmse from columns
  vect_mse <-
    unlist(lapply(X = df_all[grep(pattern = '^sim',x = names(df_all))],
                  FUN = function(col) mean((col - df_all$meas)^2)))

  df_mse <- data.frame(sim_idx = names(vect_mse),
                       mse = as.numeric(vect_mse),
                       stringsAsFactors = F)

  return(df_mse)

}


#'
#' Calculates the mse between measurements and simulations
#'
#' @param df_all the dataframe containing measurements (in meas) and simulations
#'   (in columns starting with sim)
#' @return a dataframe where the mean of each timeseries has been extracted
#'   (grouped by depth and simulation/measurement)
#' @export
calc_center <- function(df_all){

  df_all_long <-
    reshape2::melt(data = df_all,id=c("dates","z_idx"))

  df_means <-
    data.frame(df_all_long %>%
                 group_by(z_idx,variable) %>%
                 summarize(mean=mean(value)))

  df_all_long <-
    dplyr::left_join(x = df_all_long,
                     y = df_means,
                     by = c("z_idx","variable"))
  df_all_long$value_centered <-
    df_all_long$value - df_all_long$mean

  df_all_centered <-
    reshape2::dcast(data = df_all_long,
                    formula = dates + z_idx ~ variable,
                    value.var = 'value_centered')

  return(df_all_centered)

}



