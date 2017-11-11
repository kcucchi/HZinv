
#' Imports vector of parameter from csv file
#'
#' @param csv_file path to the csv file
#' @param check boolean indicating whether to check consistency of parameters
#' @return the vector of parameters
#' @export
csv2param <- function(csv_file,check=T){

  #
  #  read parameters from file
  #

  df_param <- read.csv(csv_file,
                       stringsAsFactors = F)
  # str(df_param)

  #
  # create named vector from dataframe
  #

  # this is for an easier way to access variable values
  param <- df_param$value
  names(param)<- df_param$notation
  rm(df_param)

  #
  # Calculate equivalent parameters
  #

  param <- HZinv::update_all_eq(param = param)

  # check consistency
  if(check) {HZinv::check_consistency(param)}

  return(param)

}
