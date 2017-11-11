
#' Reads combinations of physical parameters from list of files of parameters
#' given to Ginette
#'
#' @param pathFold path to folder where
#' @return a dataframe containing values of physical parameters (named as in
#'   names_phy) for each combination of iFile and iLine
#' @export
read_parameterSets <- function(pathFold){

  # initialize dataframe
  df_param_phy <-
    data.frame(matrix(ncol = 2+length(names_phy), nrow = 0))
  colnames(df_param_phy) <- c('iFile','iLine',names_phy)

  # iFile=1
  for(iFile in 1:length(list.files(pathFold))){

    # path to parameter files
    path_iFile <-
      paste0(pathFold,'parameterSets_',iFile,'.txt')

    df_param_iFile <- read.table(file = path_iFile)

    # write names of parameters
    # corresponding to physical parameters
    # notations correspond to ones in package HZinv
    names(df_param_iFile) <- c("permeability","lambda_s","n","c_s","rho_s")

    # add fields corresponding to idx of file and idx of line
    df_param_iFile$iFile <- iFile
    df_param_iFile$iLine <- 1:nrow(df_param_iFile)

    # append to main dataframe
    df_param_phy <-
      rbind(df_param_phy,df_param_iFile[,names(df_param_phy)])

  }

  return(df_param_phy)

}
