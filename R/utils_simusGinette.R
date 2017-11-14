
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


#' Reads measurements given to Ginette (both boundary conditions and
#' intermediary temperatures)
#'
#' @param pathCsv path to csv containing files (typically in
#'   field_database/1_data-hz_to_ginette/data_hz/data/03_manual_corrections/)
#' @return a dataframe in long format containing cleaned values of measurements
#'   on the field
#' @export
read_measGinette <- function(pathCsv){

  # read information in file
  data_csv <-
    read.csv(file = pathCsv,
             header = T,sep = ';',stringsAsFactors = F)

  # transform dates into date format
  data_csv$t_time <-
    as.POSIXct(x = data_csv$dates,
               tz = "GMT",
               format="%d/%m/%Y %H:%M:%S")

  data_csv <- data_csv[,setdiff(names(data_csv),'dates')]

  # reshape to long format
  data_csv_long <-
    reshape2::melt(data = data_csv,id.vars = "t_time")

  # include field 'type' for whether it is temperature or head differential
  data_csv_long$type <- "head_differential_m"
  data_csv_long$type[grep(pattern = '^T_',
                          x = data_csv_long$variable)] <- "temperature_C"

  # separate variables (z_top,z_bottom,z_1,head_differential)
  # and depth (corresponding depths in m)
  # names(data_csv)
  # now temperature depths (indicated in field T_depth_<n>_cm)
  Tdepth_names <-
    names(data_csv)[grep(pattern = 'depth',
                         names(data_csv))]
  Tdepth_vect <- gsub("T_depth_", "",Tdepth_names)
  Tdepth_vect <- gsub("cm_C", "",Tdepth_vect)
  Tdepth_vect <- as.numeric(Tdepth_vect)
  # print(Tdepth_vect)
  max_depth <- max(Tdepth_vect)

  # first define vector of depths
  # horribolos but works, so good enough
  data_csv_long$depth_m <- NA
  data_csv_long$depth_m[which(data_csv_long$variable == 'T_stream_C')] <- 0
  data_csv_long$depth_m[grep(pattern = 'T_depth_',x = data_csv_long$variable)] <-
    as.numeric(gsub("T_depth_", "",
                    gsub("cm_C","",
                         data_csv_long$variable[grep(pattern = 'T_depth_',
                                                     x = data_csv_long$variable)])))/100
  # now define vector of variables
  # unique(data_csv_long$variable)
  # replace T_stream_C by z_top
  data_csv_long$variable <-
    gsub(pattern = 'T_stream_C',
         replacement = 'z_top',
         x = data_csv_long$variable)
  # replace deepest variable by z_bottom
  data_csv_long$variable[grep(pattern = paste0('T_depth_',max_depth),
                              x = data_csv_long$variable)] <- 'z_bottom'
  for(i_out in 1:(length(Tdepth_vect)-1)){
    data_csv_long$variable[grep(pattern = paste0('T_depth_',Tdepth_vect[i_out]),
                                x = data_csv_long$variable)] <-
      paste0('z_',i_out)
  }

  return(data_csv_long)

}
