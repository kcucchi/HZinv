

#' Check the consistency in boundary conditions
#' between 2 vector of parameters
#'
#' @param param_1 the first vector of parameters
#' @param param_2 the second vector of parameters
#' @return boolean for whether boundary conditions are the same
#' @export
check_BC <- function(param_1,param_2){
  
  
  # BC for first vector of parameters
  BC_1 <- param_1[names_BC]
  
  # BC for second vector of parameters
  BC_2 <- param_2[names_BC]
  
  # check consistency
  res <- all(BC_1 == BC_2)
  
  return (res)
}


#' Check that physical parameter values are within bounds
#'
#' @param param the first vector of parameters
#' @return boolean for whether boundary conditions are the respected
#' @export
check_isInBounds <- function(param){
  
  # this matric will contain all booleans
  mat_isInBounds <-
    matrix(data = F,
           nrow = 2,ncol = length(names_phy))
  rownames(mat_isInBounds) <- c('min','max')
  colnames(mat_isInBounds) <- c(names_phy)
  
  # check permeability
  mat_isInBounds['min','permeability'] <-
    log10(param[['permeability']]) >= param[['permeability_log10_min']]
  mat_isInBounds['max','permeability'] <-
    log10(param[['permeability']]) <= param[['permeability_log10_max']]
  
  # check other parameters
  names_phy_noPerm <- setdiff(names_phy,'permeability')
  for(i in 1:length(names_phy_noPerm)){
    
    # check min bound
    mat_isInBounds['min',names_phy_noPerm[i]] <-
      (param[names_phy_noPerm[i]] >= param[[paste0(names_phy_noPerm[i],'_min')]])
    
    # check max bound
    mat_isInBounds['max',names_phy_noPerm[i]] <-
      (param[names_phy_noPerm[i]] <= param[[paste0(names_phy_noPerm[i],'_max')]])
  }
  
  if(all(mat_isInBounds)){return(T)}else{
    
    # these contain the row & col idx 
    # corresponding to non respected bounds
    idx_F <- which(!mat_isInBounds,arr.ind = T)
    
    # pastes min/max and parameter name
    str_nonRespected <-
      paste(rownames(mat_isInBounds)[idx_F[,1]],
            colnames(mat_isInBounds)[idx_F[,2]],
            sep = ' of ')
    
    # issues a warning with the names of violated bounds
    warning(paste0('The following bounds are violated: ',
                   paste(str_nonRespected,collapse = ', ')))
    
    # return a false for not respected bounds
    return(F)
    
  }
  
}
