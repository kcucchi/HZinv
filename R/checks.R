

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
