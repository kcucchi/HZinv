
#' Normalizes a pdf object - not exported
#'
#' @param df_pdf a dataframe containing columns x and p_x
#' @return a dataframe correponding to the normalized pdf of the RV
pdf_normalize <- function(df_pdf){


  # vector containing width between each x value
  delta_x <- diff(df_pdf$x)

  # vector containing mean probability for each x interval
  mean_px <- (df_pdf$p_x[1:(nrow(df_pdf)-1)] + df_pdf$p_x[2:nrow(df_pdf)]) / 2

  # calculate value of integral
  norm_constant <- sum(delta_x*mean_px)

  # return normalized pdf
  res <- data.frame(x=df_pdf$x,
                    p_x = df_pdf$p_x / norm_constant)
  return(res)

}


#' Defines uniform distribution between 2 bounds
#'
#' @param x_range a numerical vector of 2 elements
#' containing the minimal and maximal value of RV
#' @param n number of points at which to evaluate the density
#' @return a dataframe correponding to the pdf of the RV
#' @export
pdf_define_unif <- function(x_range,n=50){

  # define constant function
  res <- data.frame(x=seq(from=min(x_range),
                          to=max(x_range),
                          length.out = n),
                    p_x = 1)

  # define zero at boundaries
  res$p_x[1] <- 0; res$p_x[nrow(res)] <- 0

  res <- HZinv:::pdf_normalize(res)

  return(res)

}
