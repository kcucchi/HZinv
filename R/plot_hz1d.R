

#' Plots temperature measurement timeseries
#'
#' @param param the vector of parameters
#' @param df_hz1d_t the dataframe of measurements
#' @param z (in m) a vector of depths at which to calculate the temperature
#' Depths need to be between 0 and maximum depth.
#' @return temperature at depth z and time t for parameters param
#' @export
plot_hz1d_t <- function(param,df_hz1d_t){

  df_z_idx <- HZinv::get_level_z_fromParam(param = param)

  # create named vector for legend labels
  vect_labels <- as.character(df_z_idx$z)
  names(vect_labels) <- as.character.factor(df_z_idx$z_idx)

  ggplot(data = df_hz1d_t,
         mapping = aes(x=t_time,y=temperature,color=z_idx)) +
    geom_line() +
    scale_color_manual(
      values=col_z, # internal value for col_z defined in data-raw/internal_colors.R
      labels = vect_labels) +
    labs(x="",y="T (in C)", color="depth\n(in m)") +
    theme_bw()

}
