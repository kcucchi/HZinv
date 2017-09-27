# creates internal variables for plotting

#'
#' # Generate vector for depth colors #
#'

# choose palette from RColorBrewer

colsSet <- RColorBrewer::brewer.pal(8, "Set2")
col_z <- c(z_0 = colsSet[1],
           z_1 = colsSet[2],
           z_2 = colsSet[3],
           z_3 = colsSet[4],
           z_bottom = colsSet[5])

# save into internal datafile
devtools::use_data(col_z, internal = TRUE,overwrite = T)

