#'
#' # Define names of parameters #
#'

# names of physical parameters
names_phy <- c('lambda_s','rho_s','c_s','n','permeability')
devtools::use_data(names_phy,overwrite = T)


# names of reduced parameters
names_red <- c('alpha_e','kappa_e')
devtools::use_data(names_red,overwrite = T)

# names of parameters for boundary conditions
names_BC <- c("period_T","dH","A","T_mu","sampling_period")
devtools::use_data(names_BC,overwrite = T)
