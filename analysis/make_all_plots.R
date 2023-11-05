# start all statistical analyses from here in order to avoid loading the data
# several times over and also in order to keep an overview of the current pipeline
# of statistical analysis. All packages needed should also be loaded here


# --- Preparation -----------------------------------------------------------
# get local functions and constants
source("local-functions.R")
source("local-constants.R")

loc = local_constants()
loc$dvnames = get_dvnames(df)

# --- plots -----------------------------------------------------------------

source("plot_lme_coefficients_by_dbs_condition.R")
#source("plot_updrs_on_off.R")




#  --- dump -----------------------------------------------------------------
# plot functions not used for the publication figures
#source("plot_dv_cross_correlation.R")
