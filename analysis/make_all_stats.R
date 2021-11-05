# start all statistical analyses from here in order to avoid loading the data
# several times over and also in order to keep an overview of the current pipeline
# of statistical analysis. All packages needed should also be loaded here


# --- Preparation -----------------------------------------------------------
# get local functions and constants
source("local-functions.R")
source("local-constants.R")

# set correct orthogonal contrast settings
options(contrasts=c("contr.sum","contr.poly"))       

# load packages
ipak(c("nlme","e1071"))

# get data
df = get_data(type = "all", off_normalized = "FALSE", remove_outliers = TRUE, do_pca = 4)

# define other variables
loc = local_constants()
loc$dvnames = get_dvnames(df)

# --- Stats -----------------------------------------------------------------


#source("stat_dv_by_dbscondition_for_gait.R")
#source("stat_dbs_setting_prediction.R")
