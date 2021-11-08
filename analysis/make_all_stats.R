# start all statistical analyses from here in order to avoid loading the data
# several times over and also in order to keep an overview of the current pipeline
# of statistical analysis. All packages needed should also be loaded here


# --- Preparation -----------------------------------------------------------

 # get local functions and constants
  source("local-functions.R")
  source("local-constants.R")

 # set orthogonal contrast settings
  options(contrasts=c("contr.sum","contr.poly"))       

 # load packages
  pkgs = c("nlme", "multcomp")
  ipak(pkgs) # load packages

 # get data
  df = get_data(type = "all", off_normalized = "FALSE", remove_outliers = TRUE, do_pca = 4)

 # define other variables
  loc = local_constants()
  loc$dvnames = get_dvnames(df)

# --- Stats -----------------------------------------------------------------

  source("stat_lme_dbsconditions_gait_and_tug.R")

# --- Log -------------------------------------------------------------------

  sink(file = "analysis.log")
    msg(paste("Analysis run on", date()))
    msg("\n\nMachine and R version information:")
    version
    msg("\n\nPackage version information:")
    sapply(pkgs, getNamespaceVersion)
  sink(file = NULL)

# --- Dump -------------------------------------------------------------------

#source("stat_dv_by_dbscondition_for_gait.R")`
#source("stat_dbs_setting_prediction.R")
