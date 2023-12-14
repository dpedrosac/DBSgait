# statistical analysies for low freq DBS project with Alex


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
  loc = local_constants()
  df = get_data( type = "individual", off_normalized = "FALSE"
               , filter_id = loc$lowfreq$id
               , remove_outliers = TRUE, do_pca = 4)

 # define other variables
  loc$dvnames = get_dvnames(df)

 # filter out all conditions other than "free"
  df = df[df$test == "free",]

# --- Stats & Plots ---------------------------------------------------------

#  source("stat_lme_lowfreq_gait.R")
source("stat_vat_lowfreq.R")
#  source("plot_lme_lowfreq_gait.R")


# --- Log -------------------------------------------------------------------

  sink(file = "analysis_lowfreq.log")
    msg(paste("Analysis run on", date()))
    msg("\n\nMachine and R version information:")
    version
    msg("\n\nPackage version information:")
    sapply(pkgs, getNamespaceVersion)
  sink(file = NULL)
