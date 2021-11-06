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
ipak(c("nlme","lsmeans","multcomp"))

# get data
df = get_data(type = "all", off_normalized = "FALSE", remove_outliers = TRUE, do_pca = 4)

# define other variables
loc = local_constants()
loc$dvnames = get_dvnames(df)

# --- Stats -----------------------------------------------------------------


ddf = subset(df, test != "tug_one" & test != "tug_two")
ddf$test = factor(ddf$test)
ddf$test = relevel(ddf$test, "normal")
contrasts(ddf$test) <- contr.treatment(4)
m = lme( gait_speed_meter_per_second ~ -1 + configuration + test
        , weights = varIdent(form = ~1|configuration*test)
        , control = lmeControl(opt="optim")         
        , random = ~1|id, method = "ML", data = ddf, na.action = na.omit)

summary(glht(m, linfct = c("configuration33  - configurationOFF = 0"
                          ,"configuration66  - configurationOFF = 0"
                          ,"configuration100 - configurationOFF = 0"
                          ,"configuration40  - configurationOFF = 0"
                          ,"configuration85  - configurationOFF = 0"
                          ,"configuration130 - configurationOFF = 0"
                          ,"configuration30  - configurationOFF = 0"
                          ,"configuration90  - configurationOFF = 0"
                          ,"configuration40  - configuration85  = 0"
                          ,"configuration40  - configuration130 = 0"
                          ,"configuration85  - configuration130 = 0"
                          ,"configuration30  - configuration90  = 0"
                          ))
        ,test=adjusted("bonferroni"))

source("stat_lme_dbsconditions_gait_and_tug.R")
#source("stat_dv_by_dbscondition_for_gait.R")`
#source("stat_dbs_setting_prediction.R")
