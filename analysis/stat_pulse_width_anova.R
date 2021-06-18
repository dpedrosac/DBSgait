source("local-functions.R")
ipak(c("rstatix", "car","ez","nlme"))

df = get_data()
df.pw = subset(df, configuration == "040" | configuration == "090")
df.pw = data.frame( id     = df.pw$patient_id
                  , dv     = df.pw$mean_stride_length_cm
                  , test   = df.pw$test
                  , config = df.pw$configuration)

# not working due to empty cells:
## df.pw = df.pw[complete.cases(df.pw),]
## ezANOVA( data   =  df.pw
##        , dv     =  dv
##        , wid    = .(id), 
##        , within = .(test, config)
##        , type   = 3
##        )
options(contrasts=c("contr.sum","contr.poly"))       
m = lme(dv~test*config, random = ~1|id, data = df.pw, method = "ML", na.action = na.omit)
