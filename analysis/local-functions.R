slf <- function(){source("local-functions.R")}

msg <- function(..., verbose = TRUE, newline = TRUE, sep = " "){
  if(verbose) cat(paste(..., sep = sep))
  if(newline) cat("\n")
}

ipak <- function(pkg){
# taken from https://gist.github.com/stevenworthington/3178163
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

get_dvnames <- function(df){
# return dependent variable names
#
# would better be placed in local-constants but get_data
# depends on local-constants being complete
return(names(df)[grep("_", names(df))])
}

get_data <- function(off.normalized = FALSE){
# get local constants and paths
source("local-constants.R")
lc = local_constants()

# read in data
df = read.csv(file.path(lc$paths$data, "gait_params.csv")
             , header = TRUE, stringsAsFactors = FALSE
             , na.strings = "NaN")
df[is.na(df)] = NA # hack for different types of NA in df

# normalize data by subtracting off condition mean
if(off.normalized){
  varnames = setdiff(names(df), c("patient_id", "test", "configuration"))
  df = group_reference(varnames, df)
}

# define the dbs conditions for convenient filtering
df$dbscond = NA
df$dbscond[df$configuration == "040" | df$configuration == "090"] = "pulse"
df$dbscond[  df$configuration == "030"
           | df$configuration == "085"
           | df$configuration == "130"] = "frequency"
df$dbscond[  df$configuration == "033"
           | df$configuration == "066"
           | df$configuration == "100"] = "strength"
df$dbscond[df$configuration == "OFF"] = "OFF"

# rename patient_id for convenient filtering of dependent variables by undescores
names(df)[names(df) == 'patient_id'] = 'id'    
    
return(df)
} # end of get_data function                

group_reference <- function(varnames, df){
# subtract OFF condition mean from data
for(testname in unique(df$test)){
  for(pat in unique(df$patient_id)){
    for(var in varnames){
      ix = df$test == testname & df$patient_id == pat
      mu = mean(df[[var]][ix & df$configuration == "OFF"], na.rm = TRUE)
      df[[var]][ix] = df[[var]][ix] - mu
    } # end of var loop
  } # end of patient_id loop
} # end of testname loop
    
return(df)
}

group_center <- function(var, groups, fun = "diff"){
# subtract group means from data
# 2021-06-07 Urs Kleinholdermann with help from here:
# https://stackoverflow.com/questions/40267772/mean-centering-based-on-conditions
  df = data.frame(var, groups)
  ag = aggregate(var~groups, df, mean, na.rm = TRUE)
  merge.df = merge(df, ag, by = "groups", suffixes = c(".data", ".mean"))
  merge.df$diff = merge.df$var.data - merge.df$var.mean

return(merge.df$diff)    
}
