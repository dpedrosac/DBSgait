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

outlier_to_na <- function(x, nsigma = 3){
   mu    = mean(x)
   sigma = sd(x)
   x[x < mu - nsigma * sigma | x > mu + nsigma * sigma] = NA
return(x)   
}

get_data <- function( off_normalized = FALSE
                    , type = "aggregated"
                    , remove_outliers = FALSE){
# get local constants and paths
source("local-constants.R")
lc = local_constants()

if(type == "aggregated") datafile = "gait_params.csv"
if(type == "individual") datafile = "stride_params.csv"    
if(type == "clinical"){
  df = read.csv(file.path(lc$paths$data,"clinical_data.csv")
               , stringsAsFactors = FALSE, sep = ";")
  # make coding of pat-ID the same as in the other data:
  names(df)[names(df) == 'Patient'] = 'id'
  ix = grep("P[1-9]$", df$id)
  df$id[ ix] = gsub("P","Pat_0",df$id[ ix])
  df$id[-ix] = gsub("P","Pat_" ,df$id[-ix])
  return(df)
}
    
# read in data
df = read.csv(file.path(lc$paths$data, datafile)
             , header = TRUE
             , stringsAsFactors = FALSE
             , sep = ";"
             , na.strings = "NaN")
df[is.na(df)] = NA # hack for different types of NA in df

# do some renaming for convenient filtering of dependent variables by undescores
names(df)[names(df) == 'patient_id'] = 'id'    
if(type == "individual"){
  names(df)[names(df) == 'stride_id'] = 'stride'
  names(df)[names(df) == 'time_stamp_s'] = 'timestamp'    
}
    
# remove outliers if requested
if(remove_outliers){
  # finding cols by using _ includes also the percent values. it might not
  # be so sensible to cut at x sigmas here...
  numeric_cols =  names(df)[grep("_", names(df))] 
  df[numeric_cols] = lapply(df[numeric_cols], outlier_to_na) 
}
    
# normalize data by subtracting off condition mean
if(off_normalized){
  numeric_cols =  names(df)[grep("_", names(df))] 
#  varnames = setdiff(names(df), c("patient_id", "test", "configuration"))
  df = group_reference(numeric_cols, df)
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
    
return(df)
} # end of get_data function                

group_reference <- function(varnames, df){
# subtract OFF condition mean from data
for(testname in unique(df$test)){
  for(pat in unique(df$id)){
    for(var in varnames){
      ix = df$test == testname & df$id == pat
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
