slf <- function(){source("local-functions.R")}

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
