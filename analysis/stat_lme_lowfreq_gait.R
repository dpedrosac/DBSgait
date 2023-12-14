dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm")
#tests = c("fast", "normal", "slow", "tug", "gait")

# Use these for the additional analysis of the "free" condition
#dvs = c("max_sensor_lift_cm")
#tests = c("free")

lme_model <- function(dvname, save = FALSE, padj = "fdr"){

  modelformula = as.formula(paste(dvname, "-1 + configuration", sep = "~"))    

  mt = glht(m, linfct = c("configuration85 - configuration130 = 0"))

  summary(mt)


 # make model
  m = lme(  modelformula
          , weights = varIdent(form = ~1|configuration)
          , control = lmeControl(opt="optim")         
          , random = ~1|id
          , method = "ML"
          , data = df
          , na.action = na.omit)

 # conduct planned comparisons and print them to file
  mt = glht(m, linfct = c("configuration130 - configuration85 = 0"))

 # save results if requested
  if(save){
   # save results for plotting
    csvfile  = file.path(loc$paths$results,paste("lme_lowfreq_",dvname,".csv",sep=""))
    cf = coef(summary(m))
    wdf = data.frame( coef  = c(cf[row.names(cf) == "configuration130",colnames(cf)=="Value"]
                             ,  cf[row.names(cf) == "configuration85" ,colnames(cf)=="Value"])
                    , err   = summary(mt)$test$sigma
                    , names = c("130Hz","85Hz")
                    , lvl   = c(2,1)) #lvl = convenience level for plotting
    write.csv(wdf, file = csvfile, row.names = FALSE)
     
   # save stats
    txtfile = file.path(loc$paths$results,paste("lme_lowfreq_",dvname,".txt"  ,sep=""))
    sink(file = txtfile)
      print(summary(m))
      cat("\n\n")
      print(summary(mt))
    sink(file = NULL)
  } else {
   # print results if saving is not requested
    print(summary(mt))
  }
}

# run model over dvnames 
sapply(dvs, lme_model, save = TRUE)
