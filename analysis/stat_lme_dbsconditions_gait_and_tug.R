dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm", "PC_1")
tests = c("fast", "normal", "slow", "tug", "gait")

lme_model <- function(dvname, testname, save = TRUE, padj = "fdr"){

 # select data and model depending on test (gait vs tug)
  if(testname == "tug" ){
    ddf = subset(df, test == "tug_one" | test == "tug_two")
    modelformula = as.formula(paste(dvname, "-1 + configuration", sep = "~"))    
  }

  if(testname == "fast"){
    ddf = subset(df, test == "fast")
    modelformula = as.formula(paste(dvname, "-1 + configuration", sep = "~"))
  }

  if(testname == "normal"){
    ddf = subset(df, test == "normal")
    modelformula = as.formula(paste(dvname, "-1 + configuration", sep = "~"))
  }

  if(testname == "slow"){
    ddf = subset(df, test == "slow")
    modelformula = as.formula(paste(dvname, "-1 + configuration", sep = "~"))
  }

  if(testname == "gait"){
    ddf = subset(df, test != "tug_one" & test != "tug_two")      
    modelformula = as.formula(paste(dvname, "-1 + configuration + test", sep = "~"))

   # make normal gait the standard comparison level 
    ddf$test = factor(ddf$test)
    ddf$test = relevel(ddf$test, "normal")
    contrasts(ddf$test) <- contr.treatment(4) # test has 4 levels: normal, slow, fast, free
  }

 # make model
  m = lme(  modelformula
          , weights = varIdent(form = ~1|configuration*test)
          , control = lmeControl(opt="optim")         
          , random = ~1|id
          , method = "ML"
          , data = ddf
          , na.action = na.omit)

 # conduct planned comparisons and print them to file
  mt = glht(m, linfct = c("configuration33  - configurationOFF = 0"
                         ,"configuration66  - configurationOFF = 0"
                         ,"configuration100 - configurationOFF = 0"
                         ,"configuration30  - configurationOFF = 0"
                         ,"configuration85  - configurationOFF = 0"
                         ,"configuration130 - configurationOFF = 0"
                         ,"configuration40  - configurationOFF = 0"
                         ,"configuration90  - configurationOFF = 0"
                         ,"configuration33  - configuration66  = 0"
                         ,"configuration66  - configuration100 = 0"
                         ,"configuration30  - configuration100 = 0"
                         ,"configuration85  - configuration100 = 0"
                         ,"configuration130 - configuration100 = 0"
                         ,"configuration40  - configuration100 = 0"
                         ,"configuration90  - configuration100 = 0"
                         ,"configuration30  - configuration85  = 0"
                         ,"configuration30  - configuration130 = 0"
                         ,"configuration85  - configuration130 = 0"
                         ,"configuration40  - configuration90  = 0"
                         ))

 # save results if requested
  if(save){
   # save results for plotting
    csvfile  = file.path(loc$paths$results,paste("lme_",dvname,"_",testname,".csv",sep=""))
    write.csv(data.frame( coff  = summary(m)$coefficients$fixed["configurationOFF"]
                        , call  = summary(mt)$test$coefficients[1:8]
                        , err   = summary(mt)$test$sigma[1:8]
                        , names = names(summary(mt)$test$coefficients[1:8]))
             ,file = csvfile)
     
   # save stats
    txtfile = file.path(loc$paths$results,paste("lme_",dvname,"_",testname,".txt"  ,sep=""))
    sink(file = txtfile)
      print(summary(mt, test = adjusted(padj)))
    sink(file = NULL)
  } else {
   # print results if saving is not requested
    print(summary(mt, test=adjusted(padj)))
  }
}

# run model over dvnames and tests 
grid = expand.grid(dvs, tests, stringsAsFactors = FALSE)
mapply(lme_model, grid$Var1, grid$Var2)
