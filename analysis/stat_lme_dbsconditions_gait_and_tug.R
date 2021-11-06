dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm", "PC_1")
tests = c("tug")

lme_model <- function(dvname, testname){

 # select data and model depending on test (gait vs tug)
  if(testname == "tug" ){
    ddf = subset(df, test == "tug_one" | test == "tug_two")
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
  ms = summary(glht(m, linfct = c("configuration33  - configurationOFF = 0"
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

 # save results to rdata and plain text files
  rdfile  = file.path(loc$paths$results,paste("lme_",dvname,"_",testname,".Rdata",sep=""))
  txtfile = file.path(loc$paths$results,paste("lme_",dvname,"_",testname,".txt"  ,sep=""))
  save("m","ms",file = rdfile)
  sink(file = txtfile)
    print(ms)
  sink(file = NULL)
}

# run model over dvnames and tests 
grid = expand.grid(dvs, tests, stringsAsFactors = FALSE)
mapply(lme_model, grid$Var1, grid$Var2)
