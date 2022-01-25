dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm", "PC_1")
tests = c("gait", "tug", "fast", "normal", "slow")

lme_plot <- function(dvname, test){

 # load the data
  df = read.csv(file.path(loc$paths$results,paste("lme_",dvname,"_",test,".csv",sep="")))

 # extract model coefficients and standard errors of comparisons
  coff = df$coff[1]                # off coefficient is constant 
  sall = c(mean(df$err), df$err)   # off is the reference level and has no error of its own
  means = coff+c(0, df$call)       # first position is OFF (+0)
  ll = means - 2*sall
  ul = means + 2*sall

 # calc y axis limits
  miny   = min(ll)
  maxy   = max(ul)
  rangey = maxy - miny
  yll    = miny - .1 * rangey
  yul    = maxy + .1 * rangey

 # get tick labels   
  xtcklab = c("OFF", gsub(" - OFF","",gsub("configuration","", df$names)))

 # do plotting
  plot( c(1:9), means
      , ylim = c(yll, yul)
      , pch = 19
      , axes = FALSE
      , xlab = "condition"
      , ylab = dvname)
  arrows(c(1:9), ll, c(1:9), ul, length = 0)
  axis(1, at=c(1:9), labels=xtcklab)
  axis(2, las = 1)
  savePlot(file.path(loc$paths$img,paste("lme_",dvname,"_",test,".png",sep="")), type = "png")
}

# run plots over dvnames and tests 
grid = expand.grid(dvs, tests, stringsAsFactors = FALSE)
mapply(lme_plot, grid$Var1, grid$Var2)
