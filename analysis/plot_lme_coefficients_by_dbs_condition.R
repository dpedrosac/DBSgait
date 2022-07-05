dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm", "PC_1")
tests = c("gait", "tug", "fast", "normal", "slow")

lme_plot <- function(dvname, test, saveplot=FALSE, conds = "all", title = ""){

 # load the data
  df = read.csv(file.path(loc$paths$results,paste("lme_",dvname,"_",test,".csv",sep="")))
  if(conds == "amplitude") df = df[grep("33|66|100", df$names),]
    
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


 # set constants
  xpos = c(1, c(2,3,4)+.5, c(5,6,7)+1, c(8,9)+1.5)
  xlim = c(1,9)
  cex = 3
  if(conds == "amplitude"){
    xlim = c(xpos[1]-.5,xpos[4]+.5)
    xpos = xpos[1:4]
    par(pin = c(3,5))
    par(mar = c(5,7,4,2) + 0.1)
  }
    
 # get tick labels   
  xtcklab = c("OFF", gsub(" - OFF","",gsub("configuration","", df$names)))
    
 # do plotting
  plot( xpos, means
      , ylim = c(yll, yul)
      , xlim = xlim
      , pch = 19
      , cex = 2
      , axes = FALSE, yaxt = "n", xaxt = "n", xlab = "", ylab = ""
      , cex.main = cex*2
      , cex.sub  = cex
      , cex.lab  = cex 
      )
  arrows(xpos, ll, xpos, ul, length = 0)
  axis(1, at=xpos, labels=xtcklab, cex.axis = cex*.8, mgp=c(3,1.5,0))
  axis(2, las = 1, cex.axis = cex*.8, mgp = c(4,1,0))
  title(ylab = loc$dvlut[[dvname]], mgp = c(5,1,0), cex.lab = cex)
  title(xlab = "condition", mgp = c(4,2,0), cex.lab = cex)    
  box()

 # create title for amplitude plots
 if(conds == "amplitude") title(title, adj=0, cex.main = cex+1) 
    
 # do separator lines
  abline(v=mean(c(xpos[1], xpos[2])), lty = 3)
  text(xpos[3], yll, "amplitude (%)", cex = cex*.8)
  if(conds == "all"){
      abline(v=c(mean(c(xpos[4], xpos[5])),mean(c(xpos[7], xpos[8]))), lty = 3)
      text(xpos[6], yll, "frequency (Hz)")
      text(mean(c(xpos[8],xpos[9])), yll, "duration (us)")
  }
    
  if(saveplot) savePlot(file.path(loc$paths$img,paste("lme_",dvname,"_",test,".png",sep="")), type = "png")
}

# run plots over dvnames and tests 
#grid = expand.grid(dvs, tests, stringsAsFactors = FALSE)
#mapply(lme_plot, grid$Var1, grid$Var2)
#lme_plot(dvs[1], tests[1], conds = "amplitude", title = "A")

# first make the amplitude only plot:
dev.new(height = 7, width = 15)
  op = par(mfrow = c(1,3))
  lme_plot(dvs[1], tests[1], conds = "amplitude", title = "A")
  lme_plot(dvs[2], tests[1], conds = "amplitude", title = "B")
  lme_plot(dvs[3], tests[1], conds = "amplitude", title = "C")
  savePlot(file.path(loc$paths$img,paste("lme_amplitude_panels.png",sep="")), type = "png")
par(op)

#lme_plot(dvs[1], tests[1], conds = "amplitude")
