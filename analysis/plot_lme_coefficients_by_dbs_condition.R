dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm", "PC_1")
tests = c("gait", "tug", "fast", "normal", "slow")

dvs = c("gait_speed_meter_per_second")
tests = c("fast", "normal", "slow")

sig_lines <- function(sig, xpos, yul, yll, sigstep = .06, sigoff = .01, xoff = 0, reverse = FALSE
                     ,fromtop = FALSE, cexf = 1){
# add significance indicators to plot
  yrange = yul - yll
  sigrow = nrow(sig)
  if(sigrow > 0){
    for(i in c(1:sigrow)){
      ix = ifelse(reverse, sigrow-i+1, i)
      sx = c(xpos[sig$cone[ix]+xoff], xpos[sig$ctwo[ix]+xoff])
      if(fromtop){
        sy = rep(yul - (sigoff*yrange + yrange*sigstep*(i-1)), 2)
      } else {
        sy = rep(yll + (sigoff*yrange + yrange*sigstep*(i-1)), 2)
      }
      points(sx,sy,type = "l")
      text(mean(sx), mean(sy) - .1*sigstep*yrange, paste(rep("*",sig$p[ix]),collapse = "")
          ,cex = 2.5 * cexf, adj = c(0.5,1))
    }
  }
}

lme_plot <- function(dvname, test, saveplot=TRUE, conds = "all", title = "", skipxlabel=FALSE){

 # load the data
  df = read.csv(file.path(loc$paths$results,paste("lme_",dvname,"_",test,".csv",sep="")))
  if(conds == "amplitude"){
    df  = df[grep("33|66|100", df$names),]
    sig = loc$sigplot$amp[[dvname]]
  }
    
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
#  yll    = .89
#  yul    = 1.31

 # set constants
  xpos = c(1, c(2,3,4)+.5, c(5,6,7)+1, c(8,9)+1.5)
  xlim = c(.5,max(xpos)+.5)
  ylabmgp = c(5,1,0)

  if(conds == "all"){
    cex = 2.5
    xlabel = "condition"
    xlabmgp = c(5,2,0)
    dev.new(height = 7.6, width = 13)
    par(mar = c(6,7,4,2))
    cexsymb = 2
    cexaxis = cex*.8
  } else if(conds == "amplitude"){
    cex = 4
    cexaxis = cex*.7
    xlim = c(xlim[1],xpos[4]+.7)
    xpos = xpos[1:4]
    # par(pin = c(1,5))           # set aspect ratio of panel plots
    par(mar = c(8,9,5,2) + c(.1,.2,.1,0))   # make room for axis labels
    xlabel = "amplitude (%)"
    xlabmgp = c(6.5,2.5,0)
    ylabmgp = c(6.5,2,0)
    cexsymb = 4
  }
    
 # get tick labels   
  xtcklab = c("OFF", gsub(" - OFF","",gsub("configuration","", df$names)))
    
 # do plotting
  plot( xpos, means
      , ylim = c(yll, yul)
      , xlim = xlim
      , pch = 19
      , cex = cexsymb
      , axes = FALSE, yaxt = "n", xaxt = "n", xlab = "", ylab = ""
      , cex.main = cex*2
      , cex.sub  = cex
      , cex.lab  = cex 
      )
  arrows(xpos, ll, xpos, ul, length = 0)
  axis(1, at=xpos, labels=xtcklab, cex.axis = cexaxis, mgp = xlabmgp)
  axis(2, las = 1, cex.axis = cexaxis, mgp = c(4,1,0))
  title(ylab = loc$dvlut[[dvname]], mgp = ylabmgp, cex.lab = cex)
  if(!skipxlabel) title(xlab = xlabel, mgp = xlabmgp, cex.lab = cex)    
  box()

 # create title for amplitude plots
 if(conds == "amplitude") title(title, adj=0, cex.main = cex+1, line = +2) 
    
 # do separator lines
  abline(v=mean(c(xpos[1], xpos[2])), lty = 3)

  if(conds == "all"){
      abline(v=c(mean(c(xpos[4], xpos[5])),mean(c(xpos[7], xpos[8]))), lty = 3)
      text(xpos[3], yul, "amplitude (%)", cex = cex*.7)      
      text(xpos[6], yul, "frequency (Hz)", cex = cex*.7)
      text(mean(c(xpos[8],xpos[9])), yul, expression(paste(" duration (",mu,"s)")), cex = cex*.7)
  }
 
 # do significance indicators
  if(conds == "amplitude"){ 
    sig_lines(sig, xpos, yul, yll, reverse = TRUE, fromtop = TRUE, cexf = 1.5)
  } else {
    sig_lines(loc$sigplot[[test]][[dvname]][["amplitude"]], xpos, yul, yll, xoff = 1)
    sig_lines(loc$sigplot[[test]][[dvname]][["frequency"]], xpos, yul, yll, xoff = 4)
    sig_lines(loc$sigplot[[test]][[dvname]][["duration" ]], xpos, yul, yll, xoff = 7)
  }

  if(saveplot){
    savePlot(file.path(loc$paths$img,paste("lme_",dvname,"_",test,".png",sep="")), type = "png")
    dev.off()
  }    
}

#lme_plot("gait_speed_meter_per_second", "gait", saveplot = TRUE)

# run plots over dvnames and tests 
#grid = expand.grid(dvs, tests, stringsAsFactors = FALSE)
#mapply(lme_plot, grid$Var1, grid$Var2)


# then make the amplitude only plot:
if(TRUE){
dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm")
tests = c("gait")
#svg(file.path(loc$paths$img,paste("lme_amplitude_panels.svg",sep="")), height = 7.8, width = 15)
dev.new(height = 7.8, width = 15)
  op = par(mfrow = c(1,3), bg = NA)
  lme_plot(dvs[1], tests[1], conds = "amplitude", title = "A", skipxlabel = TRUE, saveplot = FALSE)
  lme_plot(dvs[2], tests[1], conds = "amplitude", title = "B", skipxlabel = FALSE, saveplot = FALSE)
  lme_plot(dvs[3], tests[1], conds = "amplitude", title = "C", skipxlabel = TRUE, saveplot = FALSE)
  # make png for testing
  savePlot(file.path(loc$paths$img,paste("lme_amplitude_panels.png",sep="")), type = "png")
  #dev.off()
par(op)
}
