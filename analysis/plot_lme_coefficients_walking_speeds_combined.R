#dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm", "PC_1")
dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm")

lme_plot <- function(dvname, saveplot=TRUE, conds = "all", title = "", skipxlabel=FALSE){

 # load the data
  df.fast   = read.csv(file.path(loc$paths$results,paste("lme_",dvname,"_fast.csv"  ,sep="")))
  df.normal = read.csv(file.path(loc$paths$results,paste("lme_",dvname,"_normal.csv",sep="")))
  df.slow   = read.csv(file.path(loc$paths$results,paste("lme_",dvname,"_slow.csv"  ,sep="")))
  df.free   = read.csv(file.path(loc$paths$results,paste("lme_",dvname,"_free.csv"  ,sep="")))
  if(conds == "amplitude") df = df[grep("33|66|100", df$names),]
    
 # extract model coefficients and standard errors of comparisons
  coff.fast  = df.fast$coff[1]                    # off coefficient is constant 
  sall.fast  = c(mean(df.fast$err), df.fast$err)  # off is the reference level 
  means.fast = coff.fast+c(0, df.fast$call)       # first position is OFF (+0)
  ll.fast     = means.fast - 2*sall.fast
  ul.fast     = means.fast + 2*sall.fast

  coff.normal  = df.normal$coff[1]                     
  sall.normal  = c(mean(df.normal$err), df.normal$err) 
  means.normal = coff.normal+c(0, df.normal$call)      
  ll.normal     = means.normal - 2*sall.normal
  ul.normal     = means.normal + 2*sall.normal

  coff.slow  = df.slow$coff[1]                    
  sall.slow  = c(mean(df.slow$err), df.slow$err)   
  means.slow = coff.slow+c(0, df.slow$call)       
  ll.slow     = means.slow - 2*sall.slow
  ul.slow     = means.slow + 2*sall.slow

  coff.free  = df.free$coff[1]                    
  sall.free  = c(mean(df.free$err), df.free$err)   
  means.free = coff.free+c(0, df.free$call)       
  ll.free     = means.free - 2*sall.free
  ul.free     = means.free + 2*sall.free

 # calc y axis limits
  miny   = min(c(ll.fast, ll.normal, ll.slow, ll.free))
  maxy   = max(c(ul.fast, ul.normal, ul.slow, ul.free))
  rangey = maxy - miny
  yll    = miny - .1 * rangey
  yul    = maxy + .1 * rangey

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
  xtcklab = c("OFF", gsub(" - OFF","",gsub("configuration","", df.fast$names)))

 # do plotting

  # means
  plot( xpos, means.fast
      , ylim = c(yll, yul)
      , xlim = xlim
      , pch = 20
      , type = "n"
      , col = rgb(.6,.6,.6)
      , cex = cexsymb
      , axes = FALSE, yaxt = "n", xaxt = "n", xlab = "", ylab = ""
      , cex.main = cex*2
      , cex.sub  = cex
      , cex.lab  = cex 
      )

  # errors
  arrows(xpos, ll.fast,   xpos, ul.fast,   length = 0)
  arrows(xpos, ll.normal, xpos, ul.normal, length = 0)
  arrows(xpos, ll.slow,   xpos, ul.slow,   length = 0)
  arrows(xpos, ll.free,   xpos, ul.free,   length = 0)

  # coefficients
  points( xpos, means.fast,   pch = 20, cex = cexsymb , col = rgb(.9,.9,.9))
  points( xpos, means.normal, pch = 20, cex = cexsymb , col = rgb(.7,.7,.7))
  points( xpos, means.slow,   pch = 20, cex = cexsymb , col = rgb(.5,.5,.5))
  points( xpos, means.free,   pch = 18, cex = cexsymb )

  axis(1, at=xpos, labels=xtcklab, cex.axis = cexaxis, mgp = xlabmgp)
  axis(2, las = 1, cex.axis = cexaxis, mgp = c(4,1,0))
  title(ylab = loc$dvlut[[dvname]], mgp = ylabmgp, cex.lab = cex)
  if(!skipxlabel) title(xlab = xlabel, mgp = xlabmgp, cex.lab = cex)    
  box()

 # add legend
  legend("topleft", c("fast","normal","slow","free")
                  , pch = c(20,20,20,18)
                  , cex = cex*.7
                  , bty = "n"
                  , col =  c(rgb(.9,.9,.9)
                            ,rgb(.7,.7,.7)
                            ,rgb(.5,.5,.5)
                            ,rgb( 0, 0, 0)))

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
    
  if(saveplot){
    savePlot(file.path(loc$paths$img,paste("lme_",dvname,"_allspeeds.png",sep="")), type = "png")
    dev.off()
  }    
}

lme_plot(dvs[1],saveplot = TRUE)

# run plots over dvnames and tests 
#lapply(dvs,lme_plot)
#lme_plot(dvs[1], tests[1], saveplot = FALSE)


# then make the amplitude only plot:
## dev.new(height = 7.8, width = 15)
##   op = par(mfrow = c(1,3), bg = NA)
##   lme_plot(dvs[1], tests[1], conds = "amplitude", title = "A", skipxlabel = TRUE, saveplot = FALSE)
##   lme_plot(dvs[2], tests[1], conds = "amplitude", title = "B", skipxlabel = FALSE, saveplot = FALSE)
##   lme_plot(dvs[3], tests[1], conds = "amplitude", title = "C", skipxlabel = TRUE, saveplot = FALSE)
##   savePlot(file.path(loc$paths$img,paste("lme_amplitude_panels.png",sep="")), type = "png")
##   dev.off()
## par(op)


