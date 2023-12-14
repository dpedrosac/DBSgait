dvs = c("gait_speed_meter_per_second", "stride_length_cm", "max_sensor_lift_cm")
#dvs = c("gait_speed_meter_per_second")

plot_model <- function(dvname, save = FALSE){

   # define y axis label
    if(dv == "gait_speed_meter_per_second") ylab = "gait speed (m/s)"
    if(dv == "stride_length_cm")            ylab = "stride length (cm)"
    if(dv == "max_sensor_lift_cm")          ylab = "max. sensor lift (cm)"


   # read in data
    csvfile  = file.path(loc$paths$results,paste("lme_lowfreq_",dvname,".csv",sep=""))
    dfp = read.csv(csvfile, header = TRUE, stringsAsFactors = FALSE)

   # save plot if requested
   if(save){
     svg(file.path(loc$paths$img,paste("lme_lowfreq_",dv,".svg",sep=""))
        , height = 8, width = 8)
   }
 
   # create plot
    minval = min(dfp$coef-2*dfp$err)
    maxval = max(dfp$coef+2*dfp$err)
    prange = maxval - minval
    plot(dfp$lvl, dfp$coef, pch = 19, cex = 2, las = 1, axes = FALSE, xlab = ""
        , xlim = c(.5, 2.5), ann = FALSE
        , ylim = c(minval - .2*prange, maxval + .2*prange))

    mtext(side = 2, text = ylab, line = 2, cex = 2)


   # add errorbars (2*SE)
    arrows(dfp$lvl, dfp$coef - 2*dfp$err, dfp$lvl, dfp$coef + 2*dfp$err, length = 0)


   # add x axis labels (as tick marks)
    axis(1, at = c(1,2), labels = dfp$names[dfp$lvl], cex.axis = 1.5)
    par(mgp = c(3,2,0), oma = c(0,3,0,0))

   # format y axis tick marks and add box
    par(mgp = c(3,1,0))
    axis(2, las = 1, cex.axis = 1.5)
    box()
   
   # close device if save == TRUE
    if(save) dev.off()
}

# make plots for all dvs
for( dv in dvs ){ plot_model(dv, save = TRUE) }
