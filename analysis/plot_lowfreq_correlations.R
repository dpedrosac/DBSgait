require(lattice)
save = TRUE

if(save){
  fname = file.path(loc$paths$img,"lowfreq_gait_speed_by_stride_length.png")
  png(fname, height=2800, width=2800,res=300)
}

p=xyplot(gait_speed_meter_per_second~stride_length_cm|id,df,cex.axis=2
        ,xlab=list(label = "stride length (cm)", cex = 2)
        ,ylab=list(label = "gait speed (m/s)", cex = 2)
        ,pch=19, cex = .2)

print(p)

if(save) dev.off()

if(save){
  fname = file.path(loc$paths$img,"lowfreq_sensor_lift_by_stride_length.png")
  png(fname, height=2800, width=2800,res=300)
}

p=xyplot(max_sensor_lift_cm~stride_length_cm|id,df,cex.axis=2
        ,xlab=list(label = "stride length (cm)", cex = 2)
        ,ylab=list(label = "max. sensor lift (cm)", cex = 2)
        ,pch=19, cex = .2)

print(p)

if(save) dev.off()
