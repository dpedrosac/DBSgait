local_constants <- function(){
out = list()

# set paths dependent on user
out$paths =list()
if(Sys.info()["user"] == "urs"){
  out$paths$root    = "/home/urs/sync/projects/DBSgait"
  out$paths$data    = file.path(out$paths$root, "data"   )
  out$paths$results = file.path(out$paths$root, "results")
  out$paths$img     = file.path(out$paths$root, "img"    )    
}

out$dvlut = list(gait_speed_meter_per_second = "gait speed (m/s)"
                ,stride_length_cm            = "stride length (cm)"
                ,max_sensor_lift_cm          = "max sensor lift (cm)"
                ,PC_1                        = "PC 1")

# the following lists indicate significant results in plots due to the
# requirements of the final revision. these are static since there
# was no intention to change the analysis anymore. cone marks the
# first element of the comparison, ctwo the second and p the
# level of significance (1 = p<.05, 2 = p<.01, 3 = p<.001)
out$sigplot = list()

# plots for all DBS settings
out$sigplot$gait = list()
out$sigplot$tug  = list()

out$sigplot$gait$gait_speed_meter_per_second = list(
   amplitude = data.frame(cone = c(2)
                         ,ctwo = c(3)
                         ,p    = c(3))
  ,frequency = data.frame(cone = c(1,2) 
                         ,ctwo = c(2,3) 
                         ,p    = c(3,3))
  ,duration  = data.frame(cone = c() 
                         ,ctwo = c() 
                         ,p    = c())
  )

out$sigplot$tug$gait_speed_meter_per_second = list(
   amplitude = data.frame(cone = c(2)
                         ,ctwo = c(3)
                         ,p    = c(3))
  ,frequency = data.frame(cone = c(1,2) 
                         ,ctwo = c(2,3) 
                         ,p    = c(3,3))
  ,duration  = data.frame(cone = c(1) 
                         ,ctwo = c(2) 
                         ,p    = c(3))
  )

out$sigplot$gait$stride_length_cm = list(
   amplitude = data.frame(cone = c(1,2)
                         ,ctwo = c(2,3)
                         ,p    = c(3,3))
  ,frequency = data.frame(cone = c(1,1,2) 
                         ,ctwo = c(3,2,3) 
                         ,p    = c(1,3,3))
  ,duration  = data.frame(cone = c(1) 
                         ,ctwo = c(2) 
                         ,p    = c(3))
  )

out$sigplot$tug$stride_length_cm = list(
   amplitude = data.frame(cone = c(1,2)
                         ,ctwo = c(2,3)
                         ,p    = c(2,2))
  ,frequency = data.frame(cone = c(1,2) 
                         ,ctwo = c(2,3) 
                         ,p    = c(3,3))
  ,duration  = data.frame(cone = c(1) 
                         ,ctwo = c(2) 
                         ,p    = c(1))
  )
 
out$sigplot$gait$max_sensor_lift_cm = list(
   amplitude = data.frame(cone = c(1,2)
                         ,ctwo = c(2,3)
                         ,p    = c(3,3))
  ,frequency = data.frame(cone = c(1,1,2) 
                         ,ctwo = c(3,2,3) 
                         ,p    = c(3,3,1))
  ,duration  = data.frame(cone = c(1) 
                         ,ctwo = c(2) 
                         ,p    = c(3))
  )

out$sigplot$tug$max_sensor_lift_cm = list(
   amplitude = data.frame(cone = c(1)
                         ,ctwo = c(2)
                         ,p    = c(3))
  ,frequency = data.frame(cone = c(1,1) 
                         ,ctwo = c(3,2) 
                         ,p    = c(3,3))
  ,duration  = data.frame(cone = c() 
                         ,ctwo = c() 
                         ,p    = c())
  )

# for amplitude only
out$sigplot$amp = list(
   gait_speed_meter_per_second = data.frame(cone = c(1,1,1)
                                           ,ctwo = c(2,3,4)
                                           ,p    = c(2,3,3))
  ,stride_length_cm            = data.frame(cone = c(1,1)
                                           ,ctwo = c(3,4)
                                           ,p    = c(3,3))
  ,max_sensor_lift_cm          = data.frame(cone = c(1,1)
                                           ,ctwo = c(3,4)
                                           ,p    = c(3,3))
  )


return(out)
} # end of function local_constants
