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

return(out)
} # end of function local_constants
