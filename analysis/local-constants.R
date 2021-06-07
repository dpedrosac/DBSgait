local_constants <- function(){
out = list()
out$paths =list()

if(Sys.info()["user"] == "urs"){
  out$paths$root    = "/home/urs/sync/projects/DBSgait"
  out$paths$data    = file.path(out$paths$root, "data"   )
  out$paths$results = file.path(out$paths$root, "results")
  out$paths$img     = file.path(out$paths$root, "img"    )    
}

return(out)
} # end of function local_constants
