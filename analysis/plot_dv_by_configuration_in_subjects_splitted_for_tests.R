require(lattice)
source("local-functions.R")

plotfun <- function(dvname, df){
  df$dv = df[[dvname]]

  mypanel <- function(x,y,...){
    panel.xyplot(x,y,...)
    panel.xyplot(x,y, type = "l",...)
  }

  xyplot(dv ~ as.factor(configuration)|test
        , groups = id
        , data = df
        , panel = mypanel
        )
}

df = get_data(off.normalize = TRUE)

plotfun("mean_gait_speed_meter_per_second", df)

