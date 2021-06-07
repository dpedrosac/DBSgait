require(lattice)
source("local-functions.R")

plotfun <- function(dvname, df){
  df$dv = df[[dvname]]

  mypanel <- function(x,y,...){
    panel.xyplot(x,y,...)
    panel.xyplot(x,y, type = "l",...)
  }

  xyplot(dv ~ as.factor(configuration)|test
        , groups = patient_id
        , data = df
        , panel = mypanel
        )
}

df = get_data(off.normalize = TRUE)

plotfun("mean_stride_length_cm", df)

