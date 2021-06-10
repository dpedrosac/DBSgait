source("local-functions.R")

dvname = "mean_gait_speed_meter_per_second"

df = get_data()
df.n = subset(df, test == "normal")
df.n$dv = df.n[[dvname]]
ag = do.call(data.frame, aggregate( dv~configuration, data = df.n
                       , function(x){c(mu = mean(x), sd = sd(x))}))

#ag$configuration = as.factor(ag$configuration)
# we could sort to be on the safe side...
# (40/90qs, 30/85/130Hz, 33/66/100%, OFF)

ag$order = c(4, 7, 2, 8, 5, 3, 9, 6, 1)
ag = ag[order(ag$order), ]

xtickpos = c(1:9)
xlabels = ag$configuration

# plot ----------------------------------------------------------------------
  # prepare plot panel without plotting (for setting the correct limits)
  plot( rep(xtickpos, 2)
      , c(ag$dv.mu + ag$dv.sd, ag$dv.mu - ag$dv.sd)
      , type = "n", xaxt = "n"
      , xlab = "condition"
      , ylab = dvname
      )

  # plot the averages
  points(dv.mu~order, data = ag, xaxt = "n", pch = 19)

  # plot the errorbars
  arrows( xtickpos, ag$dv.mu - ag$dv.sd, xtickpos, ag$dv.mu + ag$dv.sd
        , length=0.05, angle=90, code=3)

  # plot the connecting lines for configurations
  arrows( c(xtickpos[2], xtickpos[4], xtickpos[5], xtickpos[7], xtickpos[8])
        , c(ag$dv.mu[2], ag$dv.mu[4], ag$dv.mu[5], ag$dv.mu[7], ag$dv.mu[8])    
        , c(xtickpos[3], xtickpos[5], xtickpos[6], xtickpos[8], xtickpos[9])
        , c(ag$dv.mu[3], ag$dv.mu[5], ag$dv.mu[6], ag$dv.mu[8], ag$dv.mu[9])
        , length = 0.0001, angle = 90, code = 3)

 # draw the x-axis
  axis(1, at = xtickpos, labels = xlabels)



