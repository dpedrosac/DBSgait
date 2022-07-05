df = read.csv2("../data/clinical_data.csv")

ddf = data.frame(condition = rep(c("ON","OFF"), each = nrow(df))
                ,updrs     = c(df$UPDRS_III_Score_ON, df$UPDRS_III_Score_OFF))


boxplot(updrs~condition, ddf, yaxt = "n", xaxt = "n", ylab = "", xlab = "", cex.axis = 2
       , cex.lab = 2.5
       , mar = par(mar = par("mar") + c(0,2,0,0))
       , ylim = c(0,65)
       )

axis(2,cex.axis=2, las = 2)
axis(1,at = c(1,2), label = c("OFF","ON"), mgp = c(2,2,0), cex.axis=2)
title(ylab = "UPDRS part III score", cex.lab = 2.5, mgp = c(4,2,0))
points(c(1,2), c(60,60), type = "l")
points(c(1,1), c(60,60-2.5), type = "l")
points(c(2,2), c(60,60-2.5), type = "l")
text(1.5, 63, "***", cex = 3)
savePlot(file.path("../img/UPDRS_OFF_ON.png",sep=""), type = "png")

