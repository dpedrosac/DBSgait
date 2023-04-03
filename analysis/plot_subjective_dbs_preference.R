df = read.csv("../data/subjective_dbs_ranking.csv", header = TRUE)

df$best = as.character(df$best)
df$best[df$best == "100"] = "100%"
df$best[df$best ==  "33"] =  "33%"
df$best[df$best ==  "66"] =  "66%"
df$best[df$best == "130"] = "130Hz"
df$best[df$best ==  "85"] =  "85Hz"
df$best[df$best ==  "30"] =  "30Hz"
df$best[df$best ==  "90"] =  "90µs"
df$best[df$best ==  "40"] =  "40µs"

df$worst = as.character(df$worst)
df$worst[df$worst == "100"] = "100%"
df$worst[df$worst ==  "33"] =  "33%"
df$worst[df$worst ==  "66"] =  "66%"
df$worst[df$worst == "130"] = "130Hz"
df$worst[df$worst ==  "85"] =  "85Hz"
df$worst[df$worst ==  "30"] =  "30Hz"
df$worst[df$worst ==  "90"] =  "90µs"
df$worst[df$worst ==  "40"] =  "40µs"



x11(width=19, height=8)
layout(matrix(c(1,2), 1, 2))

par(cex.main = 3, cex.axis = 2, cex.lab = 3) # set font sizes
par(mar=par("mar")+c(4,0,6,0)) # give space for panel labels A/B
par(mar = par("mar") * c(1,1,1,0))

tit1="most preferred"
tab.best = table(df$best)
barplot( tab.best[order(tab.best, decreasing = TRUE)]
       , axes  = FALSE
       #, xlab = "condition"
       , cex.names = 2
       , asp=.3, ylim=c(0,12)
       )
axis(2, las=2)
title(tit1)
title(xlab="condition", line=6)
text(-1.2, 16, "A", cex=3.5, xpd = NA, font = 2)

par(mar = par("mar") * c(1,0,1,1))
tab.worst = table(df$worst)
barplot(tab.worst[order(tab.worst, decreasing = TRUE)]
       , axes = FALSE, width = 1, ylim=c(0,12)
       , cex.names = 2)

axis(2, las=2)
title("least preferred")
text(-.5, 16, "B", cex = 3.5, xpd = NA, font = 2)
title(xlab="condition", line=6)

savePlot(filename = "../img/subjective_dbs_preferrences.png"
        , type = c("png"))
