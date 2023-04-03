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


x11(width=15, height=8)
layout(matrix(c(1,2), 1, 2))

par(cex.main = 1.8, cex.axis = 1.8, cex.lab = 2.2)


tit1="A)         most preferred DBS parameter setting        "
tab.best = table(df$best)
barplot( tab.best[order(tab.best, decreasing = TRUE)]
       , axes  = FALSE
       , xlab = "condition"
       , cex.names = 1.5
       , asp=.6, ylim=c(0,12)
       )
axis(2, las=2)


title(tit1)


tab.worst = table(df$worst)
barplot(tab.worst[order(tab.worst, decreasing = TRUE)]
       , axes = FALSE, width = 1, ylim=c(0,12)
       , xlab = "condition", cex.names = 1.5)

axis(2, las=2)
title("B)          least preferred DBS parameter setting      ")

savePlot(filename = "../img/subjective_dbs_preferrences.png"
        , type = c("png"))
