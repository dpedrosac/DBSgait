# TODO
# swing time variation
# gait length variation
# see https://stats.stackexchange.com/questions/292533/is-a-mixed-model-appropriate-to-compare-continous-outcomes-between-categorica

source("local-functions.R")
require(lattice)
options(contrasts=c('contr.sum','contr.poly'))

df  = get_data(type = "individual", off_normalized = TRUE, remove_outliers = TRUE)
dfc = get_data(type = "clinical")
dvnames = names(df)[grep("_", names(df))]
df = merge(df, dfc, by = "id", all = TRUE, sort = FALSE)
df$configuration = factor(df$configuration
                ,levels = c("OFF","33","66","100","40","85","130","30","60","90"))
#df$configuration = relevel(df$configuration, ref = "OFF")


# do pca
ix = complete.cases(df[,dvnames])
pca = prcomp(df[ix,dvnames], scale = TRUE, center = TRUE) # do pca
df = cbind(df, predict(pca, newdata = df)[,c("PC1","PC2")])
dvnames = c(dvnames, "PC1","PC2")
ddf = subset(df, configuration%in%c("OFF","40","85","130"))
ddf$configuration = factor(ddf$configuration, levels = c("OFF","40","85","130"))

#df = subset(df, UPDRS_III_Score_OFF < 29)

panel.grpeb<-function(x,y,...){
  efun = function(x,...){sd(x)/sqrt(length(x))}
  m=aggregate(y,list(x),mean,na.rm=TRUE)
  s=aggregate(y,list(x),efun,na.rm=TRUE)
  panel.arrows(m$Group.1,m$x,m$Group.1,m$x - 1.96*s$x,code=3,length=0,...)
  panel.arrows(m$Group.1,m$x,m$Group.1,m$x + 1.96*s$x,code=3,length=0,...)
  panel.xyplot(m$Group.1,m$x,...)
  panel.abline(h=0)
}


for(dvname in dvnames){
   print(dvname)
   df$dv = df[[dvname]]
   ddf = aggregate(dv~id*configuration, mean, data = df, na.rm = TRUE)
   p <- xyplot(dv~as.factor(configuration)
        , ddf, panel = panel.grpeb, ylab = dvname)
   print(p)
   Sys.sleep(2)
   savePlot(paste("../img/all_config_updrs_",dvname,".png",sep = ""), type = "png")
#   dev.off()
}    


if(FALSE){
  for(dvname in dvnames){
     print(dvname)
     df$dv = df[[dvname]]
     ddf = aggregate(cbind(dv, UPDRS_III_Score_OFF)~id
                    , mean, data = df, na.rm = TRUE)
     p <- xyplot(dv~UPDRS_III_Score_OFF
          , panel = function(x,y,...){
               panel.xyplot(x,y,...)
               panel.lmline(x,y,...)
            }
          , ddf, ylab = dvname)
     print(p)
     Sys.sleep(4)
     savePlot(paste("../img/correl_updrs_",dvname,".png",sep = ""), type = "png")
  #   dev.off()
  }    
}

#dddf = aggregate(dv~test*configuration, nafun, data = ddf, na.action = na.pass)
#acast(dddf,test~configuration, value.var = "dv")
