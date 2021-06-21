# remove "OFF" from dbsconditions
dbsconds =  unique(df$dbscond)[!unique(df$dbscond) %in% "OFF"]
npcs = 10 # number of principal components to use
dopca = TRUE
verb = TRUE

# define svm model
model <- function(cond){
  msg("Prediction for", cond, verbose = verb)
  ddf = df[complete.cases(df) & df$dbscond == cond,]
  n = length(unique(ddf$id))
  out = data.frame(id = rep(NA, n), perccorrect = rep(NA, n))
  cnt = 0
    
  for(id in unique(ddf$id)){
    msg(" ", id, verbose = verb, newline = FALSE)
    cnt = cnt + 1
    ddf.test  = ddf[ddf$id == id, c("configuration", loc$dvnames)]
    ddf.train = ddf[ddf$id != id, c("configuration", loc$dvnames)]

    # if dopca
    if(dopca){
      msg(" doing pca", verbose = verb, newline = FALSE)
      conf.train = subset(ddf.train, select = configuration)
      conf.test  = subset(ddf.test,  select = configuration)

      pcafit = prcomp(~., ddf.train[,loc$dvnames], scale = TRUE, na.action = "na.omit")
      ddf.train = predict(pcafit, newdata = ddf.train[,loc$dvnames])[,c(1:npcs)]
      ddf.test  = predict(pcafit, newdata = ddf.test[ ,loc$dvnames])[,c(1:npcs)]    

      ddf.train = cbind(conf.train, ddf.train)
      ddf.test  = cbind(conf.test,  ddf.test)
    }
    
    if(nrow(ddf.test) == 0) browser()
    msg(" fitting svm", verbose = verb, newline = FALSE)
    m = svm( configuration ~ .
           , data = ddf.train
           , kernel = "polynomial", gamma = 1, cost = 100
           , type = "C")

    pred = predict(m, newdata = ddf.test)
    tab = table(pred, ddf.test$configuration)
    pcorr = sum(diag(tab)) / sum(tab)
    out$id[cnt] = id
    out$perccorrect[cnt] = pcorr
    msg(" DONE", verbose = verb)
  }

  msg( "Mean correct for", cond, ":"
     , formatC(mean(out$perccorrect), digits = 3, format = "f")
     , verbose = verb)

return(mean(out$perccorrect))
}

# apply svm model to all conditions
lapply(dbsconds, model)

