# remove "OFF" from dbsconditions
dbsconds =  unique(df$dbscond)[!unique(df$dbscond) %in% "OFF"]

# define mixed effects model
model <- function(dv, cond){
  #cat(paste("\n\n Mixed effects model for", dv,"and", cond, "\n\n"))

  df$dv = df[[dv]]  # inefficient but when using formula(paste(...
                    # there are problems with consecutive anova
  m0 = lme( dv~1
          , random = ~1|id
          , data = subset(df, dbscond == cond & !grepl("tug", test))
          , method = "ML"
          , na.action = na.omit
          )

  m1 = lme( dv~as.factor(test)
          , random = ~1|id
          , data = subset(df, dbscond == cond & !grepl("tug", test))
          , method = "ML"
          , na.action = na.omit
          )

  m2 = lme( dv~test+configuration
          , random = ~1|id
          , data = subset(df, dbscond == cond & !grepl("tug", test))
          , method = "ML"
          , na.action = na.omit
          )

  m3 = lme( dv~test*configuration
          , random = ~1|id
          , data = subset(df, dbscond == cond & !grepl("tug", test))
          , method = "ML"
          , na.action = na.omit
          )
 
 out = data.frame( dv            = dv
           , condition     = cond
           , p.test        = anova(m0, m1)[["p-value"]][2]
           , p.condition   = anova(m1, m2)[["p-value"]][2]
           , p.interaction = anova(m2, m3)[["p-value"]][2]
           , stringsAsFactors = FALSE)

 #browser()
 #print(summary(m))

return(out)
}

# run model over dvnames and dbs conditions (pulse width, frequency, strength)
grid = expand.grid(loc$dvnames, dbsconds, stringsAsFactors = FALSE)
results = mapply(model, grid$Var1, grid$Var2, SIMPLIFY = FALSE)
results = do.call(rbind, results)


# ANOVA is not working due to empty cells:
## df.pw = df.pw[complete.cases(df.pw),]
## ezANOVA( data   =  df.pw
##        , dv     =  dv
##        , wid    = .(id), 
##        , within = .(test, config)
##        , type   = 3
##        )
