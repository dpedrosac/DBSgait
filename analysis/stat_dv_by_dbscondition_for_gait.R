# remove "OFF" from dbsconditions
dbsconds =  unique(df$dbscond)[!unique(df$dbscond) %in% "OFF"]

# define mixed effects model
model <- function(dv, cond){

  df$dv = df[[dv]]  # inefficient but when using formula(paste(...
                    # there are problems with consecutive anova
  m0 = lme( dv ~ 1
          , random = ~1|id
          , data = subset(df, dbscond == cond & !grepl("tug", test))
          , method = "ML"
          , na.action = na.omit
          )

  m1 = lme( dv ~ test
          , random = ~1|id
          , data = subset(df, dbscond == cond & !grepl("tug", test))
          , method = "ML"
          , na.action = na.omit
          )

  m2 = lme( dv ~ test + configuration
          , random = ~1|id
          , data = subset(df, dbscond == cond & !grepl("tug", test))
          , method = "ML"
          , na.action = na.omit
          )

  m3 = lme( dv ~ test * configuration
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

return(out)
}

# run model over dvnames and dbs conditions (pulse width, frequency, strength)
grid = expand.grid(loc$dvnames, dbsconds, stringsAsFactors = FALSE)
results = do.call(rbind, mapply(model, grid$Var1, grid$Var2, SIMPLIFY = FALSE))


