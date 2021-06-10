source("local-functions.R")
ipak(c("rstatix", "car"))

df = get_data()
df.pw = subset(df, configuration == "040" | configuration == "090")
df.pw$test = as.factor(df.pw$test)
df.pw$configuration = as.factor(df.pw$configuration)

summary(aov(DV~F1*F2+Error(RF/(F1*F2))))

