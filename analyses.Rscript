# This is code to run analyses for the DBS gait project;
# Code developed by Felicitas Muegge, Amelie Heun, Urs Kleinholdermann and David Pedrosa

## First specify the packages of interest
packages = c("readxl", "tidyr", "dplyr", "ggplot2")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

library(readxl)
library("tidyr")
library("dplyr")
library("ggplot2")

# Load data in a wide format and convert to something that may be used for estimations of ANOVA and for plotting purposes
data_dir <-	"/media/storage/projekte/DBS_gait/data/"
df 		<- 	read_excel(paste(data_dir, "gait_parameters_mean.xlsx", sep=""), sheet = "gait_parameters_mean") # load data as dataframe
df 		<- 	df %>% drop_na("Mean value") # remove NA 
df 		<- 	as.data.frame(df)

# rename values witin dataframe and convert to "wide format"
names(df)[names(df) == "Mean value"] <- "average.value"
names(df)[names(df) == "Patient ID"] <- "subj"
names(df)[names(df) == "Test"] <- "metric"
names(df)[names(df) == "Configuration"] <- "condition"
names(df)[names(df) == "Gait Parameter"] <- "gait.parameter"
df_wide	<- 	spread(df, condition, average.value) # %>% gather(condition, metric, average.value)

# Eyeballing data
test <- df_wide[which(df_wide$gait.parameter=="Mean Gait Speed [m/s]" & df_wide$metric=="fast"),]
# df_long <- select(test, "030", "033", "040", "066", "085", "090", "100", "130", "OFF", subj) %>% gather(condition, average.value, -subj)
df_long <- select(test,  "OFF", "033", "066", "100", subj) %>% gather(condition, average.value, -subj)

p1 <- ggplot(data = df_long, aes(x = condition, y = average.value, group = subj, colour = subj)) +
    #mytheme +
    # coord_trans(y="log10", limy=c(10000,60000)) +
    # labs(list(y = paste("NET","n","pg/mL"))) + 
    geom_line(size=1) + 
    #geom_text(data=subset(ocdrug, ID == 9), aes(label=ID), show_guide = F) +
    #scale_colour_discrete(name="Sequence: ", labels=c("OCD then OC", "OC then OCD")) + 
    theme_minimal() #(legend.position="bottom")
