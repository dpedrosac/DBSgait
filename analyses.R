# This is code to run analyses for the DBS gait project;
# Code developed by Felicitas Muegge, Amelie Heun, Urs Kleinholdermann and David Pedrosa

ipak <- function(pkg){ # taken from https://gist.github.com/stevenworthington/3178163
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

## First specify the packages of interest and install them if necessary
packages = c("readxl", "tidyr", "dplyr", "ggplot2")
ipak(packages)

# Load data in a wide format and convert to something that may be used for estimations of ANOVA and for plotting purposes
data_dir 	<-	"/media/storage/projekte/DBS_gait/data/"
df 			<- 	read_excel(paste(data_dir, "gait_parameters_mean.xlsx", sep=""), sheet = "gait_parameters_mean") # load data as dataframe
df 			<- 	df %>% drop_na("Mean value") # remove NA 
df 			<- 	as.data.frame(df)
all_params<- unique(as.factor(df$gait.parameter))

# rename values witin dataframe and convert to "wide format"
names(df)[names(df) == "Mean value"] 		<- "average.value"
names(df)[names(df) == "Patient ID"] 			<- "subj"
names(df)[names(df) == "Test"] 					<- "metric"
names(df)[names(df) == "Configuration"] 		<- "condition"
names(df)[names(df) == "Gait Parameter"] 	<- "gait.parameter"

df_eyeball 						<- df[which(df$gait.parameter=="Mean Gait Speed [m/s]" & df$metric=="free"),] 
df_eyeball 						<- na.omit(df_eyeball)
df_eyeball$subj 					<- as.factor(df_eyeball$subj)
df_eyeball$condition 			<- as.factor(df_eyeball$condition)
df_eyeball$average.value	<- as.numeric(df_eyeball$average.value)

# Normalisation
idx_subj 				<- df_eyeball$subj[df_eyeball$condition == "OFF"] # data normalised to "OFF" condition so analyses restricted too those who were recorded in the "OFF"
df_eyeball 			<- df_eyeball[is.element(df_eyeball$subj, idx_subj),]
df_eyeball 			<- df_eyeball %>%
    group_by(subj) %>%
    mutate(normalised.value = average.value / average.value[condition == "OFF"]) # Normalisation

p1 <- ggplot(df_eyeball, aes(x=condition, y=normalised.value, group=subj, color=subj)) +
geom_point(stat='summary', fun=sum) +
stat_summary(fun=sum, geom="line")

# Problems right now: Dimensionaly reduction seem reasonable (PCA, k-nearest neighbours, etc.), a different approach might be more 
# intuitive to understand, since 030 and 033 are not similat but different settings (amplitude vs. different frequency at high amplitude respectively). 
# For the latter separate plots for frequency (30, 90) vs. OFF, amplitude (033, 066, 100) vs. OFF and pulse width (040, 085) vs. OFF. 
# Small question: what is the difference between 130Hz and 100?! pulse width?
