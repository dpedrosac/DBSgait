# This is code to run analyses for the DBS gait project;
# Code developed by Felicitas Muegge, Amelie Heun, Urs Kleinholdermann and David Pedrosa

# switches / flags / constants
normfun = "difference" # "proportion"

# subfunctions 
ipak <- function(pkg){ # taken from https://gist.github.com/stevenworthington/3178163
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

## First specify the packages of interest and install them if necessary
packages = c("readxl", "tidyr", "dplyr", "ggplot2", "reshape2")
ipak(packages)

# Load data in a wide format and convert to something that may be used for estimations of ANOVA and for plotting purposes
data_dir 	<-	"/media/storage/projekte/DBS_gait/data/"
if(Sys.info()["user"] == "urs") data_dir = "/home/urs/sync/projects/DBSgait/"
df 			<- 	read_excel(paste(data_dir, "gait_parameters_mean.xlsx", sep=""), sheet = "gait_parameters_mean") # load data as dataframe
df 			<- 	df %>% drop_na("Mean value") # remove NA 
df 			<- 	as.data.frame(df)

# rename values witin dataframe and convert to "wide format"
names(df)[names(df) == "Mean value"] 		<- "average.value"
names(df)[names(df) == "Patient ID"] 			<- "subj"
names(df)[names(df) == "Test"] 					<- "metric"
names(df)[names(df) == "Configuration"] 		<- "condition"
names(df)[names(df) == "Gait Parameter"] 	<- "gait.parameter"
all_params<- unique(as.factor(df$gait.parameter))

df_eyeball 						<- df[which(df$gait.parameter=="Cv Stride Length [cm]" & df$metric=="fast"),] 
df_eyeball 						<- na.omit(df_eyeball)
df_eyeball$subj 					<- as.factor(df_eyeball$subj)
df_eyeball$condition 			<- as.factor(df_eyeball$condition)
df_eyeball$average.value	<- as.numeric(df_eyeball$average.value)

# Separate data and normalise to "OFF" condition
idx_subj 				<- df_eyeball$subj[df_eyeball$condition == "OFF"] # data normalised to "OFF" condition so analyses restricted to those who were recorded in the "OFF"
groups = list(as.factor(c("OFF", "033", "066", "100")), as.factor(c("OFF", "040", "090")), as.factor(c("OFF", "030", "085", "130")))
names  = c("amplitude", "pulse_width", "frequency")
for(i in 1:length(groups)) {
  df_temp <- df_eyeball[is.element(df_eyeball$subj, idx_subj),]
  df_temp <- df_temp[is.element(df_temp$condition, groups[[i]]),]

  # Normalisation
  df_temp 			<- df_temp[is.element(df_temp$subj, idx_subj),]
  if(normfun == "proportion"){
    df_temp 			<- df_temp %>%
        group_by(subj) %>%
        mutate(normalised.value = average.value / average.value[condition == "OFF"]) # Normalisation
     assign(names[i], df_temp) 
  } else if (normfun == "difference"){
    df_temp 			<- df_temp %>%
        group_by(subj) %>%
        mutate(normalised.value = average.value - average.value[condition == "OFF"]) # Normalisation
     assign(names[i], df_temp) 
  }
}
 
p1 <- ggplot(amplitude, aes(x=condition, y=normalised.value, group=subj, color=subj)) +
geom_point(stat='summary', fun=sum) +
stat_summary(fun=sum, geom="line") + 
theme_minimal()

p2 <- ggplot(pulse_width, aes(x=condition, y=normalised.value, group=subj, color=subj)) +
geom_point(stat='summary', fun=sum) +
stat_summary(fun=sum, geom="line")

p3 <- ggplot(frequency, aes(x=condition, y=normalised.value, group=subj, color=subj)) +
geom_point(stat='summary', fun=sum) +
stat_summary(fun=sum, geom="line")

# Problems right now: Dimensionaly reduction seem reasonable (PCA, k-nearest neighbours, etc.); Why aren't we looking at the two sensors separately?
# with so many data, differentiating the two sides would be a small effort and could give more insight.

# Create correlation matrices for all subjects
# cpde adapted from: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

## Helper functions
# Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  } 

reorder_cormat <- function(cormat){
# Use correlation between variables as distance
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]
}

data_wide 				<- spread(df, gait.parameter, average.value)
data_wide[, c(4:20)] 	<- sapply(data_wide[, c(4:20)], as.numeric) # convert to numeric to estimate the correlations
cormat 						<- round(cor(data_wide[,4:20]),2)
cormat 						<- reorder_cormat(cormat) # Reorder the correlation matrix
upper_tri 					<- get_upper_tri(cormat)
melted_cormat 			<- melt(upper_tri, na.rm = TRUE) # Melt correlation matrix

p4 <- ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed()

p4 +
geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  legend.justification = c(1, 0),
  legend.position = c(0.6, 0.7),
  legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))
