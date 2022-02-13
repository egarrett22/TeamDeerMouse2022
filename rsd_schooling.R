library(tidyverse)
library(cowplot)
library(car)
library(multcomp)
library(lme4)
library(nlme)
library(lsmeans)
library(multcompView)
library(DataExplorer)


#to install any of these packages, simply run the command install.packages("PACKAGE_NAME")
#you only need to do this once

setwd("C:/Users/ajtur/Documents/Ranalyses/RSD_schooling")
theme_set(theme_cowplot())

##customize figures the way I like
theme_update(axis.title = element_text(size = 8), 
             axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
             plot.margin = unit(c(0.3, 0.3, 0.1, 0.3), "cm"), #TOP, RIGHT, BOTTOM, LEFT
             strip.placement = "outside", strip.background = NULL, 
             strip.text = element_text(size = 8, face = "plain", margin = margin(b=3,t=3)),
             legend.title = element_blank(), legend.text = element_text(size = 9), #legend.position = c(0.01,0.95))
             legend.position = "none")

## Set up function that summarizes data.

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
    # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  datac <- rename(datac, c("mean" = measurevar))  # Rename the "mean" column  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


###Import schooling data

data <- read.csv("RSD_schooling_master_data_sheet.csv") #reads in data from directory

create_report(data)


#this is a note
#alignment
angle_data <- summarySE(data, measurevar="angle_stdev_3d", groupvars=c("condition", "flow"))

angle_plot <- ggplot(data = angle_data, aes(x = flow, y = angle_stdev_3d, group = condition, colour = condition, shape = condition)) +
  geom_point(position=position_dodge(2)) + #adds points to graph
  geom_smooth(method = lm, se = FALSE) +
  #geom_line(position=position_dodge(2)) + #connects points with line
  geom_errorbar(aes(ymin = angle_stdev_3d-se, ymax = angle_stdev_3d+se), width = 1, position=position_dodge(2)) +
  scale_color_manual(values = c("blue", "brown")) +
  theme(legend.position = c(0.7, 0.8), legend.direction = "vertical", legend.title = element_blank(), legend.text = element_text(size = 7)) +
  #ylim(0, 20) +
  xlab("Flow speed (cm/s)") +
  ylab("School alignment (SD)") +
  annotate("text", x = c(19.5,29.5,39.5,49.5,59.5), y = c(11, 8, 7, 9, 9), label = "A", size = 4) + #no sediment
  annotate("text", x = c(20.5,30.5,40.5,50.5,60.5), y = c(15, 11, 10, 5, 6), label = c("a", "b", "bc", "c", "bc"), size = 4) #sediment
  
angle_plot

ggsave("angle_plot.png", angle_plot, width = 6.5, height = 5)


#2 ways to analyze, depends on whether we want to treat flow as a continuous or categorical variable
#categorical
angle_model_cat <- lme(angle_stdev_3d ~ condition * as.factor(flow) + average_length + turbidity_reading_FNU, data = data, random = ~ 1 | as.factor(trial), na.action=na.exclude)
anova(angle_model_cat)
lsmeans(angle_model_cat, pairwise ~ condition|flow)
lsmeans(angle_model_cat, pairwise ~ flow|condition)

angle_model_cat_marginal <- lsmeans(angle_model_cat, pairwise ~ condition:flow)

cld(angle_model_cat_marginal$lsmeans, Letters = letters)

#continuous
angle_model_con <- lme(angle_stdev_3d ~ condition * flow + average_length + turbidity_reading_FNU, data = data, random = ~ 1 | as.factor(trial), na.action=na.exclude)
anova(angle_model_con)
emtrends(angle_model_con, pairwise ~ condition, var = "flow")


#nearest neighbour distance
nn_data <- summarySE(data, measurevar="nn_mean_3d", groupvars=c("condition", "flow"))

nn_plot <- ggplot(data = nn_data, aes(x = flow, y = nn_mean_3d, group = condition, colour = condition, shape = condition)) +
  geom_point(position=position_dodge(2)) +
  geom_line(position=position_dodge(2)) +
  geom_errorbar(aes(ymin = nn_mean_3d-se, ymax = nn_mean_3d+se), width = 1, position=position_dodge(2)) +
  scale_color_manual(values = c("blue", "brown")) +
  theme(legend.position = c(0.1, 0.9), legend.direction = "vertical", legend.title = element_blank(), legend.text = element_text(size = 7)) +
  #ylim(0, 20) +
  xlab("Flow speed (cm/s)") +
  ylab("Nearest neighbour distance (cm)") +
  annotate("text", x = c(20,30,40,50,60), y = c(11, 11, 13, 15, 13), label = c("a", "a", "ab", "b", "ab"), size = 4) #flow

nn_plot

Sggsave("nn_plot.png", nn_plot, width = 6.5, height = 5)

#2 ways to analyze, depends on whether we want to treat flow as a continuous or categorical variable
#categorical
nn_model_cat <- lme(nn_mean_3d ~ condition * as.factor(flow) + average_length , data = data, random = ~ 1 | as.factor(trial), na.action=na.exclude)
anova(nn_model_cat)
lsmeans(nn_model_cat, pairwise ~ flow)
lsmeans(nn_model_cat, pairwise ~ flow|condition)

#continuous
nn_model_con <- lme(nn_mean_3d ~ condition * flow + average_length , data = data, random = ~ 1 | as.factor(trial), na.action=na.exclude)
anova(nn_model_con)
emtrends(nn_model_con, pairwise ~ condition, var = "flow")


#nearest neighbour variation
nn_stdev_data <- summarySE(data, measurevar="nn_stdev_3d", groupvars=c("condition", "flow"))

nn_stdev_plot <- ggplot(data = nn_stdev_data, aes(x = flow, y = nn_stdev_3d, group = condition, colour = condition, shape = condition)) +
  geom_point(position=position_dodge(2)) +
  geom_line(position=position_dodge(2)) +
  geom_errorbar(aes(ymin = nn_stdev_3d-se, ymax = nn_stdev_3d+se), width = 1, position=position_dodge(2)) +
  scale_color_manual(values = c("blue", "brown")) +
  theme(legend.position = c(0.1, 0.9), legend.direction = "vertical", legend.title = element_blank(), legend.text = element_text(size = 7)) +
  #ylim(0, 20) +
  xlab("Flow speed (cm/s)") +
  ylab("Variation in nearest neighbour distance (SD)") 

nn_stdev_plot

ggsave("nn_stdev_plot.png", nn_stdev_plot, width = 6.5, height = 5)

#2 ways to analyze, depends on whether we want to treat flow as a continuous or categorical variable
#categorical
nn_stdev_model_cat <- lme(nn_stdev_3d ~ condition * as.factor(flow) + average_length , data = data, random = ~ 1 | as.factor(trial), na.action=na.exclude)
anova(nn_stdev_model_cat)
lsmeans(nn_stdev_model_cat, pairwise ~ condition|flow)
lsmeans(nn_stdev_model_cat, pairwise ~ flow|condition)

#continuous
nn_stdev_model_con <- lme(nn_stdev_3d ~ condition * flow + average_length , data = data, random = ~ 1 | as.factor(trial), na.action=na.exclude)
anova(nn_stdev_model_con)
emtrends(nn_stdev_model_con, pairwise ~ condition, var = "flow")



#school volume
volume_data <- summarySE(data, measurevar="school_volume_3d_cm", groupvars=c("condition", "flow"))

volume_plot <- ggplot(data = volume_data, aes(x = flow, y = school_volume_3d_cm, group = condition, colour = condition, shape = condition)) +
  geom_point(position=position_dodge(2)) +
  geom_line(position=position_dodge(2)) +
  geom_errorbar(aes(ymin = school_volume_3d_cm-se, ymax = school_volume_3d_cm+se), width = 1, position=position_dodge(2)) +
  scale_color_manual(values = c("blue", "brown")) +
  theme(legend.position = c(0.1, 0.9), legend.direction = "vertical", legend.title = element_blank(), legend.text = element_text(size = 7)) +
  #ylim(0, 20) +
  xlab("Flow speed (cm/s)") +
  ylab(expression(School~volume~(cm^{"3"})))

volume_plot

ggsave("volume_plot.png", volume_plot, width = 6.5, height = 5)

#an alternative plot that might be better if we treat flow speed as continuous
volume_plot2 <- ggplot(data = volume_data, aes(x = flow, y = school_volume_3d_cm, group = condition, colour = condition, shape = condition)) +
  geom_point(position=position_dodge(2)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbar(aes(ymin = school_volume_3d_cm-se, ymax = school_volume_3d_cm+se), width = 1, position=position_dodge(2)) +
  scale_color_manual(values = c("blue", "brown")) +
  theme(legend.position = c(0.1, 0.9), legend.direction = "vertical", legend.title = element_blank(), legend.text = element_text(size = 7)) +
  #ylim(0, 20) +
  xlab("Flow speed (cm/s)") +
  ylab(expression(School~volume~(cm^{"3"})))

volume_plot2

ggsave("volume_plot2.png", volume_plot2, width = 6.5, height = 5)

#2 ways to analyze, depends on whether we want to treat flow as a continuous or categorical variable
#categorical
volume_model_cat <- lme(school_volume_3d_cm ~ condition * as.factor(flow) + average_length , data = data, random = ~ 1 | as.factor(trial), na.action=na.exclude)
anova(volume_model_cat)
lsmeans(volume_model_cat, pairwise ~ flow)

#continuous
volume_model_con <- lme(school_volume_3d_cm ~ condition * flow + average_length , data = data, random = ~ 1 | as.factor(trial), na.action=na.exclude)
anova(volume_model_con)
emtrends(volume_model_con, pairwise ~ condition, var = "flow")



