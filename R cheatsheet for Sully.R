#if using certain packages, load their libraries now
#install.packages("tidyverse")
library(tidyverse)

#or install them if you don't have them (install only once)
#install.packages code is highlighted below when needed
install.packages("")

#to import a file without setting the working directory (wd)

name=read.csv(file.choose(),header=T) #will prompt you to select a file
attach(name) #required to attach file to current work for callback

#altneratively, set the wd so that you can call multiple files within a folder

getwd() #to see what the currently directory is set to
setwd("C:/Users/Catie/Desktop") #change as appropriate

#to load an excel file through the wd
name<- read.csv("file_name.csv", 
                header = TRUE, 
                sep=",") 
#the separator in the file. we used a ".CSV" file, or COMMA separated values. so our sep is ","

#---------------- T-test - for comparing 2 groups

t.test(y~x, data = name)

#or
#for normally distributed (unequal variance) - Welch's two sample t-test

t.test(col1, col2, alternative = c("less" or "greater" or "two.sided"))
#note: less is col1 < col2, greater is col1 > col2

#for paired t-test

t.test(col1, col2, paired=TRUE)

#Mann-Whitney t-test - for non normally distributed data

wilcox.test(y~x, alternative = c("less/greater/two.sided"), data = name)

#---------------- Multiple (>2 comparisons)

#if a factor is expressed as numbers in a column, must tell R it's a factor
file$columnname<-as.factor(file$columnname)

#ANOVAs - comparing more than 2 groups

name <- aov(y~x1*x2, data = file)
summary(name)

#Tukey post-hoc test - for categorical variables

TukeyHSD(name)

#2-way repeated measures ANOVA

name <- aov(y~x1*x2+Error(factor), data = file)
summary(name)

#Linear Regression

reg <- lm(y~x1*x2+x3..., data = name)
aov(reg)
summary(reg)

#Linear Mixed Effects Model

library(nlme)
model <- lme(y~x1*x2+..., random=~1|variable, data = file)
aov(reg)
summary(reg)

#Linear mixed effects model with covariate and random effects
#install.packages("lme4")
#install.packages("lmerTest")
library(lme4)
library(lmerTest)

model_random <- lmer(y~covariate+x1*x2*...+(1|random effect), data = name)
model <- lmer(y~covariate+x1*x2..., data = name)

anova(model_random,model) #compares models to determine if random effect is significant

anova(model) #gives you output of the model that you want
summary(model)

#post-hoc test for mixed linear model with random factors
#install.packages("lsmeans")
library(lsmeans)

lsmeans(model, pairwise~var1*var2, ref = "control_name", adjust = "tukey")

#can use "sidak"
#pairwise can be switched for trt.vs.ctrl (will take 1st in alphabet)
