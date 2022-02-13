library("tidyverse")
library("cowplot")
library("car")
library("multcomp")
library("lme4")
library("nlme")
library("lsmeans")
library("emmeans")
library("multcompView")
library("DataExplorer")
library("devtools")
library("ggplot2")
library("lattice")
library("plyr")
library("Rmisc")
library(lmerTest)

Semitendinosus<- read.csv("Semitendinosus_Enzyme_Summary.csv")
print(Semitendinosus)

names(Semitendinosus)


#this works too
COX_data <- summarySE(Semitendinosus, measurevar='COX', groupvars=c("Population", "Acclimation"))
print(COX_data)

names (COX_data)


##this works
ggplot(COX_data, aes(x = Acclimation, y = COX)) +
  geom_point(aes(color = Population))

##this works too
##scale_X_discrete sets the order of the x axis categoricals
ggplot(COX_data, aes(x=Acclimation, y=COX, colour=Population)) + 
  geom_errorbar(aes(ymin=COX-se, ymax=COX+se), width=.1) +
  geom_line() +
  geom_point()+
  scale_x_discrete(limits = c("NX", "HX"))







#Linear mixed effects model with covariate and random effects

##this formula should be the same as the one underneath it
model_random_COX <- lmer(COX ~ Mass + Population * Acclimation + (1|Family) + (1|Sex), data= Semitendinosus)
summary(model_random_COX)



##example from the internet (this seems to work OK)
mixed.lmerCOX <- lmer(COX ~ Mass + Population * Acclimation + (1|Family) + (1|Sex) , data = Semitendinosus)
summary(mixed.lmerCOX)
print(mixed.lmerCOX)

mixed.lmerCOXwosex <- lmer(COX ~ Mass + Population * Acclimation + (1|Family) , data = Semitendinosus)
summary(mixed.lmerCOXwosex)

mixed.lmerCOXnorand <- lmer(COX ~ Mass + Population * Acclimation , data = Semitendinosus)
summary(mixed.lmerCOXnorand)



anova(mixed.lmerCOXnorand)
##need to figure out how to run the formula without a random effect, I get an error code saying that there is no random effect as is it needs one
##does running just the lm function work?
##im not sure that these are properly picking out acclimation and population differences





##anova comparing more than one group (what Sully showed me)
coxsemi <- aov(COX~Population*Acclimation, data = Semitendinosus)
summary(coxsemi)


coxdiap1 <- aov(COX~Population*Acclimation, data = Diaphragm)
summary(coxdiap1)



##Lukes attempt at graphing                        
ggplot(Semitendinosus,aes(x=Acclimation, y=COX, color = Population), size = 2)+
  geom_point()+
  geom_errorbar(aes(x = Acclimation, ymin = lower.CL, ymax = upper.CL, width = 0.2, color = Populuation))+
  scale_y_continuous (name = "COX")+
  scale_x_discrete(name = "Acclimation")


##trying COX from the diaphragm 
##not sure why this one is only saying 23 observations, maybe because there is one individual without a mass? What can I do to fix this missing mass issue?
Diaphragm <- read.csv("Diaphragm_Enzyme_Summary.csv")
names(Diaphragm)


model_random_Diaphragm <- lmer(COX ~ Mass + Population * Acclimation + (1|Family) + (1|Sex), data= Diaphragm)
summary(model_random_Diaphragm)

mixed.lmerDiaphragm <- lmer(COX ~ Mass + Population * Acclimation + (1|Family) , data = Diaphragm)
summary(mixed.lmerDiaphragm)

mixed.lmerDiaphragmrorand <- lm(COX ~ Mass + Population * Acclimation , data = Diaphragm)
summary(mixed.lmerDiaphragmrorand)












