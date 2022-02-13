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
library("lmerTest")
install.packages("tidyverse")
install.packages("pkgbuild")
library(pkgbuild)
rtools_path()
library(usethis)
use_git_config(user.name = "egarrett22", user.email = "garrett.emily22@gmail.com")

has_rtools(debug = FALSE)

check_rtools(debug = FALSE)

##principle components
##bodymass and population are highly significantly correlated with PC1, none are correlated with PC2, interaction with PC3
pc1sex <- lmer(pc1~bodymass+pop*acc+(1|sex), data=pcsmusclenz)
summary(pc1sex)
anova(pc1sex)
pc1mod<-lm(pc1~bodymass+pop*acc, data=pcsmusclenz)
anova(pc1mod)
anova(pc1sex,pc1mod)

pc2sex <- lmer(pc2~bodymass+pop*acc+(1|sex), data=pcsmusclenz)
summary(pc2sex)
anova(pc2sex)
pc2sexnobm <-lmer(pc2~pop*acc+(1|sex), data=pcsmusclenz)
summary(pc2sexnobm)
anova(pc2sexnobm)
pc2mod<-lm(pc2~pop*acc, data=pcsmusclenz)
anova(pc2mod)
anova(pc2sexnobm, pc2mod)

pc3sex <- lmer(pc3~bodymass+pop*acc+(1|sex), data=pcsmusclenz)
summary(pc3sex)
anova(pc3sex)
pc3sexnobm<-lmer(pc3~pop*acc+(1|sex), data=pcsmusclenz)
anova(pc3sexnobm)
pc3mod<-lm(pc3~pop*acc, data = pcsmusclenz)
anova(pc3mod)
anova(pc3sexnobm, pc3sex)


lsmeans(pc3sex, pairwise~pop*acc, adjust= "tukey")

##biceps brachii

bbcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbcoxfamsex)
anova(bbcoxfamsex)

bbcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbcoxfam)
anova(bbcoxfam)

bbcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbcoxsex)
anova(bbcoxsex)

bbcox <- lm(cox~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbcox)
anova(bbcox)

bbcoxnobm <- lm(cox~pop*acc, data=bbrachiienzyme)
summary(bbcoxnobm)
anova(bbcoxnobm)

anova(bbcoxfamsex,bbcoxsex)
anova(bbcoxfamsex, bbcoxfam)
anova(bbcoxfamsex,bbcoxnobm)
anova(bbcox, bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##HOAD
bbhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbhoadfamsex)
anova(bbhoadfamsex)

bbhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbhoadfam)
anova(bbhoadfam)

bbhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbhoadsex)
anova(bbhoadsex)

bbhoad<- lm(hoad~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbhoad)
anova(bbhoad)

bbhoadnobm <- lm(hoad~pop*acc, data=bbrachiienzyme)
summary(bbhoadnobm)
anova(bbhoadnobm)

anova(bbhoadfamsex,bbhoadsex)

lsmeans(, pairwise~pop*acc, adjust= "tukey")

##cs
bbcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbcsfamsex)
anova(bbcsfamsex)

bbcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbcsfam)
anova(bbcsfam)

bbcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbcssex)
anova(bbcssex)

bbcs<- lm(cs~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbcs)
anova(bbcs)

bbcsnobm <- lm(cs~pop*acc, data=bbrachiienzyme)
summary(bbcsnobm)
anova(bbcsnobm)

anova(bbcsfamsex,bbcssex)
anova(bbcsfamsex, bbcsfam)

lsmeans(bbcsnobm, pairwise~pop*acc, adjust= "tukey")

##hk
bbhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbhkfamsex)
anova(bbhkfamsex)

bbhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbhkfam)
anova(bbhkfam)

bbhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbhksex)
anova(bbhksex)

bbhk<- lm(hk~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbhk)
anova(bbhk)

bbhknobm <- lm(hk~pop*acc, data=bbrachiienzyme)
summary(bbhknobm)
anova(bbhknobm)

anova(bbhkfamsex,bbhkfam)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
bbpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbpkfamsex)
anova(bbpkfamsex)

bbpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbpkfam)
anova(bbpkfam)

bbpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbpksex)
anova(bbpksex)

bbpk<- lm(pk~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbpk)
anova(bbpk)

bbpknobm <- lm(pk~pop*acc, data=bbrachiienzyme)
summary(bbpknobm)
anova(bbpknobm)

anova(bbpkfamsex,bbpkfam)

lsmeans(bbpknobm, pairwise~pop*acc, adjust= "tukey")

##ldh 
bbldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbldhfamsex)
anova(bbldhfamsex)

bbldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbldhfam)
anova(bbldhfam)

bbldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbldhsex)
anova(bbldhsex)

bbldh<- lm(ldh~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbldh)
anova(bbldh)

bbldhnobm <- lm(ldh~pop*acc, data=bbrachiienzyme)
summary(bbldhnobm)
anova(bbldhnobm)

anova(bbldhfamsex,bbldhsex)
anova(bbldhfamsex, bbldhfam)
anova(bbldh, bbldhnobm)
anova(bbldhfamsex, bbldhnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##biceps femoris 
##cox
bfcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=bfemorisenzyme)
summary(bfcoxfamsex)
anova(bfcoxfamsex)

bfcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=bfemorisenzyme)
summary(bfcoxfam)
anova(bfcoxfam)

bfcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=bfemorisenzyme)
summary(bfcoxsex)
anova(bfcoxsex)

bfcox<- lm(cox~bodymass+pop*acc, data=bfemorisenzyme)
summary(bfcox)
anova(bfcox)

bfcoxnobm <- lm(cox~pop*acc, data=bfemorisenzyme)
summary(bfcoxnobm)
anova(bfcoxnobm)

anova(bfcoxfamsex, bfcoxfam)
anova(bfcoxfamsex, bfcoxsex)

lsmeans(bfcox, pairwise~pop*acc, adjust= "tukey")

#hoad
bfhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=bfemorisenzyme)
summary(bfhoadfamsex)
anova(bfhoadfamsex)

bfhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=bfemorisenzyme)
anova(bfhoadfam)

bfhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=bfemorisenzyme)
summary(bfhoadsex)
anova(bfhoadsex)

bfhoad<- lm(hoad~bodymass+pop*acc, data=bfemorisenzyme)
summary(bfhoad)
anova(bfhoad)

bfhoadnobm <- lm(hoad~pop*acc, data=bfemorisenzyme)
summary(bfhoadnobm)
anova(bfhoadnobm)

anova(bfhoadfamsex, bfhoadfam)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
bfcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=bfemorisenzyme)
summary(bfcsfamsex)
anova(bfcsfamsex)

bfcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=bfemorisenzyme)
summary(bfcsfam)
anova(bfcsfam)

bfcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=bfemorisenzyme)
summary(bfcssex)
anova(bfcssex)

bfcs<- lm(cs~bodymass+pop*acc, data=bfemorisenzyme)
summary(bfcs)
anova(bfcs)

bfcsnobm <- lm(cs~pop*acc, data=bfemorisenzyme)
summary(bfcsnobm)
anova(bfcsnobm)

anova(bfcsfamsex, bfcsfam)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
bfhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=bfemorisenzyme)
summary(bfhkfamsex)
anova(bfhkfamsex)

bfhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=bfemorisenzyme)
summary(bfhkfam)
anova(bfhkfam)

bfhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=bfemorisenzyme)
summary(bfhksex)
anova(bfhksex)

bfhk<- lm(hk~bodymass+pop*acc, data=bfemorisenzyme)
summary(bfhk)
anova(bfhk)

bfhknobm <- lm(hk~pop*acc, data=bfemorisenzyme)
summary(bfhknobm)
anova(bfhknobm)

anova(bfhkfamsex, bfhkfam)
anova(bfhkfamsex, bfhksex)
lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
bfpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=bfemorisenzyme)
summary(bfpkfamsex)
anova(bfpkfamsex)

bfpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=bfemorisenzyme)
summary(bfpkfam)
anova(bfpkfam)

bfpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=bfemorisenzyme)
summary(bfpksex)
anova(bfpksex)

bfpk<- lm(pk~bodymass+pop*acc, data=bfemorisenzyme)
summary(bfpk)
anova(bfpk)

bfpknobm <- lm(pk~pop*acc, data=bfemorisenzyme)
summary(bfpknobm)
anova(bfpknobm)

anova(bfpkfamsex, bfpkfam)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
bfldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=bfemorisenzyme)
summary(bfldhfamsex)
anova(bfldhfamsex)

bfldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=bfemorisenzyme)
summary(bfldhfam)
anova(bfldhfam)

bfldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=bfemorisenzyme)
summary(bfldhsex)
anova(bfldhsex)

bfldh<- lm(ldh~bodymass+pop*acc, data=bfemorisenzyme)
summary(bfldh)
anova(bfldh)

bfldhnobm <- lm(ldh~pop*acc, data=bfemorisenzyme)
summary(bfldhnobm)
anova(bfldhnobm)

anova(bfldhfam, bfldhfamsex)
anova(bfldhfamsex, bfldhsex)
lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##diaphragm
##cox
dpcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=diapenzyme)
summary(dpcoxfamsex)
anova(dpcoxfamsex)

dpcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=diapenzyme)
summary(dpcoxfam)
anova(dpcoxfam)

dpcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=diapenzyme)
summary(dpcoxsex)
anova(dpcoxsex)

dpcox<- lm(cox~bodymass+pop*acc, data=diapenzyme)
summary(dpcox)
anova(dpcox)

dpcoxnobm <- lm(cox~pop*acc, data=diapenzyme)
summary(dpcoxnobm)
anova(dpcoxnobm)

anova(dpcoxfamsex, dpcox)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
dphoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=diapenzyme)
summary(dphoadfamsex)
anova(dphoadfamsex)

dphoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=diapenzyme)
summary(dphoadfam)
anova(dphoadfam)

dphoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=diapenzyme)
summary(dphoadsex)
anova(dphoadsex)

dphoad<- lm(hoad~bodymass+pop*acc, data=diapenzyme)
summary(dphoad)
anova(dphoad)

dphoadnobm <- lm(hoad~pop*acc, data=diapenzyme)
summary(dphoadnobm)
anova(dphoadnobm)

anova(dphoadfamsex, dphoadfam)
anova(dphoadfamsex, dphoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
dpcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=diapenzyme)
summary(dpcsfamsex)
anova(dpcsfamsex)

dpcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=diapenzyme)
summary(dpcsfam)
anova(dpcsfam)

dpcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=diapenzyme)
summary(dpcssex)
anova(dpcssex)

dpcs<- lm(cs~bodymass+pop*acc, data=diapenzyme)
summary(dpcs)
anova(dpcs)

dpcsnobm <- lm(cs~pop*acc, data=diapenzyme)
summary(dpcsnobm)
anova(dpcsnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
dphkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=diapenzyme)
summary(dphkfamsex)
anova(dphkfamsex)

dphkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=diapenzyme)
summary(dphkfam)
anova(dphkfam)

dphksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=diapenzyme)
summary(dphksex)
anova(dphksex)

dphk<- lm(hk~bodymass+pop*acc, data=diapenzyme)
summary(dphk)
anova(dphk)

dphknobm <- lm(hk~pop*acc, data=diapenzyme)
summary(dphknobm)
anova(dphknobm)

anova(dphkfamsex, dphkfam)
anova(dphkfamsex, dphksex)
lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
dppkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=diapenzyme)
summary(dppkfamsex)
anova(dppkfamsex)

dppkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=diapenzyme)
summary(dppkfam)
anova(dppkfam)

dppksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=diapenzyme)
summary(dppksex)
anova(dppksex)

dppk<- lm(pk~bodymass+pop*acc, data=diapenzyme)
summary(dppk)
anova(dppk)

dppknobm <- lm(pk~pop*acc, data=diapenzyme)
summary(dppknobm)
anova(dppknobm)

anova(dppkfamsex, dppkfam)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
dpldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=diapenzyme)
summary(dpldhfamsex)
anova(dpldhfamsex)

dpldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=diapenzyme)
summary(dpldhfam)
anova(dpldhfam)

dpldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=diapenzyme)
summary(dpldhsex)
anova(dpldhsex)

dpldh<- lm(ldh~bodymass+pop*acc, data=diapenzyme)
summary(dpldh)
anova(dpldh)

dpldhnobm <- lm(ldh~pop*acc, data=diapenzyme)
summary(dpldhnobm)
anova(dpldhnobm)

anova(dpldhfamsex, dpldhfam)
anova(dpldhfamsex, dpldhsex)
lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##edl
##cox
edlcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=edlenzyme)
summary(edlcoxfamsex)
anova(edlcoxfamsex)

edlcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=edlenzyme)
summary(edlcoxfam)
anova(edlcoxfam)

edlcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=edlenzyme)
summary(edlcoxsex)
anova(edlcoxsex)

edlcox<- lm(cox~bodymass+pop*acc, data=edlenzyme)
summary(edlcox)
anova(edlcox)

edlcoxnobm <- lm(cox~pop*acc, data=edlenzyme)
summary(edlcoxnobm)
anova(edlcoxnobm)

anova(edlcoxfamsex, edlcoxfam)
anova(edlcoxfamsex, edlcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
edlcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=edlenzyme)
summary(edlcsfamsex)
anova(edlcsfamsex)

edlcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=edlenzyme)
summary(edlcsfam)
anova(edlcsfam)

edlcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=edlenzyme)
summary(edlcssex)
anova(edlcssex)

edlcs<- lm(cs~bodymass+pop*acc, data=edlenzyme)
summary(edlcs)
anova(edlcs)

edlcsnobm <- lm(cs~pop*acc, data=edlenzyme)
summary(edlcsnobm)
anova(edlcsnobm)

anova(edlcsfamsex, edlcsfam)
anova(edlcsfamsex, edlcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
edlhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=edlenzyme)
summary(edlhoadfamsex)
anova(edlhoadfamsex)

edlhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=edlenzyme)
summary(edlhoadfam)
anova(edlhoadfam)

edlhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=edlenzyme)
summary(edlhoadsex)
anova(edlhoadsex)

edlhoad<- lm(hoad~bodymass+pop*acc, data=edlenzyme)
summary(edlhoad)
anova(edlhoad)

edlhoadnobm <- lm(hoad~pop*acc, data=edlenzyme)
summary(edlhoadnobm)
anova(edlhoadnobm)

anova(edlhoadfamsex, edlhoadfam)
anova(edlhoadfamsex, edlhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
edlhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=edlenzyme)
summary(edlhkfamsex)
anova(edlhkfamsex)

edlhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=edlenzyme)
summary(edlhkfam)
anova(edlhkfam)

edlhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=edlenzyme)
summary(edlhksex)
anova(edlhksex)

edlhk<- lm(hk~bodymass+pop*acc, data=edlenzyme)
summary(edlhk)
anova(edlhk)

edlhknobm <- lm(hk~pop*acc, data=edlenzyme)
summary(edlhknobm)
anova(edlhknobm)

anova(edlhkfamsex, edlhkfam)
anova(edlhkfamsex, edlhksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
edlpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=edlenzyme)
summary(edlpkfamsex)
anova(edlpkfamsex)

edlpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=edlenzyme)
summary(edlpkfam)
anova(edlpkfam)

edlpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=edlenzyme)
summary(edlpksex)
anova(edlpksex)

edlpk<- lm(pk~bodymass+pop*acc, data=edlenzyme)
summary(edlpk)
anova(edlpk)

edlpknobm <- lm(pk~pop*acc, data=edlenzyme)
summary(edlpknobm)
anova(edlpknobm)

anova(edlpkfamsex, edlpkfam)
anova(edlpkfamsex, edlpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh 
edlldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=edlenzyme)
summary(edlldhfamsex)
anova(edlldhfamsex)

edlldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=edlenzyme)
summary(edlldhfam)
anova(edlldhfam)

edlldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=edlenzyme)
summary(edlldhsex)
anova(edlldhsex)

edlldh<- lm(ldh~bodymass+pop*acc, data=edlenzyme)
summary(edlldh)
anova(edlldh)

edlldhnobm <- lm(ldh~pop*acc, data=edlenzyme)
summary(edlldhnobm)
anova(edlldhnobm)

anova(edlldhfamsex, edlldhfam)
anova(edlldhfamsex, edlldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##erector spinae 
##cox
escoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=espinaeenzyme)
summary(escoxfamsex)
anova(escoxfamsex)

escoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=espinaeenzyme)
summary(escoxfam)
anova(escoxfam)

escoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=espinaeenzyme)
summary(escoxsex)
anova(escoxsex)

escox<- lm(cox~bodymass+pop*acc, data=espinaeenzyme)
summary(escox)
anova(escox)

escoxnobm <- lm(cox~pop*acc, data=espinaeenzyme)
summary(escoxnobm)
anova(escoxnobm)

anova(escoxfamsex,escoxsex,escoxfam,escoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
eshoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=espinaeenzyme)
summary(eshoadfamsex)
anova(eshoadfamsex)

eshoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=espinaeenzyme)
summary(eshoadfam)
anova(eshoadfam)

eshoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=espinaeenzyme)
summary(eshoadsex)
anova(eshoadsex)

eshoad<- lm(hoad~bodymass+pop*acc, data=espinaeenzyme)
summary(eshoad)
anova(eshoad)

eshoadnobm <- lm(hoad~pop*acc, data=espinaeenzyme)
summary(eshoadnobm)
anova(eshoadnobm)

anova(eshoadfamsex,eshoadsex,eshoadfam,eshoadnobm)
anova(eshoadnobm, eshoadsex)


##cs
escsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=espinaeenzyme)
summary(escsfamsex)
anova(escsfamsex)

escsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=espinaeenzyme)
summary(escsfam)
anova(escsfam)

escssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=espinaeenzyme)
summary(escssex)
anova(escssex)

escs<- lm(cs~bodymass+pop*acc, data=espinaeenzyme)
summary(escs)
anova(escs)

escsnobm <- lm(cs~pop*acc, data=espinaeenzyme)
summary(escsnobm)
anova(escsnobm)

anova(escsfamsex, escsfam)
anova(escsfamsex, escssex)
lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
eshkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=espinaeenzyme)
summary(eshkfamsex)
anova(eshkfamsex)

eshkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=espinaeenzyme)
summary(eshkfam)
anova(eshkfam)

eshksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=espinaeenzyme)
summary(eshksex)
anova(eshksex)

eshk<- lm(hk~bodymass+pop*acc, data=espinaeenzyme)
summary(eshk)
anova(eshk)

eshknobm <- lm(hk~pop*acc, data=espinaeenzyme)
summary(eshknobm)
anova(eshknobm)

anova(eshkfamsex, eshkfam)
anova(eshkfamsex, eshksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
espkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=espinaeenzyme)
summary(espkfamsex)
anova(espkfamsex)

espkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=espinaeenzyme)
summary(espkfam)
anova(espkfam)

espksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=espinaeenzyme)
summary(espksex)
anova(espksex)

espk<- lm(pk~bodymass+pop*acc, data=espinaeenzyme)
summary(espk)
anova(espk)

espknobm <- lm(pk~pop*acc, data=espinaeenzyme)
summary(espknobm)
anova(espknobm)

anova(espkfamsex, espkfam)
anova(espkfamsex, espksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
esldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=espinaeenzyme)
summary(esldhfamsex)
anova(esldhfamsex)

esldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=espinaeenzyme)
summary(esldhfam)
anova(esldhfam)

esldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=espinaeenzyme)
summary(esldhsex)
anova(esldhsex)

esldh<- lm(ldh~bodymass+pop*acc, data=espinaeenzyme)
summary(esldh)
anova(esldh)

esldhnobm <- lm(ldh~pop*acc, data=espinaeenzyme)
summary(esldhnobm)
anova(esldhnobm)

anova(esldhfamsex, esldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##gastroc
##cox
grcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=gastrocenzyme)
summary(grcoxfamsex)
anova(grcoxfamsex)

grcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=gastrocenzyme)
summary(grcoxfam)
anova(grcoxfam)

grcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=gastrocenzyme)
summary(grcoxsex)
anova(grcoxsex)

grcox<- lm(cox~bodymass+pop*acc, data=gastrocenzyme)
summary(grcox)
anova(grcox)

grcoxnobm <- lm(cox~pop*acc, data=gastrocenzyme)
summary(grcoxnobm)
anova(grcoxnobm)

anova(grcoxfamsex, grcoxfam)
anova(grcoxfamsex, grcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
grhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=gastrocenzyme)
summary(grhoadfamsex)
anova(grhoadfamsex)

grhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=gastrocenzyme)
summary(grhoadfam)
anova(grhoadfam)

grhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=gastrocenzyme)
summary(grhoadsex)
anova(grhoadsex)

grhoad<- lm(hoad~bodymass+pop*acc, data=gastrocenzyme)
summary(grhoad)
anova(grhoad)

grhoadnobm <- lm(hoad~pop*acc, data=gastrocenzyme)
summary(grhoadnobm)
anova(grhoadnobm)

anova(grhoadfam, grhoadfamsex)
anova(grhoadfamsex, grhoadsex)
lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
grcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=gastrocenzyme)
summary(grcsfamsex)
anova(grcsfamsex)

grcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=gastrocenzyme)
summary(grcsfam)
anova(grcsfam)

grcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=gastrocenzyme)
summary(grcssex)
anova(grcssex)

grcs<- lm(cs~bodymass+pop*acc, data=gastrocenzyme)
summary(grcs)
anova(grcs)

grcsnobm <- lm(cs~pop*acc, data=gastrocenzyme)
summary(grcsnobm)
anova(grcsnobm)

anova(grcsfamsex, grcsfam)
anova(grcsfamsex, grcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
grhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=gastrocenzyme)
summary(grhkfamsex)
anova(grhkfamsex)

grhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=gastrocenzyme)
summary(grhkfam)
anova(grhkfam)

grhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=gastrocenzyme)
summary(grhksex)
anova(grhksex)

grhk<- lm(hk~bodymass+pop*acc, data=gastrocenzyme)
summary(grhk)
anova(grhk)

grhknobm <- lm(hk~pop*acc, data=gastrocenzyme)
summary(grhknobm)
anova(grhknobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
grpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=gastrocenzyme)
summary(grpkfamsex)
anova(grpkfamsex)

grpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=gastrocenzyme)
summary(grpkfam)
anova(grpkfam)

grpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=gastrocenzyme)
summary(grpksex)
anova(grpksex)

grpk<- lm(pk~bodymass+pop*acc, data=gastrocenzyme)
summary(grpk)
anova(grpk)

grpknobm <- lm(pk~pop*acc, data=gastrocenzyme)
summary(grpknobm)
anova(grpknobm)

anova(grpkfamsex, grpkfam)
anova(grpkfamsex, grpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
grldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=gastrocenzyme)
summary(grldhfamsex)
anova(grldhfamsex)

grldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=gastrocenzyme)
summary(grldhfam)
anova(grldhfam)

grldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=gastrocenzyme)
summary(grldhsex)
anova(grldhsex)

grldh<- lm(ldh~bodymass+pop*acc, data=gastrocenzyme)
summary(grldh)
anova(grldh)

grldhnobm <- lm(ldh~pop*acc, data=gastrocenzyme)
summary(grldhnobm)
anova(grldhnobm)

anova(grldhfamsex, grldhfam)
anova(grldhfamsex, grldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")


##glut max 
##cox
gmcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=glutmaxenzyme)
summary(gmcoxfamsex)
anova(gmcoxfamsex)

gmcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=glutmaxenzyme)
summary(gmcoxfam)
anova(gmcoxfam)

gmcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=glutmaxenzyme)
summary(gmcoxsex)
anova(gmcoxsex)

gmcox<- lm(cox~bodymass+pop*acc, data=glutmaxenzyme)
summary(gmcox)
anova(gmcox)

gmcoxnobm <- lm(cox~pop*acc, data=glutmaxenzyme)
summary(gmcoxnobm)
anova(gmcoxnobm)

anova(gmcoxfamsex, gmcoxfam)
anova(gmcoxfamsex, gmcoxsex)
lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
gmhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=glutmaxenzyme)
summary(gmhoadfamsex)
anova(gmhoadfamsex)

gmhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=glutmaxenzyme)
summary(gmhoadfam)
anova(gmhoadfam)

gmhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=glutmaxenzyme)
summary(gmhoadsex)
anova(gmhoadsex)

gmhoad<- lm(hoad~bodymass+pop*acc, data=glutmaxenzyme)
summary(gmhoad)
anova(gmhoad)

gmhoadnobm <- lm(hoad~pop*acc, data=glutmaxenzyme)
summary(gmhoadnobm)
anova(gmhoadnobm)

anova(gmhoadfamsex, gmhoadfam)
anova(gmhoadfamsex, gmhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
gmcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=glutmaxenzyme)
summary(gmcsfamsex)
anova(gmcsfamsex)

gmcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=glutmaxenzyme)
summary(gmcsfam)
anova(gmcsfam)

gmcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=glutmaxenzyme)
summary(gmcssex)
anova(gmcssex)

gmcs<- lm(cs~bodymass+pop*acc, data=glutmaxenzyme)
summary(gmcs)
anova(gmcs)

gmcsnobm <- lm(cs~pop*acc, data=glutmaxenzyme)
summary(gmcsnobm)
anova(gmcsnobm)

anova(gmcsfamsex, gmcsfam)
anova(gmcsfamsex, gmcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
gmhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=glutmaxenzyme)
summary(gmhkfamsex)
anova(gmhkfamsex)

gmhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=glutmaxenzyme)
summary(gmhkfam)
anova(gmhkfam)

gmhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=glutmaxenzyme)
summary(gmhksex)
anova(gmhksex)

gmhk<- lm(hk~bodymass+pop*acc, data=glutmaxenzyme)
summary(gmhk)
anova(gmhk)

gmhknobm <- lm(hk~pop*acc, data=glutmaxenzyme)
summary(gmhknobm)
anova(gmhknobm)

anova(gmhkfamsex, gmhkfam)
anova(gmhkfamsex, gmhksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
gmpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=glutmaxenzyme)
summary(gmpkfamsex)
anova(gmpkfamsex)

gmpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=glutmaxenzyme)
summary(gmpkfam)
anova(gmpkfam)

gmpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=glutmaxenzyme)
summary(gmpksex)
anova(gmpksex)

gmpk<- lm(pk~bodymass+pop*acc, data=glutmaxenzyme)
summary(gmpk)
anova(gmpk)

gmpknobm <- lm(pk~pop*acc, data=glutmaxenzyme)
summary(gmpknobm)
anova(gmpknobm)

anova(gmpkfamsex, gmpkfam)
anova(gmpkfamsex, gmpksex)

lsmeans(gmpknobm, pairwise~pop*acc, adjust= "tukey")

##ldh
gmldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=glutmaxenzyme)
summary(gmldhfamsex)
anova(gmldhfamsex)

gmldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=glutmaxenzyme)
summary(gmldhfam)
anova(gmldhfam)

gmldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=glutmaxenzyme)
summary(gmldhsex)
anova(gmldhsex)

gmldh<- lm(ldh~bodymass+pop*acc, data=glutmaxenzyme)
summary(gmldh)
anova(gmldh)

gmldhnobm <- lm(ldh~pop*acc, data=glutmaxenzyme)
summary(gmldhnobm)
anova(gmldhnobm)

anova(gmldhfamsex, gmldhfam)
anova(gmldhfamsex, gmldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##intercostals 
##cox
itcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=intenzyme)
summary(itcoxfamsex)
anova(itcoxfamsex)

itcoxfam <- lmer(cox~pop*acc+(1|fam), data=intenzyme)
summary(itcoxfam)
anova(itcoxfam)

itcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=intenzyme)
summary(itcoxsex)
anova(itcoxsex)

itcox<- lm(cox~bodymass+pop*acc, data=intenzyme)
summary(itcox)
anova(itcox)

itcoxnobm <- lm(cox~pop*acc, data=intenzyme)
summary(itcoxnobm)
anova(itcoxnobm)

anova(itcoxfamsex, itcoxfam)
anova(itcoxfamsex, itcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
ithoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=intenzyme)
summary(ithoadfamsex)
anova(ithoadfamsex)

ithoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=intenzyme)
summary(ithoadfam)
anova(ithoadfam)

ithoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=intenzyme)
summary(ithoadsex)
anova(ithoadsex)

ithoad<- lm(hoad~bodymass+pop*acc, data=intenzyme)
summary(ithoad)
anova(ithoad)

ithoadnobm <- lm(hoad~pop*acc, data=intenzyme)
summary(ithoadnobm)
anova(ithoadnobm)

anova(ithoadfamsex, ithoadfam)
anova(ithoadfamsex, ithoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
itcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=intenzyme)
summary(itcsfamsex)
anova(itcsfamsex)

itcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=intenzyme)
summary(itcsfam)
anova(itcsfam)

itcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=intenzyme)
summary(itcssex)
anova(itcssex)

itcs<- lm(cs~bodymass+pop*acc, data=intenzyme)
summary(itcs)
anova(itcs)

itcsnobm <- lm(cs~pop*acc, data=intenzyme)
summary(itcsnobm)
anova(itcsnobm)

anova(itcsfamsex, itcsfam)
anova(itcsfamsex, itcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
ithkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=intenzyme)
summary(ithkfamsex)
anova(ithkfamsex)

ithkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=intenzyme)
summary(ithkfam)
anova(ithkfam)

ithksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=intenzyme)
summary(ithksex)
anova(ithksex)

ithk<- lm(hk~bodymass+pop*acc, data=intenzyme)
summary(ithk)
anova(ithk)

ithknobm <- lm(hk~pop*acc, data=intenzyme)
summary(ithknobm)
anova(ithknobm)

anova(ithkfamsex, ithkfam)
anova(ithkfamsex, ithksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
itpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=intenzyme)
summary(itpkfamsex)
anova(itpkfamsex)

itpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=intenzyme)
summary(itpkfam)
anova(itpkfam)

itpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=intenzyme)
summary(itpksex)
anova(itpksex)

itpk<- lm(pk~bodymass+pop*acc, data=intenzyme)
summary(itpk)
anova(itpk)

itpknobm <- lm(pk~pop*acc, data=intenzyme)
summary(itpknobm)
anova(itpknobm)

anova(itpkfamsex, itpkfam)
anova(itpkfamsex, itpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
itldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=intenzyme)
summary(itldhfamsex)
anova(itldhfamsex)

itldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=intenzyme)
summary(itldhfam)
anova(itldhfam)

itldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=intenzyme)
summary(itldhsex)
anova(itldhsex)

itldh<- lm(ldh~bodymass+pop*acc, data=intenzyme)
summary(itldh)
anova(itldh)

itldhnobm <- lm(ldh~pop*acc, data=intenzyme)
summary(itldhnobm)
anova(itldhnobm)

anova(itldhfamsex, itldhfam)
anova(itldhfamsex, itldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")


##lower trapezeus 
##cox
ltcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=ltrapenzyme)
summary(ltcoxfamsex)
anova(ltcoxfamsex)

ltcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=ltrapenzyme)
summary(ltcoxfam)
anova(ltcoxfam)

ltcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=ltrapenzyme)
summary(ltcoxsex)
anova(ltcoxsex)

ltcox<- lm(cox~bodymass+pop*acc, data=ltrapenzyme)
summary(ltcox)
anova(ltcox)

ltcoxnobm <- lm(cox~pop*acc, data=ltrapenzyme)
summary(ltcoxnobm)
anova(ltcoxnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
lthoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=ltrapenzyme)
summary(lthoadfamsex)
anova(lthoadfamsex)

lthoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=ltrapenzyme)
summary(lthoadfam)
anova(lthoadfam)

lthoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=ltrapenzyme)
summary(lthoadsex)
anova(lthoadsex)

lthoad<- lm(hoad~bodymass+pop*acc, data=ltrapenzyme)
summary(lthoad)
anova(lthoad)

lthoadnobm <- lm(hoad~pop*acc, data=ltrapenzyme)
summary(lthoadnobm)
anova(lthoadnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
ltcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=ltrapenzyme)
summary(ltcsfamsex)
anova(ltcsfamsex)

ltcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=ltrapenzyme)
summary(ltcsfam)
anova(ltcsfam)

ltcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=ltrapenzyme)
summary(ltcssex)
anova(ltcssex)

ltcs<- lm(cs~bodymass+pop*acc, data=ltrapenzyme)
summary(ltcs)
anova(ltcs)

ltcsnobm <- lm(cs~pop*acc, data=ltrapenzyme)
summary(ltcsnobm)
anova(ltcsnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
lthkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=ltrapenzyme)
summary(lthkfamsex)
anova(lthkfamsex)

lthkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=ltrapenzyme)
summary(lthkfam)
anova(lthkfam)

lthksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=ltrapenzyme)
summary(lthksex)
anova(lthksex)

lthk<- lm(hk~bodymass+pop*acc, data=ltrapenzyme)
summary(lthk)
anova(lthk)

lthknobm <- lm(hk~pop*acc, data=ltrapenzyme)
summary(lthknobm)
anova(lthknobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
ltpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=ltrapenzyme)
summary(ltpkfamsex)
anova(ltpkfamsex)

ltpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=ltrapenzyme)
summary(ltpkfam)
anova(ltpkfam)

ltpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=ltrapenzyme)
summary(ltpksex)
anova(ltpksex)

ltpk<- lm(pk~bodymass+pop*acc, data=ltrapenzyme)
summary(ltpk)
anova(ltpk)

ltpknobm <- lm(pk~pop*acc, data=ltrapenzyme)
summary(ltpknobm)
anova(ltpknobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
ltldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=ltrapenzyme)
summary(ltldhfamsex)
anova(ltldhfamsex)

ltldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=ltrapenzyme)
summary(ltldhfam)
anova(ltldhfam)

ltldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=ltrapenzyme)
summary(ltldhsex)
anova(ltldhsex)

ltldh<- lm(ldh~bodymass+pop*acc, data=ltrapenzyme)
summary(ltldh)
anova(ltldh)

ltldhnobm <- lm(ldh~pop*acc, data=ltrapenzyme)
summary(ltldhnobm)
anova(ltldhnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##masseter 
##cox
mscoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=massterenzyme)
summary(mscoxfamsex)
anova(mscoxfamsex)

mscoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=massterenzyme)
summary(mscoxfam)
anova(mscoxfam)

mscoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=massterenzyme)
summary(mscoxsex)
anova(mscoxsex)

mscox<- lm(cox~bodymass+pop*acc, data=massterenzyme)
summary(mscox)
anova(mscox)

mscoxnobm <- lm(cox~pop*acc, data=massterenzyme)
summary(mscoxnobm)
anova(mscoxnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
mshoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=massterenzyme)
summary(mshoadfamsex)
anova(mshoadfamsex)

mshoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=massterenzyme)
summary(mshoadfam)
anova(mshoadfam)

mshoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=massterenzyme)
summary(mshoadsex)
anova(mshoadsex)

mshoad<- lm(hoad~bodymass+pop*acc, data=massterenzyme)
summary(mshoad)
anova(mshoad)

mshoadnobm <- lm(hoad~pop*acc, data=massterenzyme)
summary(mshoadnobm)
anova(mshoadnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
mscsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=massterenzyme)
summary(mscsfamsex)
anova(mscsfamsex)

mscsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=massterenzyme)
summary(mscsfam)
anova(mscsfam)

mscssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=massterenzyme)
summary(mscssex)
anova(mscssex)

mscs<- lm(cs~bodymass+pop*acc, data=massterenzyme)
summary(mscs)
anova(mscs)

mscsnobm <- lm(cs~pop*acc, data=massterenzyme)
summary(mscsnobm)
anova(mscsnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
mshkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=massterenzyme)
summary(mshkfamsex)
anova(mshkfamsex)

mshkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=massterenzyme)
summary(mshkfam)
anova(mshkfam)

mshksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=massterenzyme)
summary(mshksex)
anova(mshksex)

mshk<- lm(hk~bodymass+pop*acc, data=massterenzyme)
summary(mshk)
anova(mshk)

mshknobm <- lm(hk~pop*acc, data=massterenzyme)
summary(mshknobm)
anova(mshknobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
mspkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(mspkfamsex)
anova(mspkfamsex)

mspkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=)
summary(mspkfam)
anova(mspkfam)

mspksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=)
summary(mspksex)
anova(mspksex)

mspk<- lm(pk~bodymass+pop*acc, data=)
summary(mspk)
anova(mspk)

mspknobm <- lm(pk~pop*acc, data=massterenzyme)
summary(mspknobm)
anova(mspknobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
msldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=massterenzyme)
summary(msldhfamsex)
anova(msldhfamsex)

msldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=massterenzyme)
summary(msldhfam)
anova(msldhfam)

msldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=massterenzyme)
summary(msldhsex)
anova(msldhsex)

msldh<- lm(ldh~bodymass+pop*acc, data=massterenzyme)
summary(msldh)
anova(msldh)

msldhnobm <- lm(ldh~pop*acc, data=massterenzyme)
summary(msldhnobm)
anova(msldhnobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##medial trapezius 
##cox
mtcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=medtrapenzyme)
summary(mtcoxfamsex)
anova(mtcoxfamsex)

mtcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=medtrapenzyme)
summary(mtcoxfam)
anova(mtcoxfam)

mtcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=medtrapenzyme)
summary(mtcoxsex)
anova(mtcoxsex)

mtcox<- lm(cox~bodymass+pop*acc, data=medtrapenzyme)
summary(mtcox)
anova(mtcox)

mtcoxnobm <- lm(cox~pop*acc, data=medtrapenzyme)
summary(mtcoxnobm)
anova(mtcoxnobm)

anova(mtcoxfamsex, mtcoxfam)
anova(mtcoxfamsex, mtcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
mthoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=medtrapenzyme)
summary(mthoadfamsex)
anova(mthoadfamsex)

mthoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=medtrapenzyme)
summary(mthoadfam)
anova(mthoadfam)

mthoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=medtrapenzyme)
summary(mthoadsex)
anova(mthoadsex)

mthoad<- lm(hoad~bodymass+pop*acc, data=medtrapenzyme)
summary(mthoad)
anova(mthoad)

mthoadnobm <- lm(hoad~pop*acc, data=medtrapenzyme)
summary(mthoadnobm)
anova(mthoadnobm)

anova(mthoadfamsex, mthoadfam)
anova(mthoadfamsex, mthoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
mtcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=medtrapenzyme)
summary(mtcsfamsex)
anova(mtcsfamsex)

mtcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=medtrapenzyme)
summary(mtcsfam)
anova(mtcsfam)

mtcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=medtrapenzyme)
summary(mtcssex)
anova(mtcssex)

mtcs<- lm(cs~bodymass+pop*acc, data=medtrapenzyme)
summary(mtcs)
anova(mtcs)

mtcsnobm <- lm(cs~pop*acc, data=medtrapenzyme)
summary(mtcsnobm)
anova(mtcsnobm)

anova(mtcsfamsex, mtcsfam)
anova(mtcsfamsex, mtcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
mthkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=medtrapenzyme)
summary(mthkfamsex)
anova(mthkfamsex)

mthkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=medtrapenzyme)
summary(mthkfam)
anova(mthkfam)

mthksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=medtrapenzyme)
summary(mthksex)
anova(mthksex)

mthk<- lm(hk~bodymass+pop*acc, data=medtrapenzyme)
summary(mthk)
anova(mthk)

mthknobm <- lm(hk~pop*acc, data=medtrapenzyme)
summary(mthknobm)
anova(mthknobm)

anova(mthkfamsex, mthkfam)
anova(mthkfamsex, mthksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
mtpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=medtrapenzyme)
summary(mtpkfamsex)
anova(mtpkfamsex)

mtpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=medtrapenzyme)
summary(mtpkfam)
anova(mtpkfam)

mtpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=medtrapenzyme)
summary(mtpksex)
anova(mtpksex)

mtpk<- lm(pk~bodymass+pop*acc, data=medtrapenzyme)
summary(mtpk)
anova(mtpk)

mtpknobm <- lm(pk~pop*acc, data=medtrapenzyme)
summary(mtpknobm)
anova(mtpknobm)

anova(mtpkfamsex, mtpkfam)
anova(mtpkfamsex, mtpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
mtldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=medtrapenzyme)
summary(mtldhfamsex)
anova(mtldhfamsex)

mtldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=medtrapenzyme)
summary(mtldhfam)
anova(mtldhfam)

mtldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=medtrapenzyme)
summary(mtldhsex)
anova(mtldhsex)

mtldh<- lm(ldh~bodymass+pop*acc, data=medtrapenzyme)
summary(mtldh)
anova(mtldh)

mtldhnobm <- lm(ldh~pop*acc, data=medtrapenzyme)
summary(mtldhnobm)
anova(mtldhnobm)

anova(mtldhfamsex, mtldhfam)
anova(mtldhfamsex, mtldhfam)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pectoralis major 
##cox
pmcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=pecmajorenzyme)
summary(pmcoxfamsex)
anova(pmcoxfamsex)

pmcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=pecmajorenzyme)
summary(pmcoxfam)
anova(pmcoxfam)

pmcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=pecmajorenzyme)
summary(pmcoxsex)
anova(pmcoxsex)

pmcox<- lm(cox~bodymass+pop*acc, data=pecmajorenzyme)
summary(pmcox)
anova(pmcox)

pmcoxnobm <- lm(cox~pop*acc, data=pecmajorenzyme)
summary(pmcoxnobm)
anova(pmcoxnobm)

anova(pmcoxfamsex, pmcoxfam)
anova(pmcoxfamsex, pmcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
pmhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=pecmajorenzyme)
summary(pmhoadfamsex)
anova(pmhoadfamsex)

pmhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=pecmajorenzyme)
summary(pmhoadfam)
anova(pmhoadfam)

pmhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=pecmajorenzyme)
summary(pmhoadsex)
anova(pmhoadsex)

pmhoad<- lm(hoad~bodymass+pop*acc, data=pecmajorenzyme)
summary(pmhoad)
anova(pmhoad)

pmhoadnobm <- lm(hoad~pop*acc, data=pecmajorenzyme)
summary(pmhoadnobm)
anova(pmhoadnobm)

anova(pmhoadfamsex, pmhoadfam)
anova(pmhoadfamsex, pmhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
pmcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=pecmajorenzyme)
summary(pmcsfamsex)
anova(pmcsfamsex)

pmcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=pecmajorenzyme)
summary(pmcsfam)
anova(pmcsfam)

pmcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=pecmajorenzyme)
summary(pmcssex)
anova(pmcssex)

pmcs<- lm(cs~bodymass+pop*acc, data=pecmajorenzyme)
summary(pmcs)
anova(pmcs)

pmcsnobm <- lm(cs~pop*acc, data=pecmajorenzyme)
summary(pmcsnobm)
anova(pmcsnobm)

anova(pmcsfam, pmcsfamsex)
anova(pmcsfamsex, pmcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
pmhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=pecmajorenzyme)
summary(pmhkfamsex)
anova(pmhkfamsex)

pmhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=pecmajorenzyme)
summary(pmhkfam)
anova(pmhkfam)

pmhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=pecmajorenzyme)
summary(pmhksex)
anova(pmhksex)

pmhk<- lm(hk~bodymass+pop*acc, data=pecmajorenzyme)
summary(pmhk)
anova(pmhk)

pmhknobm <- lm(hk~pop*acc, data=pecmajorenzyme)
summary(pmhknobm)
anova(pmhknobm)

anova(pmhkfamsex, pmhkfam)
anova(pmhkfamsex, pmhksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
pmpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=pecmajorenzyme)
summary(pmpkfamsex)
anova(pmpkfamsex)

pmpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=pecmajorenzyme)
summary(pmpkfam)
anova(pmpkfam)

pmpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=pecmajorenzyme)
summary(pmpksex)
anova(pmpksex)

pmpk<- lm(pk~bodymass+pop*acc, data=pecmajorenzyme)
summary(pmpk)
anova(pmpk)

pmpknobm <- lm(pk~pop*acc, data=pecmajorenzyme)
summary(pmpknobm)
anova(pmpknobm)

anova(pmpkfam, pmpkfamsex)
anova(pmpkfamsex, pmpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
pmldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=pecmajorenzyme)
summary(pmldhfamsex)
anova(pmldhfamsex)

pmldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=pecmajorenzyme)
summary(pmldhfam)
anova(pmldhfam)

pmldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=pecmajorenzyme)
summary(pmldhsex)
anova(pmldhsex)

pmldh<- lm(ldh~bodymass+pop*acc, data=pecmajorenzyme)
summary(pmldh)
anova(pmldh)

pmldhnobm <- lm(ldh~pop*acc, data=pecmajorenzyme)
summary(pmldhnobm)
anova(pmldhnobm)

anova(pmldhfamsex, pmldhfam)
anova(pmldhfamsex, pmldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##plantaris
##cox
plcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=plantarisenzyme)
summary(plcoxfamsex)
anova(plcoxfamsex)

plcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=plantarisenzyme)
summary(plcoxfam)
anova(plcoxfam)

plcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=plantarisenzyme)
summary(plcoxsex)
anova(plcoxsex)

plcox<- lm(cox~bodymass+pop*acc, data=plantarisenzyme)
summary(plcox)
anova(plcox)

plcoxnobm <- lm(cox~pop*acc, data=plantarisenzyme)
summary(plcoxnobm)
anova(plcoxnobm)

anova(plcoxfam, plcoxfamsex)
anova(plcoxfamsex, plcoxsex)
lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
plhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=plantarisenzyme)
summary(plhoadfamsex)
anova(plhoadfamsex)

plhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=plantarisenzyme)
summary(plhoadfam)
anova(plhoadfam)

plhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=plantarisenzyme)
summary(plhoadsex)
anova(plhoadsex)

plhoad<- lm(hoad~bodymass+pop*acc, data=plantarisenzyme)
summary(plhoad)
anova(plhoad)

plhoadnobm <- lm(hoad~pop*acc, data=plantarisenzyme)
summary(plhoadnobm)
anova(plhoadnobm)

anova(plhoadfamsex, plhoadfam)
anova(plhoadfamsex, plhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
plcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=plantarisenzyme)
summary(plcsfamsex)
anova(plcsfamsex)

plcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=plantarisenzyme)
summary(plcsfam)
anova(plcsfam)

plcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=plantarisenzyme)
summary(plcssex)
anova(plcssex)

plcs<- lm(cs~bodymass+pop*acc, data=plantarisenzyme)
summary(plcs)
anova(plcs)

plcsnobm <- lm(cs~pop*acc, data=plantarisenzyme)
summary(plcsnobm)
anova(plcsnobm)

anova(plcsfamsex, plcsfam)
anova(plcsfamsex, plcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
plhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=plantarisenzyme)
summary(plhkfamsex)
anova(plhkfamsex)

plhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=plantarisenzyme)
summary(plhkfam)
anova(plhkfam)

plhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=plantarisenzyme)
summary(plhksex)
anova(plhksex)

plhk<- lm(hk~bodymass+pop*acc, data=plantarisenzyme)
summary(plhk)
anova(plhk)

plhknobm <- lm(hk~pop*acc, data=plantarisenzyme)
summary(plhknobm)
anova(plhknobm)

anova(plhkfamsex, plhksex)
anova(plhkfamsex, plhkfam)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
plpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=plantarisenzyme)
summary(plpkfamsex)
anova(plpkfamsex)

plpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=plantarisenzyme)
summary(plpkfam)
anova(plpkfam)

plpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=plantarisenzyme)
summary(plpksex)
anova(plpksex)

plpk<- lm(pk~bodymass+pop*acc, data=plantarisenzyme)
summary(plpk)
anova(plpk)

plpknobm <- lm(pk~pop*acc, data=plantarisenzyme)
summary(plpknobm)
anova(plpknobm)

anova(plpkfamsex, plpkfam)
anova(plpkfamsex, plpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
plldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=plantarisenzyme)
summary(plldhfamsex)
anova(plldhfamsex)

plldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=plantarisenzyme)
summary(plldhfam)
anova(plldhfam)

plldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=plantarisenzyme)
summary(plldhsex)
anova(plldhsex)

plldh<- lm(ldh~bodymass+pop*acc, data=plantarisenzyme)
summary(plldh)
anova(plldh)

plldhnobm <- lm(ldh~pop*acc, data=plantarisenzyme)
summary(plldhnobm)
anova(plldhnobm)

anova(plldhfamsex, plldhfam)
anova(plldhfamsex, plldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##rectus femoris 
##cox
rfcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=recfemenzyme)
summary(rfcoxfamsex)
anova(rfcoxfamsex)

rfcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=recfemenzyme)
summary(rfcoxfam)
anova(rfcoxfam)

rfcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=recfemenzyme)
summary(rfcoxsex)
anova(rfcoxsex)

rfcox<- lm(cox~bodymass+pop*acc, data=recfemenzyme)
summary(rfcox)
anova(rfcox)

rfcoxnobm <- lm(cox~pop*acc, data=recfemenzyme)
summary(rfcoxnobm)
anova(rfcoxnobm)

anova(rfcoxfamsex, rfcoxfam)
anova(rfcoxfamsex, rfcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
rfhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=recfemenzyme)
summary(rfhoadfamsex)
anova(rfhoadfamsex)

rfhoad <- lmer(hoad~bodymass+pop*acc+(1|fam), data=recfemenzyme)
summary(rfhoadfam)
anova(rfhoadfam)

rfhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=recfemenzyme)
summary(rfhoadsex)
anova(rfhoadsex)

rfhoad<- lm(hoad~bodymass+pop*acc, data=recfemenzyme)
summary(rfhoad)
anova(rfhoad)

rfhoadnobm <- lm(hoad~pop*acc, data=recfemenzyme)
summary(rfhoadnobm)
anova(rfhoadnobm)

anova(rfhoadfam, rfhoadfamsex)
anova(rfhoadfamsex, rfhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
rfcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=recfemenzyme)
summary(rfcsfamsex)
anova(rfcsfamsex)

rfcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=recfemenzyme)
summary(rfcsfam)
anova(rfcsfam)

rfcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=recfemenzyme)
summary(rfcssex)
anova(rfcssex)

rfcs<- lm(cs~bodymass+pop*acc, data=recfemenzyme)
summary(rfcs)
anova(rfcs)

rfcsnobm <- lm(cs~pop*acc, data=recfemenzyme)
summary(rfcsnobm)
anova(rfcsnobm)

anova(rfcsfamsex, rfcsfam)
anova(rfcsfamsex, rfcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
rfhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=recfemenzyme)
summary(rfhkfamsex)
anova(rfhkfamsex)

rfhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=recfemenzyme)
summary(rfhkfam)
anova(rfhkfam)

rfhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=recfemenzyme)
summary(rfhksex)
anova(rfhksex)

rfhk<- lm(hk~bodymass+pop*acc, data=recfemenzyme)
summary(rfhk)
anova(rfhk)

rfhknobm <- lm(hk~pop*acc, data=recfemenzyme)
summary(rfhknobm)
anova(rfhknobm)

anova(rfhkfamsex, rfhkfam)
anova(rfhkfamsex, rfhksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
rfpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=recfemenzyme)
summary(rfpkfamsex)
anova(rfpkfamsex)

rfpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=recfemenzyme)
summary(rfpkfam)
anova(rfpkfam)

rfpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=recfemenzyme)
summary(rfpksex)
anova(rfpksex)

rfpk<- lm(pk~bodymass+pop*acc, data=recfemenzyme)
summary(rfpk)
anova(rfpk)

rfpknobm <- lm(pk~pop*acc, data=recfemenzyme)
summary(rfpknobm)
anova(rfpknobm)

anova(rfpkfamsex, rfpkfam)
anova(rfpkfamsex, rfpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
rfldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=recfemenzyme)
summary(rfldhfamsex)
anova(rfldhfamsex)

rfldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=recfemenzyme)
summary(rfldhfam)
anova(rfldhfam)

rfldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=recfemenzyme)
summary(rfldhsex)
anova(rfldhsex)

rfldh<- lm(ldh~bodymass+pop*acc, data=recfemenzyme)
summary(rfldh)
anova(rfldh)

rfldhnobm <- lm(ldh~pop*acc, data=recfemenzyme)
summary(rfldhnobm)
anova(rfldhnobm)

anova(rfldhfamsex, rfldhfam)
anova(rfldhfamsex, rfldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")


##semitendinosus
##cox
smcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=semienzyme)
summary(smcoxfamsex)
anova(smcoxfamsex)

smcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=semienzyme)
summary(smcoxfam)
anova(smcoxfam)

smcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=semienzyme)
summary(smcoxsex)
anova(smcoxsex)

smcox<- lm(cox~bodymass+pop*acc, data=semienzyme)
summary(smcox)
anova(smcox)

smcoxnobm <- lm(cox~pop*acc, data=semienzyme)
summary(smcoxnobm)
anova(smcoxnobm)

anova(smcoxfamsex, smcoxfam)
anova(smcoxfamsex, smcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
smhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=semienzyme)
summary(smhoadfamsex)
anova(smhoadfamsex)

smhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=semienzyme)
summary(smhoadfam)
anova(smhoadfam)

smhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=semienzyme)
summary(smhoadsex)
anova(smhoadsex)

smhoad<- lm(hoad~bodymass+pop*acc, data=semienzyme)
summary(smhoad)
anova(smhoad)

smhoadnobm <- lm(hoad~pop*acc, data=semienzyme)
summary(smhoadnobm)
anova(smhoadnobm)

anova(smhoadfamsex, smhoadfam)
anova(smhoadfamsex, smhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
smcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=semienzyme)
summary(smcsfamsex)
anova(smcsfamsex)

smcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=semienzyme)
summary(smcsfam)
anova(smcsfam)

smcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=semienzyme)
summary(smcssex)
anova(smcssex)

smcs<- lm(cs~bodymass+pop*acc, data=semienzyme)
summary(smcs)
anova(smcs)

smcsnobm <- lm(cs~pop*acc, data=semienzyme)
summary(smcsnobm)
anova(smcsnobm)

anova(smcsfamsex, smcsfam)
anova(smcsfamsex, smcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
smhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=semienzyme)
summary(smhkfamsex)
anova(smhkfamsex)

smhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=semienzyme)
summary(smhkfam)
anova(smhkfam)

smhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=semienzyme)
summary(smhksex)
anova(smhksex)

smhk<- lm(hk~bodymass+pop*acc, data=semienzyme)
summary(smhk)
anova(smhk)

smhknobm <- lm(hk~pop*acc, data=semienzyme)
summary(smhknobm)
anova(smhknobm)

anova(smhkfamsex, smhkfam)
anova(smhkfamsex, smhksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
smpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=semienzyme)
summary(smpkfamsex)
anova(smpkfamsex)

smpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=semienzyme)
summary(smpkfam)
anova(smpkfam)

smpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=semienzyme)
summary(smpksex)
anova(smpksex)

smpk<- lm(pk~bodymass+pop*acc, data=semienzyme)
summary(smpk)
anova(smpk)

smpknobm <- lm(pk~pop*acc, data=semienzyme)
summary(smpknobm)
anova(smpknobm)

anova(smpkfamsex, smpkfam)
anova(smpkfamsex, smpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
smldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=semienzyme)
summary(smldhfamsex)
anova(smldhfamsex)

smldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=semienzyme)
summary(smldhfam)
anova(smldhfam)

smldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=semienzyme)
summary(smldhsex)
anova(smldhsex)

smldh<- lm(ldh~bodymass+pop*acc, data=semienzyme)
summary(smldh)
anova(smldh)

smldhnobm <- lm(ldh~pop*acc, data=semienzyme)
summary(smldhnobm)
anova(smldhnobm)

anova(smldhfamsex, smldhfam)
anova(smldhfamsex, smldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##soleus
##cox
socoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=soleusenzyme)
summary(socoxfamsex)
anova(socoxfamsex)

socoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=soleusenzyme)
summary(socoxfam)
anova(socoxfam)

socoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=soleusenzyme)
summary(socoxsex)
anova(socoxsex)

socox<- lm(cox~bodymass+pop*acc, data=soleusenzyme)
summary(socox)
anova(socox)

socoxnobm <- lm(cox~pop*acc, data=soleusenzyme)
summary(socoxnobm)
anova(socoxnobm)

anova(socoxfamsex, socoxfam)
anova(socoxfamsex, socoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
sohoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=soleusenzyme)
summary(sohoadfamsex)
anova(sohoadfamsex)

sohoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=soleusenzyme)
summary(sohoadfam)
anova(sohoadfam)

sohoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=soleusenzyme)
summary(sohoadsex)
anova(sohoadsex)

sohoad<- lm(hoad~bodymass+pop*acc, data=soleusenzyme)
summary(sohoad)
anova(sohoad)

sohoadnobm <- lm(hoad~pop*acc, data=soleusenzyme)
summary(sohoadnobm)
anova(sohoadnobm)

anova(sohoadfamsex, sohoadfam)
anova(sohoadfamsex, sohoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
socsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=soleusenzyme)
summary(socsfamsex)
anova(socsfamsex)

socsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=soleusenzyme)
summary(socsfam)
anova(socsfam)

socssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=soleusenzyme)
summary(socssex)
anova(socssex)

socs<- lm(cs~bodymass+pop*acc, data=soleusenzyme)
summary(socs)
anova(socs)

socsnobm <- lm(cs~pop*acc, data=soleusenzyme)
summary(socsnobm)
anova(socsnobm)

anova(socsfam, socsfamsex)
anova(socsfamsex, socssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
sohkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=soleusenzyme)
summary(sohkfamsex)
anova(sohkfamsex)

sohkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=soleusenzyme)
summary(sohkfam)
anova(sohkfam)

sohksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=soleusenzyme)
summary(sohksex)
anova(sohksex)

sohk<- lm(hk~bodymass+pop*acc, data=soleusenzyme)
summary(sohk)
anova(sohk)

sohknobm <- lm(hk~pop*acc, data=soleusenzyme)
summary(sohknobm)
anova(sohknobm)

anova(bbcoxfamsex,bbcoxsex,bbcoxfam,bbcoxnobm)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
sopkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=soleusenzyme)
summary(sopkfamsex)
anova(sopkfamsex)

sopkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=soleusenzyme)
summary(sopkfam)
anova(sopkfam)

sopksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=soleusenzyme)
summary(sopksex)
anova(sopksex)

sopk<- lm(pk~bodymass+pop*acc, data=soleusenzyme)
summary(sopk)
anova(sopk)

sopknobm <- lm(pk~pop*acc, data=soleusenzyme)
summary(sopknobm)
anova(sopknobm)

anova(sopkfamsex, sopkfam)
anova(sopkfamsex, sopksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
soldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=soleusenzyme)
summary(soldhfamsex)
anova(soldhfamsex)

soldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=soleusenzyme)
summary(soldhfam)
anova(soldhfam)

soldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=soleusenzyme)
summary(soldhsex)
anova(soldhsex)

soldh<- lm(ldh~bodymass+pop*acc, data=soleusenzyme)
summary(soldh)
anova(soldh)

soldhnobm <- lm(ldh~pop*acc, data=soleusenzyme)
summary(soldhnobm)
anova(soldhnobm)

anova(soldhfam, soldhfamsex)
anova(soldhfamsex, soldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##tibialis anterior 
##cox
tacoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=taenzyme)
summary(tacoxfamsex)
anova(tacoxfamsex)

tacoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=taenzyme)
summary(tacoxfam)
anova(tacoxfam)

tacoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=taenzyme)
summary(tacoxsex)
anova(tacoxsex)

tacox<- lm(cox~bodymass+pop*acc, data=taenzyme)
summary(tacox)
anova(tacox)

tacoxnobm <- lm(cox~pop*acc, data=taenzyme)
summary(tacoxnobm)
anova(tacoxnobm)

anova(tacoxfamsex, tacoxfam)
anova(tacoxfamsex, tacoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
tahoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=taenzyme)
summary(tahoadfamsex)
anova(tahoadfamsex)

tahoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=taenzyme)
summary(tahoadfam)
anova(tahoadfam)

tahoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=taenzyme)
summary(tahoadsex)
anova(tahoadsex)

tahoad<- lm(hoad~bodymass+pop*acc, data=taenzyme)
summary(tahoad)
anova(tahoad)


tahoadnobm <- lm(hoad~pop*acc, data=taenzyme)
summary(tahoadnobm)
anova(tahoadnobm)

anova(tahoadfam, tahoadfamsex)
anova(tahoadfamsex, tahoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
tacsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=taenzyme)
summary(tacsfamsex)
anova(tacsfamsex)

tacsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=taenzyme)
summary(tacsfam)
anova(tacsfam)

tacssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=taenzyme)
summary(tacssex)
anova(tacssex)

tacs<- lm(cs~bodymass+pop*acc, data=taenzyme)
summary(tacs)
anova(tacs)

tacsnobm <- lm(cs~pop*acc, data=taenzyme)
summary(tacsnobm)
anova(tacsnobm)

anova(tacsfamsex, tacsfam)
anova(tacsfamsex, tacssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
tahkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=taenzyme)
summary(tahkfamsex)
anova(tahkfamsex)

tahkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=taenzyme)
summary(tahkfam)
anova(tahkfam)

tahksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=taenzyme)
summary(tahksex)
anova(tahksex)

tahk<- lm(hk~bodymass+pop*acc, data=taenzyme)
summary(tahk)
anova(tahk)

tahknobm <- lm(hk~pop*acc, data=taenzyme)
summary(tahknobm)
anova(tahknobm)

anova(tahkfamsex, tahkfam)
anova(tahkfamsex, tahksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
tapkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=taenzyme)
summary(tapkfamsex)
anova(tapkfamsex)

tapkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=taenzyme)
summary(tapkfam)
anova(tapkfam)

tapksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=taenzyme)
summary(tapksex)
anova(tapksex)

tapk<- lm(pk~bodymass+pop*acc, data=taenzyme)
summary(tapk)
anova(tapk)

tapknobm <- lm(pk~pop*acc, data=taenzyme)
summary(tapknobm)
anova(tapknobm)

anova(tapkfamsex,tapkfam)
anova(tapkfamsex, tapksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
taldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=taenzyme)
summary(taldhfamsex)
anova(taldhfamsex)

taldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=taenzyme)
summary(taldhfam)
anova(taldhfam)

taldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=taenzyme)
summary(taldhsex)
anova(taldhsex)

taldh<- lm(ldh~bodymass+pop*acc, data=taenzyme)
summary(taldh)
anova(taldh)

taldhnobm <- lm(ldh~pop*acc, data=taenzyme)
summary(taldhnobm)
anova(taldhnobm)

anova(taldhfamsex,taldhfam)
anova(taldhfamsex, taldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")


##triceps 
##cox
trcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=tricepsenzyme)
summary(trcoxfamsex)
anova(trcoxfamsex)

trcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=tricepsenzyme)
summary(trcoxfam)
anova(trcoxfam)

trcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=tricepsenzyme)
summary(trcoxsex)
anova(trcoxsex)

trcox<- lm(cox~bodymass+pop*acc, data=tricepsenzyme)
summary(trcox)
anova(trcox)

trcoxnobm <- lm(cox~pop*acc, data=tricepsenzyme)
summary(trcoxnobm)
anova(trcoxnobm)

anova(trcoxfamsex,trcoxfam)
anova(trcoxfamsex, trcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
trhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=tricepsenzyme)
summary(trhoadfamsex)
anova(trhoadfamsex)

trhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=tricepsenzyme)
summary(trhoadfam)
anova(trhoadfam)

trhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=tricepsenzyme)
summary(trhoadsex)
anova(trhoadsex)

trhoad<- lm(hoad~bodymass+pop*acc, data=tricepsenzyme)
summary(trhoad)
anova(trhoad)

trhoadnobm <- lm(hoad~pop*acc, data=tricepsenzyme)
summary(trhoadnobm)
anova(trhoadnobm)

anova(trhoadfamsex,trhoadfam)
anova(trhoadfamsex, trhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
trcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=tricepsenzyme)
summary(trcsfamsex)
anova(trcsfamsex)

trcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=tricepsenzyme)
summary(trcsfam)
anova(trcsfam)

trcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=tricepsenzyme)
summary(trcssex)
anova(trcssex)

trcs<- lm(cs~bodymass+pop*acc, data=tricepsenzyme)
summary(trcs)
anova(trcs)

trcsnobm <- lm(cs~pop*acc, data=tricepsenzyme)
summary(trcsnobm)
anova(trcsnobm)

anova(trcsfamsex, trcsfam)
anova(trcsfamsex, trcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
trhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=tricepsenzyme)
summary(trhkfamsex)
anova(trhkfamsex)

trhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=tricepsenzyme)
summary(trhkfam)
anova(trhkfam)

trhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=tricepsenzyme)
summary(trhksex)
anova(trhksex)

trhk<- lm(hk~bodymass+pop*acc, data=tricepsenzyme)
summary(trhk)
anova(trhk)

trhknobm <- lm(hk~pop*acc, data=tricepsenzyme)
summary(trhknobm)
anova(trhknobm)

anova(trhkfamsex,trhkfam)
anova(trhkfamsex, trhksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
trpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=tricepsenzyme)
summary(trpkfamsex)
anova(trpkfamsex)

trpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=tricepsenzyme)
summary(trpkfam)
anova(trpkfam)

trpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=tricepsenzyme)
summary(trpksex)
anova(trpksex)

trpk<- lm(pk~bodymass+pop*acc, data=tricepsenzyme)
summary(trpk)
anova(trpk)

trpknobm <- lm(pk~pop*acc, data=tricepsenzyme)
summary(trpknobm)
anova(trpknobm)

anova(trpkfamsex,trpkfam)
anova(trpkfamsex, trpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
trldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=tricepsenzyme)
summary(trldhfamsex)
anova(trldhfamsex)

trldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=tricepsenzyme)
summary(trldhfam)
anova(trldhfam)

trldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=tricepsenzyme)
summary(trldhsex)
anova(trldhsex)

trldh<- lm(ldh~bodymass+pop*acc, data=tricepsenzyme)
summary(trldh)
anova(trldh)

trldhnobm <- lm(ldh~pop*acc, data=tricepsenzyme)
summary(trldhnobm)
anova(trldhnobm)

anova(trldhfamsex,trldhfam)
anova(trldhfamsex, trldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##vastus lateralis 
##cox
vlcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=vlatenzyme)
summary(vlcoxfamsex)
anova(vlcoxfamsex)

vlcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=vlatenzyme)
summary(vlcoxfam)
anova(vlcoxfam)

vlcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=vlatenzyme)
summary(vlcoxsex)
anova(vlcoxsex)

vlcox<- lm(cox~bodymass+pop*acc, data=vlatenzyme)
summary(vlcox)
anova(vlcox)

vlcoxnobm <- lm(cox~pop*acc, data=vlatenzyme)
summary(vlcoxnobm)
anova(vlcoxnobm)

anova(vlcoxfamsex,vlcoxfam)
anova(vlcoxfamsex, vlcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
vlhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=vlatenzyme)
summary(vlhoadfamsex)
anova(vlhoadfamsex)

vlhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=vlatenzyme)
summary(vlhoadfam)
anova(vlhoadfam)

vlhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=vlatenzyme)
summary(vlhoadsex)
anova(vlhoadsex)

vlhoad<- lm(hoad~bodymass+pop*acc, data=vlatenzyme)
summary(vlhoad)
anova(vlhoad)

vlhoadnobm <- lm(hoad~pop*acc, data=vlatenzyme)
summary(vlhoadnobm)
anova(vlhoadnobm)

anova(vlhoadfamsex,vlhoadfam)
anova(vlhoadfamsex, vlhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
vlcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=vlatenzyme)
summary(vlcsfamsex)
anova(vlcsfamsex)

vlcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=vlatenzyme)
summary(vlcsfam)
anova(vlcsfam)

vlcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=vlatenzyme)
summary(vlcssex)
anova(vlcssex)

vlcs<- lm(cs~bodymass+pop*acc, data=vlatenzyme)
summary(vlcs)
anova(vlcs)

vlcsnobm <- lm(cs~pop*acc, data=vlatenzyme)
summary(vlcsnobm)
anova(vlcsnobm)

anova(vlcsfamsex,vlcsfam)
anova(vlcsfamsex, vlcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
vlhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=vlatenzyme)
summary(vlhkfamsex)
anova(vlhkfamsex)

vlhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=vlatenzyme)
summary(vlhkfam)
anova(vlhkfam)

vlhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=vlatenzyme)
summary(vlhksex)
anova(vlhksex)

vlhk<- lm(hk~bodymass+pop*acc, data=vlatenzyme)
summary(vlhk)
anova(vlhk)

vlhknobm <- lm(hk~pop*acc, data=vlatenzyme)
summary(vlhknobm)
anova(vlhknobm)

anova(vlhkfamsex,vlhkfam)
anova(vlhkfamsex, vlhksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
vlpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=vlatenzyme)
summary(vlpkfamsex)
anova(vlpkfamsex)

vlpkfam <- lmer(pk~pop*acc+(1|fam), data=vlatenzyme)
summary(vlpkfam)
anova(vlpkfam)

vlpksex <- lmer(pk~pop*acc+(1|sex), data=vlatenzyme)
summary(vlpksex)
anova(vlpksex)

vlpk<- lm(pk~bodymass+pop*acc, data=vlatenzyme)
summary(vlpk)
anova(vlpk)

vlpknobm <- lm(pk~pop*acc, data=vlatenzyme)
summary(vlpknobm)
anova(vlpknobm)

anova(vlpkfamsex,vlpkfam)
anova(vlpkfamsex, vlpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
vlldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=vlatenzyme)
summary(vlldhfamsex)
anova(vlldhfamsex)

vlldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=vlatenzyme)
summary(vlldhfam)
anova(vlldhfam)

vlldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=vlatenzyme)
summary(vlldhsex)
anova(vlldhsex)

vlldh<- lm(ldh~bodymass+pop*acc, data=vlatenzyme)
summary(vlldh)
anova(vlldh)

vlldhnobm <- lm(ldh~pop*acc, data=vlatenzyme)
summary(vlldhnobm)
anova(vlldhnobm)

anova(vlldhfamsex,vlldhfam)
anova(vlldhfamsex, vlldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##vastus medialis
##cox
vmcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=vmedenzyme)
summary(vmcoxfamsex)
anova(vmcoxfamsex)

vmcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=vmedenzyme)
summary(vmcoxfam)
anova(vmcoxfam)

vmcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=vmedenzyme)
summary(vmcoxsex)
anova(vmcoxsex)

vmcox<- lm(cox~bodymass+pop*acc, data=vmedenzyme)
summary(vmcox)
anova(vmcox)

vmcoxnobm <- lm(cox~pop*acc, data=vmedenzyme)
summary(vmcoxnobm)
anova(vmcoxnobm)

anova(vmcoxfamsex,vmcoxfam)
anova(vmcoxfamsex, vmcoxsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
vmhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=vmedenzyme)
summary(vmhoadfamsex)
anova(vmhoadfamsex)

vmhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=vmedenzyme)
summary(vmhoadfam)
anova(vmhoadfam)

vmhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=vmedenzyme)
summary(vmhoadsex)
anova(vmhoadsex)

vmhoad<- lm(hoad~bodymass+pop*acc, data=vmedenzyme)
summary(vmhoad)
anova(vmhoad)

vmhoadnobm <- lm(hoad~pop*acc, data=vmedenzyme)
summary(vmhoadnobm)
anova(vmhoadnobm)

anova(vmhoadfamsex,vmhoadfam)
anova(vmhoadfamsex, vmhoadsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
vmcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=vmedenzyme)
summary(vmcsfamsex)
anova(vmcsfamsex)

vmcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=vmedenzyme)
summary(vmcsfam)
anova(vmcsfam)

vmcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=vmedenzyme)
summary(vmcssex)
anova(vmcssex)


vmcs<- lm(cs~bodymass+pop*acc, data=vmedenzyme)
summary(vmcs)
anova(vmcs)

vmcsnobm <- lm(cs~pop*acc, data=vmedenzyme)
summary(vmcsnobm)
anova(vmcsnobm)

anova(vmcsfamsex,vmcsfam)
anova(vmcsfamsex, vmcssex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
vmhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=vmedenzyme)
summary(vmhkfamsex)
anova(vmhkfamsex)

vmhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=vmedenzyme)
summary(vmhkfam)
anova(vmhkfam)

vmhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=vmedenzyme)
summary(vmhksex)
anova(vmhksex)

vmhk<- lm(hk~bodymass+pop*acc, data=vmedenzyme)
summary(vmhk)
anova(vmhk)

vmhknobm <- lm(hk~pop*acc, data=vmedenzyme)
summary(vmhknobm)
anova(vmhknobm)

anova(vmhkfamsex,vmhkfam)
anova(vmhkfamsex, vmhksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
vmpkfamsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=vmedenzyme)
summary(vmpkfamsex)
anova(vmpkfamsex)

vmpkfam <- lmer(pk~bodymass+pop*acc+(1|fam), data=vmedenzyme)
summary(vmpkfam)
anova(vmpkfam)

vmpksex <- lmer(pk~bodymass+pop*acc+(1|sex), data=vmedenzyme)
summary(vmpksex)
anova(vmpksex)

vmpk<- lm(pk~bodymass+pop*acc, data=vmedenzyme)
summary(vmpk)
anova(vmpk)

vmpknobm <- lm(pk~pop*acc, data=vmedenzyme)
summary(vmpknobm)
anova(vmpknobm)

anova(vmpkfamsex,vmpkfam)
anova(vmpkfamsex, vmpksex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
vmldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=vmedenzyme)
summary(vmldhfamsex)
anova(vmldhfamsex)

vmldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=vmedenzyme)
summary(vmldhfam)
anova(vmldhfam)

vmldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=vmedenzyme)
summary(vmldhsex)
anova(vmldhsex)

vmldh<- lm(ldh~bodymass+pop*acc, data=vmedenzyme)
summary(vmldh)
anova(vmldh)

vmldhnobm <- lm(ldh~pop*acc, data=vmedenzyme)
summary(vmldhnobm)
anova(vmldhnobm)

anova(vmldhfamsex,vmldhfam)
anova(vmldhfamsex, vmldhsex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")










Extra stuff
##cox
famsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(cox~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(cox~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(cox~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(cox~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
famsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(hoad~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(hoad~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
famsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(cs~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(cs~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(cs~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(cs~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
famsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(hk~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(hk~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(hk~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(hk~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
famsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(pk~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(pk~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(pk~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(pk~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
famsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(ldh~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(ldh~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##muscle 
##cox
famsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(cox~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(cox~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(cox~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(cox~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hoad
famsex <- lmer(hoad~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(hoad~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(hoad~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##cs
famsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(cs~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(cs~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(cs~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(cs~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##hk
famsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(hk~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(hk~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(hk~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(hk~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##pk
famsex <- lmer(pk~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(pk~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(pk~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(pk~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(pk~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")

##ldh
famsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)
anova(famsex)

fam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=)
summary(fam)
anova(fam)

sex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=)
summary(sex)
anova(sex)

<- lm(ldh~bodymass+pop*acc, data=)
summary()
anova()

nobm <- lm(ldh~pop*acc, data=)
summary(nobm)
anova(nobm)

anova(famsex,fam)
anova(famsex, sex)

lsmeans(bbcoxnobm, pairwise~pop*acc, adjust= "tukey")