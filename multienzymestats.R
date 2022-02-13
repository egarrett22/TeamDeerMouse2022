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



##biceps brachii (bb)

##COX
bbcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbcoxfamsex)

bbcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbcoxfam)

bbcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbcoxsex)

bbcox <- lm(cox~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbcox)

anova(bbcoxfamsex,bbcoxsex)

lsmeans(bbcoxfamsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbcoxfam, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbcoxsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbcox, pairwise~pop*acc, adjust= "tukey")

bbaovcox <-aov(cox~bodymass+pop*acc , data=bbrachiienzyme)
summary(bbaovcox)
TukeyHSD(bbaovcox)



##bb HOAD

bbhoadfamsex <- lmer(hoad~bodymass+pop*acc+(1|fam) + (1|sex), data=bbrachiienzyme)
summary(bbhoadfamsex)

bbhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbhoadfam)
anova(bbhoadfam)
plot(bbhoadfam)


bbhoadsex <- lmer(hoad~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbhoadsex)

bbhoad <- lm(hoad~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbhoad)

anova(bbhoadfamsex,bbhoadsex)

lsmeans(bbhoadfamsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhoadfam, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhoadsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhoad, pairwise~pop*acc, adjust= "tukey")

bbaovhoad <-aov(hoad~bodymass+pop*acc , data=bbrachiienzyme)
summary(bbaovhoad)
TukeyHSD(bbaovhoad)
TukeyHSD(aov(hoad~bodymass+pop*acc , data=bbrachiienzyme),"pop")


##bbCS
bbcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbcsfamsex)

bbcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbcsfam)

bbcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbcssex)

bbcs <- lm(cs~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbcs)

anova(bbcsfamsex,bbcssex)

lsmeans(bbcsfamsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbcsfam, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbcssex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbcs, pairwise~pop*acc, adjust= "tukey")

bbaovcs <-aov(cs~bodymass+pop*acc , data=bbrachiienzyme)
summary(bbaovcs)
TukeyHSD(bbaovcs)

##bbLDH
bbldhfamsex <- lmer(ldh~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbldhfamsex)

bbldhfam <- lmer(ldh~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbldhfam)

bbldhsex <- lmer(ldh~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbldhsex)

bbldh <- lm(ldh~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbldh)

anova(bbldhfamsex,bbldhsex)

lsmeans(bbldhfamsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbldhfam, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbldhsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbldh, pairwise~pop*acc, adjust= "tukey")


bbaovldh <-aov(ldh~bodymass+pop*acc , data=bbrachiienzyme)
summary(bbaovldh)
TukeyHSD(bbaovldh)

##bbHK
bbhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=bbrachiienzyme)
summary(bbhkfamsex)

bbhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbhkfam)

bbhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=bbrachiienzyme)
summary(bbhksex)

bbhk <- lm(hk~bodymass+pop*acc, data=bbrachiienzyme)
summary(bbhk)

anova(bbhkfamsex,bbhksex)
anova(bbhkfamsex, bbhkfam)

lsmeans(bbhkfamsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhkfam, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhksex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhk, pairwise~pop*acc, adjust= "tukey")

bbaovhk <-aov(hk~bodymass+pop*acc , data=bbrachiienzyme)
summary(bbaovhk)
TukeyHSD(bbaovhk)



##beformis (bf)

bfcoxfamsex <- lmer(cox~bodymass+pop*acc+(1|fam)+(1|sex), data=bfemorisenzyme)
summary(bfcoxfamsex)

bfcoxfam <- lmer(cox~bodymass+pop*acc+(1|fam), data=bfemorisenzyme)
summary(bfcoxfam)

bfcoxsex <- lmer(cox~bodymass+pop*acc+(1|sex), data=bfemorisenzyme)
summary(bfcoxsex)

bfcox <- lm(cox~bodymass+pop*acc, data=bfemorisenzyme)
summary(bfcox)

anova(bfcoxfamsex,bfcoxsex)

lsmeans(bfcoxfamsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bfcoxfam, pairwise~pop*acc, adjust= "tukey")
lsmeans(bfcoxsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bfcox, pairwise~pop*acc, adjust= "tukey")

bfaovcox <-aov(cox~bodymass+pop*acc , data=bfemorisenzyme)
summary(bfaovcox)
TukeyHSD(bfaovcox)



##ldh
bfhkfamsex <- lmer(hk~bodymass+pop*acc+(1|fam)+(1|sex), data=bfemorisenzyme)
summary(bfhkfamsex)

bfhkfam <- lmer(hk~bodymass+pop*acc+(1|fam), data=bfemorisenzyme)
summary(bfhkfam)

bfhksex <- lmer(hk~bodymass+pop*acc+(1|sex), data=bfemorisenzyme)
summary(bfhksex)

bfhk <- lm(hk~bodymass+pop*acc, data=bfemorisenzyme)
summary(bfhk)

anova(bfhkfamsex,bfhksex)
anova(bfhkfamsex, bfhkfam)

lsmeans(bbhkfamsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhkfam, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhksex, pairwise~pop*acc, adjust= "tukey")
lsmeans(bbhk, pairwise~pop*acc, adjust= "tukey")




##vastus medialis 

##vmcs
vmcsfamsex <- lmer(cs~bodymass+pop*acc+(1|fam)+(1|sex), data=vmedenzyme)
summary(vmcsfamsex)

vmcsfam <- lmer(cs~bodymass+pop*acc+(1|fam), data=vmedenzyme)
summary(vmcsfam)

vmcssex <- lmer(cs~bodymass+pop*acc+(1|sex), data=vmedenzyme)
summary(vmcssex)

vmcs <- lm(cs~bodymass+pop*acc, data=vmedenzyme)
summary(vmcs)

anova(vmcsfamsex,vmcssex)

lsmeans(vmcsfamsex, pairwise~pop*acc, adjust= "tukey")
lsmeans(vmcsfam, pairwise~pop*acc, adjust= "tukey")
lsmeans(vmcssex, pairwise~pop*acc, adjust= "tukey")
lsmeans(vmcs, pairwise~pop*acc, adjust= "tukey")

vmaovcs <-aov(cs~bodymass + pop*acc , data=vmedenzyme)
summary(vmaovcs)
TukeyHSD(vmaovcs)



##blank codes/formulas 

famsex <- lmer(~bodymass+pop*acc+(1|fam)+(1|sex), data=)
summary(famsex)

fam <- lmer(~bodymass+pop*acc+(1|fam), data=)
summary(fam)

sex <- lmer(~bodymass+pop*acc+(1|sex), data=)
summary(sex)

 <- lm(~bodymass+pop*acc, data=)
summary(bfhk)

anova(famsex,sex)
anova(famsex, fam)


