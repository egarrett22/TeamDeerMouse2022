##for updating R and carying over your library
#install.packages("installr")
##library(installr)
##updateR()


##how Catie said to run the linear model, run the anova on the model as well
bbhoadfam <- lmer(hoad~bodymass+pop*acc+(1|fam), data=bbrachiienzyme)
summary(bbhoadfam)
anova(bbhoadfam)






##Wilcoxon tests


##bicepsbrachii
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)


wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=bbrachiiwilcox)

##bicepsfemoris
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=bfemoriswilcox)

##diaphragm
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=diapwilcox)

##EDL
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=edlwilcox)

##erector spinae 
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=espinaewilcox)



##gastroc 
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=gastrocwilcox)


##glutmax
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=glutmaxwilcox)

##intercostals 
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=intwilcox)

##lower trapezius 
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=ltrapwilcox)

##masseter 
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=masseterwilcox)

##medial trapezius
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=medtrapwilcox)

##pec major
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=pecmajorwilcox)

##plantaris 
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=plantariswilcox)

##rectus femoris
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=recfemwilcox)
##semitendinosus
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=semiwilcox)
##soleus
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=soleuswilcox)
##tibialis anterios
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=tawilcox)
##triceps
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=tricepswilcox)
##vastus lateralis
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=vlatwilcox)
##vastus medialis 
wilcox.test(coxhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(hoadhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(cshx~pophx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(hkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(pkhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(ldhhx~pophx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)

wilcox.test(coxnx~popnx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(hoadnx~popnx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(csnx~popnx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(hknx~popnx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(pknx~popnx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)
wilcox.test(ldhnx~popnx, alternative=c("two.sided"), paired = FALSE, data=vmedwilcox)




##between muscle characterization within an acclimation and enzyme
##COX
wilcox.test(coxnx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)
wilcox.test(coxhx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)
wilcox.exact(coxnx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)

##hoad
wilcox.test(hoadnx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)
wilcox.test(hoadhx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)

##CS
wilcox.test(csnx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)
wilcox.test(cshx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)

##hk
wilcox.test(hknx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)
wilcox.test(hkhx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)

##pk
wilcox.test(pknx ~ pop, alternative=c("greater"), paired=TRUE, data=betweenmuscles)
wilcox.test(pkhx ~ pop, alternative=c("greater"), paired=TRUE, data=betweenmuscles)

##ldh
wilcox.test(ldhnx ~ pop, alternative=c("greater"), paired=TRUE, data=betweenmuscles)
wilcox.test(ldhhx ~ pop, alternative=c("greater"), paired=TRUE, data=betweenmuscles)

##muscle mass plots x vs y : include muscle masses in the PCA, dont include sex and family in PCA
library(exactRankTests)

wilcox.test(ldhnx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)
wilcox.test(ldhhx ~ pop, alternative=c("less"), paired=TRUE, data=betweenmuscles)

##checking muscle mass to see if there is any effects of population on muscle mass
wilcox.test(menx ~ lnnx, alternative=c("less"), paired=TRUE, data=musclemassbybodymass)
wilcox.test(mehx ~ lnhx, alternative=c("less"), paired=TRUE, data=musclemassbybodymass)


wilcox.test(menx ~ lnnx, alternative=c("greater"), paired=TRUE, data=musclemassbybodymass)
wilcox.test(mmhx ~ pop, alternative=c("greater"), paired=TRUE, data=musclemassbybodymass)

wilcox.test(mmnx ~ pop, alternative=c("two.sided"), paired=TRUE, data=musclemassbybodymass)
wilcox.test(mmhx ~ pop, alternative=c("two.sided"), paired=TRUE, data=musclemassbybodymass)

##muscle mass/body mass comparison between highland and lowlanders
wilcox.test(menx ~ lnnx, alternative=c("less"), paired=TRUE, data=musclemasswilcox)
wilcox.test(mehx ~ lnhx, alternative=c("less"), paired=TRUE, data=musclemasswilcox)

wilcox.test(ggnx ~ ggpop, alternative=c("greater"), paired=TRUE, data=musclemasswilcox)
wilcox.test(gghx ~ ggpop, alternative=c("greater"), paired=TRUE, data=musclemasswilcox)

wilcox.test(ggnx ~ ggpop, alternative=c("two.sided"), paired=TRUE, data=musclemasswilcox)
wilcox.test(gghx ~ ggpop, alternative=c("two.sided"), paired=TRUE, data=musclemasswilcox)
