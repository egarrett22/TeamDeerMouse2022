##Practicing and running a PCA

mtcars
mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

summary(mtcars.pca)

str(mtcars.pca)


library(devtools)
library(ggbiplot)

ggbiplot(mtcars.pca)

ggbiplot(mtcars.pca, labels=rownames(mtcars))

mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))

ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)

ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),   labels=rownames(mtcars), groups=mtcars.country)


##to remove the arrow from the plot 
ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=rownames(mtcars), groups=mtcars.country)


ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country) +
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
  ggtitle("PCA of mtcars dataset")+
  theme_minimal()+
  theme(legend.position = "bottom")





##dealing with missing values in a PCA
library(pcaMethods)
library(devtools)
library(ggbiplot)
data(metaboliteData)
data(metaboliteDataComplete)
md <- prep(metaboliteData, scale="none", center=TRUE)
mdC <- prep(metaboliteDataComplete, scale="none", center=TRUE)
resSVDI <- pca(md, method="svdImpute", center=FALSE, nPcs=5)
resSVDI
slplot(resSVDI)
plotPcs(resSVDI)

pc <- pca(iris)
irdf <- merge(iris, scores(pc), by=0)
library(ggplot2)
ggplot(irdf, aes(PC1, PC2, colour=Species)) +
  geom_point() +
  stat_ellipse()

ggplot(irdf, aes(PC1, PC2, colour=Species)) +
  geom_point() +
  stat_ellipse()



library(pcaMethods)
data(metaboliteData)
print(metaboliteData)
mD <- metaboliteData
sum(is.na(mD))

##estimate missing data 
pc <- pca(mD, nPcs=3, method="ppca")
imputed <- completeObs(pc)

##compare imputed data set to the original values
data(metaboliteDataComplete)
mdComp <- metaboliteDataComplete
sum((mdComp[is.na(mD)] - imputed[is.na(mD)])^2) / sum(mdComp[is.na(mD)]^2)


##different PCA method
imputedNipals <- completeObs(pca(mD, nPcs=3, method="nipals"))
sum((mdComp[is.na(mD)] - imputedNipals[is.na(mD)])^2) / sum(mdComp[is.na(mD)]^2)



