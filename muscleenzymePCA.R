library(devtools)
library(ggbiplot)
library(pcaMethods)
library(Biobase)
library(factoextra)
library(GEOquery)
library(mixOmics)
install.packages('mixOmics')
install.packages("ggbiplot")
install.packages("pcaMethods")
install.packages("Biobase")
install.packages("factoextra")
install.packages("devtools")
install.packages("GEOquery")
##load the data set "MuscleenzymePCAdata"
##this is how you would do it if your data set was PERFECT and you werent missing any values. not the case in my data set, I have alot of random missing values
##when a PCA encounters a missing value, it completely removes that individual from the data set, which I cannot afford to do in this case, we had a low number of individuals to begin with, we need to include all the data we have 
muscleenzymePCAdata2 <- na.omit(MuscleenzymePCAdata)
print(muscleenzymePCAdata2)

muscleenzymePCAdata3 <- muscleenzymePCAdata2[,3:121]
print(muscleenzymePCAdata3)

muscleenzyme.pca <- prcomp(muscleenzymePCAdata3, center = TRUE,scale. = TRUE)

summary(muscleenzyme.pca)

print(muscleenzyme.pca)

biplot(muscleenzyme.pca)

plot(muscleenzyme.pca)

ggbiplot(muscleenzyme.pca)


gpop <- ggbiplot(muscleenzyme.pca, obs.scale = 1, var.scale = 1, groups = muscleenzymePCAdata2[,2], ellipse = TRUE, circle = TRUE)
gpop

ggbiplot(muscleenzyme.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE, groups = muscleenzymePCAdata2[,2])

gacc <- ggbiplot(muscleenzyme.pca, obs.scale = 1, var.scale = 1, groups = muscleenzymePCAdata2[,3], ellipse = TRUE, circle = TRUE)
gacc

ggbiplot(muscleenzyme.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE, groups = muscleenzymePCAdata2[,3])

PC12<-ggbiplot(muscleenzyme.pca,ellipse=TRUE,choices=c(1,2), var.axes=FALSE, groups=muscleenzymePCAdata2[,2])
PC12
PC23<-ggbiplot(muscleenzyme.pca,ellipse=TRUE,choices=c(2,3), var.axes=FALSE, groups=muscleenzymePCAdata2[,2])
PC23
PC34<-ggbiplot(muscleenzyme.pca,ellipse=TRUE,choices=c(3,4), var.axes=FALSE, groups=muscleenzymePCAdata2[,2])
PC34
##################################################################################################################

##screen plot of PC dimension versus percentage of explained variance
fviz_eig(muscleenzyme.pca)


##running PCA without omitting missing samples- update 11.29.2021: ran into a problem here, PCAs dont like missing values 
muscleenzyme.pca2 <- prcomp(MuscleenzymePCAdata[,4,123], center = TRUE,scale. = TRUE)
summary(muscleenzyme.pca2)

print(muscleenzyme.pca2)

biplot(muscleenzyme.pca2)

plot(muscleenzyme.pca2)

ggbiplot(muscleenzyme.pca2)




##other ways of plotting PCAs
library(factoextra)

##eigan values 
fviz_eig(muscleenzyme.pca)

fviz_pca_ind(muscleenzyme.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(muscleenzyme.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



fviz_pca_biplot(muscleenzyme.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)



##Muscle enzyme PCA attempt: 02.12.2021
MuscleenzymePCAdata



muscleenzymepc <- pca(MuscleenzymePCAdata, nPcs=10, method="ppca")
imputedmuscleenzyme <- completeObs(muscleenzymepc)
print(imputedmuscleenzyme)

getwd()##function to get your working directory 
write.csv(imputedmuscleenzyme,'imputedmusclenzyme.csv')

ppcamusclenz.pca<- prcomp(imputedmuscleenzyme, center = TRUE,scale. = TRUE)
summary(ppcamusclenz.pca)

print(ppcamusclenz.pca)

biplot(ppcamusclenz.pca)

plot(ppcamusclenz.pca)

ggbiplot(ppcamusclenz.pca)


gpop1 <- ggbiplot(ppcamusclenz.pca, obs.scale = 1, var.scale = 1, groups = MuscleenzymePCAdata[,2], ellipse = TRUE, circle = TRUE)
gpop1

ggbiplot(impmusclenz.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE, groups = MuscleenzymePCAdata[,2])












##SVDI method of imputing the data 
musclenzSVDI<-pca(MuscleenzymePCAdata, method = "svdImpute", center=FALSE, nPcs = 10)
imputedmuscleenz<-completeObs(musclenzSVDI)
print(imputedmuscleenz)



impmusclenz.pca<- prcomp(imputedmuscleenz, center = TRUE,scale. = TRUE)
summary(impmusclenz.pca)

print(impmusclenz.pca)

biplot(impmusclenz.pca)

plot(impmusclenz.pca)

ggbiplot(impmusclenz.pca)


gpop2 <- ggbiplot(impmusclenz.pca, obs.scale = 1, var.scale = 1, groups = MuscleenzymePCAdata[,2], ellipse = TRUE, circle = TRUE)
gpop2

ggbiplot(impmusclenz.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE, groups = MuscleenzymePCAdata[,2])


##imputedvals only positive values 
muscleenzymeabsvals.pca <- prcomp(imputedmusclenzymeabsvals, center = TRUE,scale. = TRUE)

summary(muscleenzymeabsvals.pca)

print(muscleenzymeabsvals.pca)

biplot(muscleenzymeabsvals.pca)

plot(muscleenzymeabsvals.pca)

ggbiplot(muscleenzymeabsvals.pca)


gpop3 <- ggbiplot(muscleenzymeabsvals.pca, obs.scale = 1, var.scale = 1, groups = MuscleenzymePCAdata[,2], ellipse = TRUE, circle = TRUE)
gpop3

ggbiplot(muscleenzymeabsvals.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE, groups = MuscleenzymePCAdata[,2])



##using the nipals function : this option is looking the most promising so far, no negative numbers introduced into the data set, and the magnitude of the imputed numbers looks good 
musclenznipals<-pca(MuscleenzymePCAdata, method = "nipals", center=FALSE, nPcs = 25)
imputednipalsmuscleenz<-completeObs(musclenznipals)
print(imputednipalsmuscleenz)


impnipalsmusclenz.pca<- prcomp(imputednipalsmuscleenz, center = TRUE,scale. = TRUE)
summary(impnipalsmusclenz.pca)

print(impnipalsmusclenz.pca)

biplot(impnipalsmusclenz.pca)

plot(impnipalsmusclenz.pca)

ggbiplot(impnipalsmusclenz.pca)


gpop4 <- ggbiplot(impnipalsmusclenz.pca, obs.scale = 1, var.scale = 1, groups = MuscleenzymePCAdata[,2], ellipse = TRUE, circle = TRUE)
gpop4

ggbiplot(impnipalsmusclenz.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE, groups = MuscleenzymePCAdata[,2])

PC12<-ggbiplot(impnipalsmusclenz.pca,ellipse=TRUE,choices=c(1,2), var.axes=FALSE, groups=MuscleenzymePCAdata[,2])
PC12
PC34<-ggbiplot(impnipalsmusclenz.pca,ellipse=TRUE,choices=c(3,4), var.axes=FALSE, groups=MuscleenzymePCAdata[,2])
PC34


##NLPCA method
##in the vignette for using inputation methods, this NLPCA method is what is suggested for use with enzyme activity data (the reasoning behind why I chose to use this method)
musclenzresNLPCA <- pca(MuscleenzymePCAdata, method="nlpca", center=FALSE, nPcs=25, maxSteps=300)
NLPCAmuscleenz<-completeObs(musclenzresNLPCA)
print(NLPCAmuscleenz)##the completed data set with the imputation that the nlpca function adds data to the missing data spots


NLPCAmusclenz.pca<- prcomp(NLPCAmuscleenz, center = TRUE,scale. = TRUE)
summary(NLPCAmusclenz.pca)

print(NLPCAmusclenz.pca)

biplot(NLPCAmusclenz.pca)

plot(NLPCAmusclenz.pca)

ggbiplot(NLPCAmusclenz.pca)

biplot(NLPCAmusclenz.pca, showLoadings=TRUE)
plotloadings(NLPCAmusclenz.pca, labSize = 3)



NLPCAmusclenz.pca$x
NLPCAmusclenz.pca$x[,1]
NLPCAmusclenz.pca$x[,2]
NLPCAmusclenz.pca$x[,3]


var <- get_pca_var(NLPCAmusclenz.pca)
var
head(var$contrib, 30)

library("corrplot")
corrplot(var$contrib, is.corr=FALSE)  

##the top 10 contributors explaining the variation the PC1 and PC2
library(factoextra)
# Contributions of variables to PC1
fviz_contrib(NLPCAmusclenz.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(NLPCAmusclenz.pca, choice = "var", axes = 2, top = 10)
#total contribution of variable to PC1 AND PC2
fviz_contrib(NLPCAmusclenz.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(NLPCAmusclenz.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


set.seed(123)
NLPCA.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(NLPCA.km$cluster)
# Color variables by groups
fviz_pca_var(NLPCAmusclenz.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

library(FactoMineR)

fviz_pca_ind(NLPCAmusclenz.pca)
fviz_pca_ind(NLPCAmusclenz.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(NLPCAmusclenz.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

install.packages("RColorBrewer")
library(RColorBrewer)
fviz_pca_ind(NLPCAmusclenz.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = MuscleenzymePCAdata$pop, # color by groups
             palette = "Dark2",
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)


# Add confidence ellipses
fviz_pca_ind(NLPCAmusclenz.pca, geom.ind = "point", col.ind = MuscleenzymePCAdata$pop, 
             palette = "Drak2",
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)




gpop5 <- ggbiplot(NLPCAmusclenz.pca, obs.scale = 1, var.scale = 1, groups = MuscleenzymePCAdata[,2], ellipse = TRUE, circle = TRUE)
gpop5

ggbiplot(NLPCAmusclenz.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE, groups = MuscleenzymePCAdata[,2])

PC125<-ggbiplot(NLPCAmusclenz.pca,ellipse=TRUE,choices=c(1,2), var.axes=FALSE, groups=MuscleenzymePCAdata[,2])
PC125
PC345<-ggbiplot(NLPCAmusclenz.pca,ellipse=TRUE,choices=c(3,4), var.axes=FALSE, groups=MuscleenzymePCAdata[,2])
PC345

##scree plot 
fviz_eig(NLPCAmusclenz.pca, addlabels = TRUE)


##get the eigan values 
get_eigenvalue(NLPCAmusclenz.pca)




get_pca_ind(NLPCAmusclenz.pca)##results for indivs
get_pca_var(NLPCAmusclenz.pca)##results for variables 

                  
                   