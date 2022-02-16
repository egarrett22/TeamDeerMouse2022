library(devtools)
library(ggbiplot)
library(pcaMethods)
library(Biobase)
library(factoextra)
library(GEOquery)
library(mixOmics)

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
##get the individual % contribution for each variable 
contribs<-var$contrib

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
##ellipse level 0.95
fviz_pca_ind(NLPCAmusclenz.pca, geom.ind = "point", col.ind = MuscleenzymePCAdata$pop, 
             palette = "Drak2",
             addEllipses = TRUE,ellipse.type = "confidence" , ellipse.level=0.95, 
             legend.title ="Group"
)

##convex ellipse
fviz_pca_ind(NLPCAmusclenz.pca, geom.ind = "point", col.ind = MuscleenzymePCAdata$pop, 
             palette = "Drak2",
             addEllipses = TRUE,ellipse.type = "convex" , ellipse.level=0.95, 
             legend.title ="Group"
)
##normal distribution
fviz_pca_ind(NLPCAmusclenz.pca, geom.ind = "point", col.ind = MuscleenzymePCAdata$pop, 
             palette = "Drak2",
             addEllipses = TRUE,ellipse.type = "norm" , ellipse.level=0.95, 
             legend.title ="Group"
)

gpop5 <- ggbiplot(NLPCAmusclenz.pca, obs.scale = 1, var.scale = 1, groups = MuscleenzymePCAdata[,2], ellipse = TRUE, circle = TRUE)
gpop5

ggbiplot(NLPCAmusclenz.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE, groups = MuscleenzymePCAdata[,2])

PC125<-ggbiplot(NLPCAmusclenz.pca,ellipse=TRUE,choices=c(1,2), var.axes=FALSE, groups=MuscleenzymePCAdata[,2])
PC125
PC345<-ggbiplot(NLPCAmusclenz.pca,ellipse=TRUE,choices=c(3,4), var.axes=FALSE, groups=MuscleenzymePCAdata[,2])
PC345
