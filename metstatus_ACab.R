library(DMwR) #knnImputation
library(car) #qqPlot
library(e1071) #cmeans
library(mclust) #mclust
library(FactoMineR)
library(MASS) #lda/qda
library(Rtsne)# T-sne dimension reduction

codam <- read.csv('/home/acabbia/datasets/CODAM_dataset/Codam_dataset.csv')  
set.seed(666)
################################################################################################################
######### Pre-processing #######################################################################################
################################################################################################################
#missing values
codam = knnImputation(codam, k = 5)
codam = codam[-c(348,458,475),] #outliers

#subset met_status

ms_org = subset(codam, select = met_status)
table(ms_org$met_status)

#remove rows
codam = codam[-c(1,2)]
codam = subset(codam, select= -Glc)
codam = subset(codam, select= -met_status)

#make matrix and scale to ~N(0,1)
codam = data.matrix(codam)
codam = scale(codam)

#################################################################################################################
####### unsupervised learning: embedding (PCA, T-SNE) + clustering (k-means,fuzzy c-means) ##################
#################################################################################################################
#PCA
PCAdata = prcomp(codam)
PCAsdev = sum((PCAdata$sdev^2)>1)
data_sdev = PCAdata$x[,1:PCAsdev]

#T-SNE
Tsne=Rtsne(codam,theta = 0.0, dims = 6, perplexity = 30, verbose=TRUE, PCA=FALSE)

#k-means clustering
km_pca = kmeans(data_sdev,3,nstart = 20)
km_tsne = kmeans(Tsne$Y,3,nstart = 20)

# fuzzy c-means
cm_pca = cmeans(data_sdev,3, method = "cmeans")
cm_tsne = cmeans(Tsne$Y,3, method = "cmeans")

###############################################################################
########## results  #############################################################################################
#################################################################################################################

cm.km.pca=as.matrix(table(ms_org$met_status,km_pca$cluster))
cm.km.tsne=as.matrix(table(ms_org$met_status,km_tsne$cluster))
cm.cm.pca=as.matrix(table(ms_org$met_status,cm_pca$cluster))
cm.cm.tsne=as.matrix(table(ms_org$met_status,cm_tsne$cluster))

