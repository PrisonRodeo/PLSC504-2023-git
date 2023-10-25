#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries                                ####
#
# PLSC 504 -- Fall 2023
#
# Principal components analysis, factor analysis,
# and cluster analysis (whew, that's a lot...)
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("RCurl","readr","psych","randomNames","car","ggcorrplot",
     "cluster","pvclust","fpc","mclust","dendextend",
     "circlize","smacof","lme4","plm","gtools",
     "plyr","texreg","statmod","ltm","plotrix")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 12-15 times until you get "Package count = 21."
#
# NOTE: If you are doing this on a Mac / machine running MacOS,
# you need to install XQuartz (https://www.xquartz.org/) for all
# packages to load. (Having XQuartz on your machine is generally
# a good idea anyway.)
#
# setwd("~/Dropbox (Personal)/PLSC 504/Notes")  # <-- change as necessary...
#
# Options:

options(scipen = 24) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PCA starter / toy example                    ####

X <- data.frame(X1=c(0,1,2,3),X2=c(6,5,3,0),
                X3=c(7,9,10,13),X4=c(4,1,7,4))
X

CX <- sweep(X,2,colMeans(X),"-")  # "centered" X
CX

# Plot:

pdf("PCAToyPlot.pdf",6,5)
par(mar=c(3,3,3,3))
scatterplotMatrix(X,smooth=FALSE,pch=19,
                  diagonal=list(method="histogram"))
dev.off()

# Covariances and correlations:

Sigma <- cov(CX)
Sigma

R <- cor(CX)
R

# Eigenvalues and eigenvectors:

E <- eigen(Sigma)
E
L <- E$values
V <- E$vectors

sum(E$values)
tr(Sigma)

# PCScores <- Sigma*V
# PCScores

# SVD:

SVD <- svd(CX)
SVD
S <- SVD$d
U <- SVD$u
otherV <- SVD$v

# Eigenvalues:

(S^2)/(nrow(X)-1)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PCA intuition figure:

set.seed(7222009)
Y1 <- rnorm(400)
Y2 <- Y1 + rnorm(400)
fit<-lm(Y2~Y1)

pdf("PCA-Intuition.pdf",10,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(Y1,Y2,xlim=c(-4,4),ylim=c(-4,4),pch=20,
     xaxt="n",yaxt="n",xlab="X",ylab="Y")
abline(h=0,lwd=2)
abline(v=0,lwd=2)
plot(Y1,Y2,pch=20,xlim=c(-4,4),ylim=c(-4,4),
     xaxt="n",yaxt="n",xlab="",ylab="")
abline(fit,lwd=2)
abline(a=fit$coefficients[1], b=-(1/fit$coefficients[2]),
       lwd=2)
abline(h=0,lwd=1,lty=2)
abline(v=0,lwd=1,lty=2)
text(3.5,2.5,"PC1")
text(-3.3,2.5,"PC2")
text(3,-0.3,labels=expression(cos(theta) == L1))
dev.off()

# PCA:

princomp(CX) # via eigenvalues
prcomp(CX) # via SVD
otherV # from -svd-

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Larger PCA simulation...                     ####
#
# A small dataset with N=20 and K=10 variables. Six
# of the variables (Z1-Z6) are related to the latent
# Z; the other four (X1-X4) are related to the 
# latent X:

N <- 20
set.seed(7222009)
Name <- randomNames(N, which.names="first")
Z <- rnorm(N)
Z1 <- Z + 0.2*rnorm(N)
Z2 <- Z + 0.5*rnorm(N)
Z3 <- Z + 1*rnorm(N)
Z4 <- Z + 1.5*rnorm(N)
Z5 <- Z + 2*rnorm(N)
Z6 <- Z + 3*rnorm(N)

X <- rnorm(N)
X1 <- X + rnorm(N)
X2 <- X + rnorm(N)
X3 <- X + rt(N,5)
X4 <- X + rt(N,5)

df <- data.frame(Z1,Z2,Z3,Z4,Z5,Z6,X1,X2,X3,X4)
rownames(df)<-Name 

# head(df)
corrMat<-cor(df)
# corrMat
#
# Plot the correlations:

pdf("PCA-sim-corr-plot.pdf",6,5)
ggcorrplot(corrMat, hc.order="TRUE",
           lab="TRUE",type="lower",
           legend.title="Correlation")
dev.off()

# Flavors of PCA:

PCE <- princomp(df)
PCE

PCS <- prcomp(df,retx=FALSE)
PCS

# Using -principal-

PCSim1 <- principal(df, nfactors=1,rotate="none")
PCSim1

# minimalist plot:

pdf("PCA-loadings-plot.pdf",6,5)
plot(PCSim1,ylim=c(-1,1))
dev.off()

# Scores:

PCSim1$scores

# Two principal components:

PCSim2 <- principal(df, nfactors=2,rotate="none")
PCSim2

PCSim2$scores

# a little scatterplot:

pdf("PCA-scores-scatterplot.pdf",6,6)
par(mar=c(4,4,2,2))
plot(PCSim2$scores,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),
     xlab="PC One",ylab="PC Two",pch="")
text(PCSim2$scores, labels=rownames(df))
abline(h=0,lty=2)
abline(v=0,lty=2)
dev.off()

# Three principal components:

PCSim3 <- principal(df, nfactors=3,rotate="none")
PCSim3

# scatterplot matrix:

pdf("PCA-sim-scattermatrix.pdf",7,6)
scatterplotMatrix(PCSim3$scores,col="black",
                  pch=19,smooth=FALSE,regLine=FALSE,
                  var.labels=c("PC One","PC Two","PC Three"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Biplots                                      ####

foo<-prcomp(df)
foo$rotation[,1:2]

pdf("SimulatedBiplot.pdf",7,6)
par(mar=c(4,4,4,2))
biplot(foo,main=" ",cex=0.8)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Factor Analysis...                           ####
#
# Return to our simulated data...
#
# FA using simulated data:

FASim1 <- factanal(df,factors=1,scores="regression",
                   rotation="none")
print(FASim1,cutoff=0)

# Plot of factor loadings vs. correlations with "Z"

FA1Ls <- FASim1$loadings[1:10,1]
ZRs<-cor(cbind(Z,df))[2:11,1]

pdf("FAvsRealCorrs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(FA1Ls,ZRs,pch="",xlim=c(-0.6,1.1),
     ylim=c(-0.6,1.2),xlab="Factor Loadings",
     ylab="Correlations with Z")
text(FA1Ls,ZRs,labels=names(ZRs))
abline(a=0,b=1,lwd=2)
abline(h=0, lty=2)
dev.off()

# Two-factor model:

FASim2 <- factanal(df,factors=2,scores="regression",
                   rotation="none")
print(FASim2,cutoff=0)

# Three-factor model:

FASim3 <- factanal(df,factors=3,scores="regression",
                   rotation="none")
print(FASim3,cutoff=0)

# scatterplot matrix...

pdf("FA-loadings-scattermatrix.pdf",7,6)
scatterplotMatrix(FASim3$loadings[1:10,1:3],col="black",
                  pch=19,smooth=FALSE,regLine=FALSE,
                  var.labels=c("Factor One","Factor Two","Factor Three"))
dev.off()

cor(FASim3$loadings[1:10,1:3])

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# FA Remix: ANES 2016 feeling thermometers     ####
#
# Data stuff...

ANES <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/ANES2016.csv")

Tvars <- c("V162310","V162311","V162312","V162313",
           "V162314","V162078","V162079","V162080",
           "V162081","V162091","V162092","V162093",
           "V162094","V162095","V162096","V162097",
           "V162098","V162099","V162100","V162101",
           "V162102","V162103","V162104","V162105",
           "V162106","V162107","V162108","V162109",
           "V162110","V162111","V162112","V162113")

Therms <- ANES[Tvars]
Therms[Therms==-5] <- NA
Therms[Therms==-6] <- NA
Therms[Therms==-7] <- NA
Therms[Therms==-9] <- NA
Therms[Therms==998] <- NA
Therms[Therms==999] <- NA
Therms <- na.omit(Therms)
colnames(Therms) <- c("Asian-Americans","Hispanics","Blacks",
                      "Illegal Immigrants","Whites","Dem. Pres. Candidate",
                      "GOP Pres. Candidate","Libertarian Pres. Candidate",
                      "Green Pres. Candidate","Dem. VP", "GOP VP",
                      "John Roberts", "Pope Francis",
                      "Christian Fundamentalists","Feminists","Liberals",
                      "Labor Unions","Poor People","Big Business",
                      "Conservatives","SCOTUS","Gays & Lesbians",
                      "Congress","Rich People","Muslims","Christians",
                      "Jews","Tea Party","Police","Transgender People",
                      "Scientists","BLM")

describe(Therms,range=FALSE)

# Factor Analysis, one factor:

FTFA1 <- fa(Therms,nfactors=1,fm="ml",rotate="none")
print(FTFA1)

# Plot:
FA1Ls<-as.numeric(print(FTFA1$loadings,cutoff=0))

pdf("FATherm1.pdf",7,4.5)
plot(FA1Ls,rep(0,times=length(FA1Ls)),pch=19,yaxt="n",
     xlim=c(-0.82,0.82),ylim=c(-0.1,1),xlab="Loadings",ylab="")
text(FA1Ls,0.03,labels=colnames(Therms),pos=4,srt=90,offset=0.1)
dev.off()


# Two factors:

FTFA2 <- fa(Therms,nfactors=2,fm="ml",
            rotate="none")
print(FTFA2)

pdf("FATherm2.pdf",6,5)
par(mar=c(4,4,2,2))
plot(FTFA2,labels=colnames(Therms),cex=0.6,
     xlim=c(-1.5,1.5),ylim=c(-0.1,0.6))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Rotation sim example...

RoNone <- fa(df,nfactors=2,rotate="none",fm="ml")
RoVM <- fa(df,nfactors=2,rotate="varimax",fm="ml")
RoQM <- fa(df,nfactors=2,rotate="quartimax",fm="ml")
RoOb <- fa(df,nfactors=2,rotate="oblimin",fm="ml")

pdf("RotationSims.pdf",11,8.5)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
plot(RoNone,labels=colnames(df),title="None")
plot(RoVM,labels=colnames(df),title="Varimax")
plot(RoQM,labels=colnames(df),title="Quartimax")
plot(RoOb,labels=colnames(df),title="Oblimin")
dev.off()

# Rotation: FTs:

FTNone <- fa(Therms,nfactors=2,rotate="none",fm="ml")
FTVM <- fa(Therms,nfactors=2,rotate="varimax",fm="ml")
FTQM <- fa(Therms,nfactors=2,rotate="quartimax",fm="ml")
FTOb <- fa(Therms,nfactors=2,rotate="oblimin",fm="ml")

pdf("RotationFTs.pdf",11,8.5)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
plot(FTNone,labels=colnames(Therms),title="None")
plot(FTVM,labels=colnames(Therms),title="Varimax")
plot(FTQM,labels=colnames(Therms),title="Quartimax")
plot(FTOb,labels=colnames(Therms),title="Oblimin")
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dimensionality
#
# Scree plot w/"parallel" plots, simulated data:

FAP <- fa.parallel(df)

pdf("SimulatedScreePlot.pdf",7,6)
FAP <- fa.parallel(df)
dev.off()

# Same, with FT data:

pdf("FTScreePlot.pdf",6,6)
FAPTherm <- fa.parallel(Therms)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SCOTUS votes example (binary data)           ####
#
# Data:

SCOTUS <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/SCOTUS-IRT.csv")

SCOTUS <- na.omit(SCOTUS)
head(SCOTUS)

# Correlations:

SCOTUSR <- cor(SCOTUS[,2:10])
SCOTUSRs <- as.vector(SCOTUSR)
SCOTUST <- tetrachoric(SCOTUS[,2:10])
SCOTUSTs <- as.vector(SCOTUST$rho)

pdf("SCOTUSPearsonVsTet.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUSRs,SCOTUSTs,xlim=c(0,1),ylim=c(0,1),pch=19,
     xlab="Pearson Correlations",
     ylab="Tetrachoric Correlations")
abline(a=0,b=1,lwd=2)
dev.off()

# Scree / parallel plot, using tetrachoric correlation:

SCOTUSScree <- fa.parallel(SCOTUS[,2:10],cor="tet")

pdf("SCOTUSScreePlot.pdf",6,6)
fa.parallel(SCOTUS[,2:10],cor="tet")
dev.off()

# FA on tetrachoric voting correlations:

SCOTUSFA <- fa(SCOTUS[,2:10],nfactors=2,rotate="varimax",
               fm="ml",cor="tet")
SCOTUSFA

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# CLUSTER ANALYSIS                             ####
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Silly Tick example:

Tick<-c(1,711,0.08)
Arthur<-c(0,588,0.27)
L2<-dist(rbind(Tick,Arthur))
L1<-dist(rbind(Tick,Arthur),method="manhattan")
# Mahalanobis by hand:
Diff<-Arthur-Tick
S <- cov(rbind(Tick,Arthur))
LM <- sqrt(t(Diff)%*%solve(S,tol=1e-30)%*%Diff)
# Check:
sqrt(mahalanobis(Arthur,center=Tick,cov=S,tol=1e-30))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simulation Data                              ####

N <- 20
set.seed(7222009)
Name <- randomNames(N, which.names="first")
X <- 5*rbeta(N,0.5,0.5)
Y <- runif(N,-4,4)
Z <- rbinom(N,1,pnorm(Y/2))

df <- data.frame(Name=Name,X=X,Y=Y,Z=Z)
rownames(df)<-df$Name

pdf("ClusterSimPlotOne.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=Z+1,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("black","red"),
       legend=c("Z=0","Z=1"))
dev.off()

pdf("ClusterSimPlotTwo.pdf",3,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=Z+1,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("black","red"),
       legend=c("Z=0","Z=1"))
dev.off()

pdf("ClusterSimPlotThree.pdf",6,3)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=NA,xlim=c(-1,6),ylim=c(-4,4)))
with(df, text(X,Y,labels=Name,col=Z+1,
              cex=0.8))
legend("topright",bty="n",pch=c(20,20),col=c("black","red"),
       legend=c("Z=0","Z=1"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Distances:
#
# CENTER AND RESCALE / STANDARDIZE THE DATA:

ds <- scale(df[,2:4])

DL2 <- dist(ds) # L2 / Euclidean distance
DL1 <- dist(ds,method="manhattan") # L1 / Manhattan distance
DM <- sqrt(D2.dist(ds,cov(ds))) # Mahalanobis distances

# Scatterplot matrix of distances:

pdf("ClusterDistanceComparisons.pdf",6,5)
scatterplotMatrix(~DL2+DL1+DM,pch=20,
                  var.labels=c("Euclidean","Manhattan","Mahalanobis"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Agglomerative clustering (Euclidean distance)####

ADL2.s <- hclust(DL2,method="single")
ADL2.c <- hclust(DL2,method="complete")
ADL2.a <- hclust(DL2,method="average")

str(ADL2.s)

pdf("ClusterSimDendrogram.pdf",4,6)
par(mar=c(4,4,4,2))
plot(ADL2.s,main="Single Linkage",xlab=" ",cex=0.8)
dev.off()

pdf("Cluster-HCLUST-Sims.pdf",8,6)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
plot(ADL2.s,main="Single",xlab=" ",cex=0.8)
plot(ADL2.c,main="Complete",xlab=" ",cex=0.8)
plot(ADL2.a,main="Average",xlab=" ",cex=0.8)
dev.off()

# Same thing, with Mahalanobis distance...

ADM.s <- hclust(DM,method="single")
ADM.c <- hclust(DM,method="complete")
ADM.a <- hclust(DM,method="average")

pdf("Cluster-HCLUST-Mahal.pdf",8,6)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
plot(ADM.s,main="Single",xlab=" ",cex=0.8)
plot(ADM.c,main="Complete",xlab=" ",cex=0.8)
plot(ADM.a,main="Average",xlab=" ",cex=0.8)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ward's Method:
# 
# ADL2.w <- hclust(DL2,method="ward.D2")
# 
# pdf("ClusterSimWardDendrogram.pdf",6,4)
# par(mar=c(4,4,4,2))
# plot(ADL2.w,main="Ward's Method",xlab=" ",cex=0.8)
# dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Coefficient of Agglomeration

set.seed(7222009)
AC1 <- data.frame(A1=append(rnorm(10,10,1),rnorm(10,0.1)),
                  A2=append(rnorm(10,0,1),rnorm(10,10,1)))
AC2 <- data.frame(A1=runif(20,0,10),
                  A2=runif(20,0,10))

CAC1<-agnes(AC1,metric="euclidean",method="average")
CAC2<-agnes(AC2,metric="euclidean",method="average")

pdf("ClusterACExamplePlot.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(AC2,pch=20,xlab="X",ylab="Y",xlim=c(0,10),ylim=c(0,10))
legend("topleft",bty="n",legend=paste("AC = ",round(CAC2$ac,3)))
plot(AC1,pch=20,xlab="X",ylab="Y")
legend("topright",bty="n",legend=paste("AC = ",round(CAC1$ac,3)))
dev.off()

# Sim data:

Agnes.s <- agnes(ds, metric="euclidean",method="single")
Agnes.s$ac
Agnes.c <- agnes(ds, metric="euclidean",method="complete")
Agnes.c$ac
Agnes.a <- agnes(ds, metric="euclidean",method="average")
Agnes.a$ac
# Using Mahalanobis distance:
Agnes.M <- agnes(DM, diss=TRUE, method="average")
Agnes.M$ac

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# P-Values:

dst<-data.frame(t(ds))
PVDL2.s <- pvclust(dst,method.hclust="average",
                   method.dist="euclidean",nboot=1001)
PVDL2.s

pdf("ClusterPValueDendrogram.pdf",6,6)
par(mar=c(4,4,4,2))
plot(PVDL2.s,main="Euclidean/Single Linkage",xlab=" ",
     sub=" ",cex=0.8)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Divisive clustering                          ####

Diana.L2 <- diana(ds,metric="euclidean")
Diana.L2

Diana.L1 <- diana(ds,metric="manhattan")

pdf("DivisiveClusteringDendrograms.pdf",8,5)
par(mar=c(4,4,4,2))
par(mfrow=c(1,2))
plot(Diana.L2,which.plots=2,main="Euclidean Distance",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",cex=0.8,
       legend=paste("DC = ",round(Diana.L2$dc,3)))
plot(Diana.L1,which.plots=2,main="Manhattan Distance",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",cex=0.8,
       legend=paste("DC = ",round(Diana.L1$dc,3)))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# k-Means Clustering                           ####

KM2 <- kmeans(ds,2)
KM2

# Plot:

pdf("KMeansPlot2.pdf",6,5)
par(mar=c(4,4,4,2))
clusplot(ds,KM2$cluster,color=TRUE,shade=TRUE,
         labels=2,lines=0,main="K = 2",xlab="First PC",
         ylab="Second PC")
dev.off()

KM3 <- kmeans(ds,3)
KM3

pdf("KMeansPlot3.pdf",6,5)
par(mar=c(4,4,4,2))
clusplot(ds,KM3$cluster,color=TRUE,shade=TRUE,
         labels=2,lines=0,main="K = 3",xlab="First PC",
         ylab="Second PC")
dev.off()

# Alternative:

PAM3 <- pam(ds,3)
PAM3

pdf("PAM3Cluster.pdf",6,5)
plot(PAM3,which.plots=1,main="PAM Cluster Plot (k=3)")
dev.off()


# Scree plot to determine number of clusters
wss <- (nrow(ds)-1)*sum(apply(ds,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(ds, 
                                     centers=i)$withinss)

pdf("ClusterKMeansScree.pdf",6,5)
par(mar=c(4,4,2,2))
plot(1:15, wss, t="o", xlab="Number of Clusters",pch=20,
     ylab="Within-Groups Sum of Squares",lwd=2)
dev.off()

# Model-based cluster choosing:

MCC <- Mclust(ds)
summary(MCC)

pdf("ModelBasedClusterPlot.pdf",6,5)
plot(MCC)
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# States 2005 data examples...                 ####

States <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/States2005.csv")

summary(States)

StS <- data.frame(scale(States[,3:10]))
rownames(StS)<-States$statename

# Agglomerative Clustering:

StSL2 <- dist(StS) # L2 / Euclidean distance

StS.agg <- agnes(StS,metric="euclidean",method="average")

pdf("StatesAggDendrogram.pdf",8,6)
par(mar=c(4,4,4,2))
plot(StS.agg,which.plots=2,main="Euclidean Distance / Average Linkage",
     xlab=" ",cex=0.8)
legend("topleft",bty="n",legend=paste("AC = ",round(StS.agg$ac,3)))
dev.off()

# Circular dendrogram using -dendextend-:

StS.dend <- hang.dendrogram(as.dendrogram(StS.agg))
StS.dend <- rotate(StS.dend, 1:50)
StS.dend <- color_branches(StS.dend, k=6)

pdf("StatesCircDendrogram.pdf",8,8)
par(mar=c(2,2,2,2))
StS.circ <- circlize_dendrogram(StS.dend,
                                labels_track_height=0.4,
                                dend_track_height=0.4)
dev.off()

# Kmeans: How many clusters?

wss <- (nrow(StS)-1)*sum(apply(StS,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(StS,centers=i)$withinss)

pdf("StatesKMeansScree.pdf",6,5)
par(mar=c(4,4,2,2))
plot(1:15, wss, t="o", xlab="Number of Clusters",pch=20,
     ylab="Within-Groups Sum of Squares",lwd=2)
dev.off()

# K-means plot, K=5:

StSKM5 <- kmeans(StS,5)

pdf("StateKMeans5.pdf",7,6)
par(mar=c(4,4,2,2))
clusplot(StS,StSKM5$cluster,color=TRUE,shade=TRUE,
         labels=3,lines=0,main=" ",xlab="First PC",
         ylab="Second PC",xlim=c(-7,4))
dev.off()

# /fin