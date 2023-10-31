#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro                                          ####
#
# PLSC 504 -- Fall 2023
#
# Scaling + Item Response Theory (IRT)...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","psych","smacof","car","lme4","plm",
     "gtools","plyr","texreg","statmod","ltm",
     "plotrix")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 4-5 times until you get "Package count = 12"
#
# setwd("~/Dropbox (Personal)/PLSC 504/Notes")  # <-- change as necessary...

options(scipen = 12) # bias against scientific notation
options(digits = 4) # show fewer decimal places
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# UDS: Cities data                               ####

Cities <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/USCities.csv")
Cities <- Cities[1:10,]
rownames(Cities) <- Cities$city

# Create Euclidean distance matrix, 1D (longitude):

CityLong <- data.frame(t(Cities$longitude)) # longitudes in a row
colnames(CityLong) <- t(Cities$City) # names
D1long <- dist(t(CityLong)) # distance object

# UDS using uniscale in package(smacof):

UDS <- uniscale(D1long)
UDS
UDS$conf

# Plot:

pdf("UDS-Cities.pdf",6,4)
par(mar=c(4,4,4,4))
plot(UDS, 
     main="East-West Locations for the Ten Largest 
     U.S. Cities via UDS")
dev.off()

# # "Distance" - latitude + longitude
# 
# print(Cities[,c(1,3:4)])
# 
# D1LL <- dist(Cities[,3:4])
# D1LL
# 
# # UDS solution for "distance":
# 
# UDS2 <- uniscale(D1LL)
# UDS2

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SCOTUS votes:

SCOTUS <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/SCOTUS-IRT.csv")

SCOTUS <- na.omit(SCOTUS)
head(SCOTUS)

# Sum Scores:

SumScores <- colSums(SCOTUS[,2:10],na.rm=TRUE) / nrow(SCOTUS)
SumScores

D1SCOTUS <- dist(t(SCOTUS[,2:10]))
D1SCOTUS

SCOTUS.UDS <- uniscale(D1SCOTUS)
SCOTUS.UDS

# Plot:

pdf("UDS-SCOTUS.pdf",6,6)
par(mar=c(2,2,4,2))
par(mfrow=c(2,1))
plot(SumScores,rep(0,times=9),type="p",xlab="",
     ylab="",axes=FALSE,main="Sum Scores",
     xlim=c(min(SumScores),max(SumScores)),pch=19)
text(SumScores,rep(0,times=9),labels=names(SumScores),
     srt=90,adj=c(-0.2,NA))
segments(0,0,1,0)
plot(SCOTUS.UDS,main="UDS Results")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Kronbach's Alpha                               ####
#
# A "real" data example: SCOTUS votes...

SCOTUSAlpha <- alpha(SCOTUS[,2:10],check.keys=TRUE)
SCOTUSAlpha


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MDS: ANES 2016 feeling thermometers            ####
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

# Summary:

pdf("ThermsBoxplot.pdf",6,5)
par(mar=c(11,4,1,2))
boxplot(Therms,notch=TRUE,las=2,pch=20,cex.axis=0.8)
dev.off()


# Distance thingy:

ThermDist <- dist(t(Therms))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2-D MDS...

MDS2.alt <- cmdscale(ThermDist,k=2)
head(MDS2.alt)

pdf("AltMDS2-Thermometers.pdf",6,5)
par(mar=c(4,4,2,2))
plot(MDS2.alt[,1], MDS2.alt[,2],type="n",xlab="First Dimension",
     ylab="Second Dimension",main=" ",xlim=c(-2500,2000))
text(MDS2.alt[,1],MDS2.alt[,2],labels(ThermDist),cex=0.9,
     xpd=TRUE)
dev.off()

# Alternative, using -mds-:

MDS2 <- mds(ThermDist, ndim=2)
MDS2

pdf("MDS2-Thermometers.pdf",6,5)
par(mar=c(4,4,2,2))
plot(MDS2, xlim=c(-1.5,1.5),cex=0.8,
     ylim=c(-1,1),main=" ")
dev.off()

# Compare the two:

pdf("MDS-Compare.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(MDS2.alt[,1],MDS2$conf[,1],pch=".",xlab="-cmdscale-",
     ylab="-mds-",main="First Dimension",xlim=c(-2500,2000))
text(MDS2.alt[,1],MDS2$conf[,1],cex=0.4,labels(ThermDist))
plot(MDS2.alt[,2],MDS2$conf[,2],pch=".",xlab="-cmdscale-",
     ylab="-mds-",main="Second Dimension",xlim=c(-1500,1500))
text(MDS2.alt[,2],MDS2$conf[,2],cex=0.4,labels(ThermDist))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Variations on MDS...
#
# "Interval":

MDS2.int <- mds(ThermDist, ndim=2, type="interval")
MDS2.int

# Ordinal:

MDS2.ord <- mds(ThermDist, ndim=2, type="ordinal")
MDS2.ord

# Compare:

FirstDs<-data.frame(Ratio=MDS2.alt$conf[,1],
                    Interval=MDS2.int$conf[,1],
                    Ordinal=MDS2.ord$conf[,1])

SecondDs<-data.frame(Ratio=MDS2.alt$conf[,2],
                     Interval=MDS2.int$conf[,2],
                     Ordinal=MDS2.ord$conf[,2])

pdf("MDS-Therms-compareD1.pdf",4,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(FirstDs,pch=20,main="First Dimension")
dev.off()

pdf("MDS-Therms-compareD2.pdf",4,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(SecondDs,pch=20,main="Second Dimension")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Same thing, using SCOTUS voting data...

SCR <- mds(D1SCOTUS, ndim=2, type="ratio")
SCR
SCI <- mds(D1SCOTUS, ndim=2, type="interval")
SCO <- mds(D1SCOTUS, ndim=2, type="ordinal")

# Example plot:

pdf("MDS2R-SCOTUS-plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCR, xlim=c(-1.5,1.5),cex=0.8,
     ylim=c(-1,1),main=" ")
dev.off()

SC1<-data.frame(Ratio=SCR$conf[,1],
                    Interval=SCI$conf[,1],
                    Ordinal=SCO$conf[,1])
SC2<-data.frame(Ratio=SCR$conf[,2],
                Interval=SCI$conf[,2],
                Ordinal=SCO$conf[,2])

JNames <- names(SCR$spp) # labels

pdf("MDS-SCOTUS-compareD1.pdf",4,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(SC1,pch=20,main="First Dimension")
dev.off()

pdf("MDS-SCOTUS-compareD2.pdf",4,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(SC2,pch=20,main="Second Dimension")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Item Response Theory...                        ####
#
# SCOTUS voting data:

SCOTUS <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/SCOTUS-IRT.csv")

head(SCOTUS,10)
summary(SCOTUS)

# 1PLM:

OnePLM<-rasch(SCOTUS[c(2:10)])
summary(OnePLM)

coef(OnePLM, prob=TRUE, order=TRUE)

# Alternative model constraining alpha = 1.0:

IRTData <- SCOTUS[c(2:10)]

AltOnePLM<-rasch(IRTData, constraint=cbind(length(IRTData)+1,1))
summary(AltOnePLM)

# Compare:

pdf("1PLM-coefs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OnePLM$coefficients[,1],AltOnePLM$coefficients[,1],
     xlim=c(-3,3),ylim=c(-1.75,1.25),pch="",
     xlab="Unconstrained",
     ylab=expression(paste("Constrained (",alpha,"=1)")))
text(OnePLM$coefficients[,1],AltOnePLM$coefficients[,1],
     labels=colnames(IRTData),cex=0.8)
abline(h=0,lwd=1,lty=2)
abline(v=0,lwd=1,lty=2)
dev.off()


# 2PLM:

TwoPLM<-ltm(IRTData ~ z1)
summary(TwoPLM)

# 2PLM Probabilities and testing:

coef(TwoPLM, prob=TRUE, order=TRUE)
anova(OnePLM, TwoPLM)

# 3PLM:

ThreePLM<-tpm(IRTData)
summary(ThreePLM)

anova(TwoPLM, ThreePLM)

# Plots:

pdf("1PLMIRFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OnePLM,lty=c(1,2,3,4,5,6,7,8,9), lwd=3, 
     zrange=c(-2.5,2.5),xlab="Liberalism",
     legend=TRUE,main="1PLM ICCs")
dev.off()

pdf("2PLMIRFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(TwoPLM,lty=c(1,2,3,4,5,6,7,8,9), lwd=3, 
     zrange=c(-2.5,2.5),xlab="Liberalism",
     legend=TRUE,main="2PLM ICCs")
dev.off()

pdf("3PLMIRFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(ThreePLM,lty=c(1,2,3,4,5,6,7,8,9), lwd=3, 
     zrange=c(-2.5,2.5),xlab="Liberalism",
     legend=TRUE,main="3PLM ICCs")
dev.off()

# Ladderplot (2PLM example):

foo <- data.frame(P=(summary(TwoPLM)$coefficients[1:9,1]),
                  UB=(summary(TwoPLM)$coefficients[1:9,1] +
                              1.96*summary(TwoPLM)$coefficients[1:9,2]),
                  LB=(summary(TwoPLM)$coefficients[1:9,1] -
                              1.96*summary(TwoPLM)$coefficients[1:9,2]))
rownames(foo)<-rownames(TwoPLM$coefficients)
foo<-foo[order(foo$P),]

pdf("Ladder-2PLM.pdf",6,5)
par(mar=c(4,8,2,2))
dotchart(foo$P,pch=19,xlim=c(min(foo$LB)-0.1,max(foo$UB)+0.1),
         labels=rownames(foo),xlab="Position")
segments(foo$LB,c(1:9),foo$UB,c(1:9))
dev.off()

# Ladderplot (3PLM):

foo2 <- data.frame(P=(summary(ThreePLM)$coefficients[10:18,1]),
                   UB=(summary(ThreePLM)$coefficients[10:18,1] +
                               1.96*summary(ThreePLM)$coefficients[10:18,2]),
                   LB=(summary(ThreePLM)$coefficients[10:18,1] -
                               1.96*summary(ThreePLM)$coefficients[10:18,2]))
rownames(foo2)<-rownames(ThreePLM$coefficients)
foo2<-foo2[order(foo2$P),]

pdf("Ladder-3PLM.pdf",6,5)
par(mar=c(4,8,2,2))
dotchart(foo2$P,pch=19,xlim=c(min(foo2$LB)-0.1,max(foo2$UB)+0.1),
         labels=rownames(foo2),xlab="Position")
segments(foo2$LB,c(1:9),foo2$UB,c(1:9))
dev.off()

# /fin