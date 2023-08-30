#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# INTRODUCTION                                  ####
#
# Code for PLSC 504 - Fall 2023 
#
# Binary- and Nominal-Response Regression 
# Models: Extensions (mostly)
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                               ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("devtools","readr","RCurl","elrm","logistf","VGAM","mlogit",
     "aod","stargazer","MNP","ggcorrplot","zeligverse")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 7-8 times until you get all smileys. :)
#
# Also: "Zelig" / "zeligverse" are notoriously unreliable to load, etc.
# If "zeligverse" doesn't load, uncomment and try this:
#
# devtools::install_version("Zelig", version="5.1.6.1",
#                          repos="http://cran.us.r-project.org")
#
# OR this...
#
# devtools::install_github('IQSS/zeligverse')
#
# OR this...
#
# devtools::install_github('IQSS/Zelig') # <- most likely to work...
#
# Then:
#
# library(Zelig) # Note: Zelig can be weird to load, sometimes...
#
# Set working directory:
#
# setwd("~/Dropbox (Personal)/PLSC 504") # <- change as needed...
#                                             or use a project, whatever
#
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Binary Responses: Separation...                   ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Table

Yeas<-t(c(rep(0,times=212),rep(1,times=219)))
Dems<-t(c(rep(0,times=178),rep(1,times=253)))
table(Yeas,Dems)

# Simulated Logits:

set.seed(7222009)
X<-runif(100,min=-5,max=5)
X<-X[order(X)]
Z<-runif(100,min=-5,max=5)
Y<-ifelse(plogis(X+Z)>0.5,1,0)
Y2<-ifelse(plogis(X+0.5*Z)>0.5,1,0)
Y3<-ifelse(plogis(X+0.1*Z)>0.5,1,0)
Ysep<-ifelse(plogis(X)>0.5,1,0)
Yfit<-glm(Y~X,family="binomial")
Y2fit<-glm(Y2~X,family="binomial")
Y3fit<-glm(Y3~X,family="binomial")
Ysepfit<-glm(Ysep~X,family="binomial")

# Plots:

pdf("Separation.pdf",8,7)
par(mar=c(4,4,2,2))
par(mfrow=c(2,2))
plot(X,Y,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Yfit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Yfit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Yfit))[4],digits=2))))
plot(X,Y2,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Y2fit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Y2fit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Y2fit))[4],digits=2))))
plot(X,Y3,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Y3fit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Y3fit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Y3fit))[4],digits=2))))
plot(X,Ysep,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Ysepfit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Ysepfit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Ysepfit))[4],digits=2))))
dev.off()

# Toy data:

rm(X,Y,Z)
set.seed(7222009)
Z<-rnorm(500)
W<-rnorm(500)
Y<-rbinom(500,size=1,prob=plogis((0.2+0.5*W-0.5*Z)))
X<-rbinom(500,1,(pnorm(Z)))
X<-ifelse(Y==0,0,X)

summary(glm(Y~W+Z+X,family="binomial"))
summary(glm(Y~W+Z+X,family="binomial",maxit=100,epsilon=1e-16))

# data<-as.data.frame(cbind(W,X,Y,Z))
# write.dta(data,"SepSim.dta") # for the old Stata example

# Exact logistic regression...
# DO NOT ACTUALLY RUN THIS CODE -- your computer will likely
# freeze up. It's here for example purposes only.
#
# df <- data.frame(one=1,Y=Y,W=W,Z=Z,X=X)
# toy.elrm <- elrm(Y/one~W+Z+X,interest=~X,dataset=df,
#                  r=4,iter=5000,burnIn=1000)
# summary(toy.elrm)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Pets-as-family data example:

Pets<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/Pets.csv")

Pets.1<-glm(petfamily~female+as.factor(married)+as.factor(partyid)
            +as.factor(education),data=Pets,family=binomial)
summary(Pets.1)

Pets.2<-glm(petfamily~female+as.factor(married)*female+as.factor(partyid)+
              as.factor(education),data=Pets,family=binomial)
summary(Pets.2)

with(Pets, xtabs(~petfamily+as.factor(married)+female))

Pets.Firth<-logistf(petfamily~female+
                      as.factor(married)*female+as.factor(partyid)+
                      as.factor(education),data=Pets)
Pets.Firth

# Profile Firth profile likelihood:

Pets.profile<-profile(Pets.Firth,
     variable="femaleMale:as.factor(married)Widowed",
     firth=TRUE)

# Plot it:

pdf("Notes/PetsProfileL.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Pets.profile)
abline(v=Pets.Firth$coefficients[15],lty=2,lwd=2)
abline(v=0,lty=3,lwd=1)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Binary Responses: Rare Events                 ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Rare Event bias figure:

N<-c(.1,5,seq(10,10000,by=10))
p4 <- (0.4-0.5) / (N*(0.4)*(1-0.4))
p1 <- (0.1-0.5) / (N*(0.1)*(1-0.1))
p01 <- (0.01-0.5) / (N*(0.01)*(1-0.01))
p001 <- (0.001-0.5) / (N*(0.001)*(1-0.001))

pdf("Notes/RareEventBiasR.pdf",7,6)
par(mar=c(4,4,2,2))
plot(N,p4,t="l",lwd=2,lty=1,col="black",xlab="N",
      ylab="Bias in the Intercept",ylim=c(-0.5,0))
lines(N,p1,t="l",lwd=2,lty=2,col="green")
lines(N,p01,t="l",lwd=2,lty=3,col="yellow")
lines(N,p001,t="l",lwd=2,lty=4,col="red")
legend("bottomright",bty="n",
        legend=c(expression(paste(bar(pi),"=0.4")),
                 expression(paste(bar(pi),"=0.1")),
                 expression(paste(bar(pi),"=0.01")),
                 expression(paste(bar(pi),"=0.001"))),
       col=c("black","green","yellow","red"),
       lty=c(1,2,3,4),lwd=2)
dev.off()

# TAPS data:

TAPS<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/main/Data/SomeTAPSData.csv")
TAPS<-subset(TAPS,select=-(BeesOrBear)) # Zap
TAPS<-TAPS[complete.cases(TAPS),] # delete cases w/NAs
TAPS$Age10<-TAPS$Age/10 # rescale Age variable
rm(temp)

table(TAPS$RunOutOfGas)
prop.table(table(TAPS$RunOutOfGas))

# Basic logit:

ROGlogit<-glm(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                     Democrat+GOP+Ideology,data=TAPS,family=binomial)
summary(ROGlogit)

# (Fake) Case-control sampling:

set.seed(7222009)
ROGones<-TAPS[TAPS$RunOutOfGas==1,]
ROGzeros<-TAPS[TAPS$RunOutOfGas==0,]
ROGSzeros<-ROGzeros[sample(1:nrow(ROGzeros),100,replace=FALSE),]
ROGsample<-data.frame(rbind(ROGones,ROGSzeros))
table(ROGsample$RunOutOfGas)

sample.logit<-glm(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                        Democrat+GOP+Ideology,data=ROGsample,family=binomial)
summary(sample.logit)

# RE logit, prior correction (using Zelig)...

relogit.pc<-zelig(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                  Democrat+GOP+Ideology,data=ROGsample,model="relogit",
                  tau=28/971,case.control=c("prior"))
summary(relogit.pc)

# RE logit, weighting correction...

relogit.wc<-zelig(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                  Democrat+GOP+Ideology,data=ROGsample,model="relogit",
                  tau=28/971,case.control=c("weighting"))
summary(relogit.wc)

# Firth logit, for comparison:

relogit.firth<-logistf(RunOutOfGas~Education+Age10+Female+White+Black+Asian+
                    Democrat+GOP+Ideology,data=ROGsample)
summary(relogit.firth)


# Combine and plot coefficients:

BHats<-data.frame(Logit=coef(ROGlogit))
BHats$CCSampled<-coef(sample.logit)
BHats$RE.PC<-coef(relogit.pc)
BHats$RE.WC<-coef(relogit.wc)
BHats$RE.Firth<-coef(relogit.firth)
BHT<-data.frame(t(BHats))
BHT$Intercept<-BHT$X.Intercept.
BHT$X.Intercept.<-NULL

pdf("Notes/REStripchart.pdf",8,6)
par(mar=c(4,7,2,2))
stripchart(BHT[1,],group.names=colnames(BHT),yaxt="n",
           pch=15,xlim=c(-2,2),
           col="black")
points(BHT[2,],1:10,col="blue",pch=16)
points(BHT[3,],1:10,col="orange",pch=17)
points(BHT[4,],1:10,col="darkgreen",pch=18)
points(BHT[5,],1:10,col="red",pch=20)
abline(v=0,lty=2)
axis(2,at=c(1:10),labels=colnames(BHT),las=1)
legend("bottomright",pch=c(15:18,20),
       col=c("black","blue","orange","darkgreen"),
       legend=c("Logit","Sampled","Prior Correction",
                "Weighting Correction","Firth"))
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Nominal-Level Responses: Introduction         ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# First, the "small" version of the 1992 U.S. presidential
# election data... this is from PLSC 503, and isn't in the
# slides for PLSC 504, but it might be useful as a review,
# etc.:

nes92<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/Election1992small.csv")

summary(nes92)

# Multinomial logits:

nes92.mlogit<-vglm(presvote~partyid, multinomial, nes92)
summary(nes92.mlogit)

Bush.nes92.mlogit<-vglm(formula=presvote~partyid, 
                        family=multinomial(refLevel=1),data=nes92) 
summary(Bush.nes92.mlogit)

Clinton.nes92.mlogit<-vglm(formula=presvote~partyid,
                           family=multinomial(refLevel=2),data=nes92)
summary(Clinton.nes92.mlogit)

# Conditional Logit...
#
# Reshape the data:

colnames(nes92)<-c("caseid","presvote","partyid","FT.Bush","FT.Clinton","FT.Perot")
nes92$PVote<-factor(nes92$presvote,labels=c("Bush","Clinton","Perot"))
head(nes92)

# (Re)Fit the MNL model:

nes92CL<-mlogit.data(nes92,shape="wide",choice="PVote",varying=4:6)
head(nes92CL,6)

# CL regression:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# Interpretation, using the "bigger" dataset:

BigNES92<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/Election1992.csv")

# Fit model:

NES.MNL<-vglm(presvote~partyid+age+white+female,data=BigNES92,
              multinomial(refLevel=1))
summaryvglm(NES.MNL)

# Some Wald tests:

wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(5,6))
wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(1,3,5,7,9))

# Generate "hats":

PickBush<-ifelse(fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,2] 
                 & fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,3], 1,0)
PickWJC<-ifelse(fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,3], 2, 0)
PickHRP<-ifelse(fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,2], 3, 0)
OutHat<-PickBush+PickWJC+PickHRP
table(BigNES92$presvote,OutHat)

# Odds ratios:

mnl.or <- function(model) { 
  coeffs <- c(t(coef(NES.MNL))) 
  lci <- exp(coeffs - 1.96 * diag(vcov(NES.MNL))^0.5) 
  or <- exp(coeffs) 
  uci <- exp(coeffs + 1.96* diag(vcov(NES.MNL))^0.5) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

mnl.or(NES.MNL)

# In-sample predictions:

hats<-as.data.frame(fitted.values(NES.MNL))
names(hats)<-c("Bush","Clinton","Perot")
attach(hats)

library(car)

pdf("InSampleRScatterplotMatrix.pdf",8,7)
spm(~Bush+Clinton+Perot,pch=20,plot.points=TRUE,
    diagonal="histogram",col=c("black","grey"))
dev.off()

pdf("InSampleMNLPredProbsR.pdf",8,6)
par(mfrow=c(1,3))
plot(BigNES92$partyid,Bush,xlab="Party ID")
plot(BigNES92$partyid,Clinton,xlab="Party ID")
plot(BigNES92$partyid,Perot,xlab="Party ID")
par(mfrow=c(1,1))
dev.off()

# Conditional Logit example:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# In-sample predictions:

CLhats<-predict(nes92.clogit,nes92CL)

pdf("InSampleCLHatsR.pdf",7,6)
plot(nes92$FT.Bush,CLhats[,1],pch=19,
     col=rgb(100,0,0,100,maxColorValue=255),
     xlab="Feeling Thermometer",
     ylab="Predicted Probability")
points(nes92$FT.Clinton+runif(nrow(CLhats),-1,1),
       CLhats[,2],pch=4,col=rgb(0,0,100,100,maxColorValue=255))
points(nes92$FT.Perot+runif(nrow(CLhats),-1,1),
       CLhats[,3],pch=17,col=rgb(0,100,0,50,maxColorValue=255))
lines(lowess(nes92$FT.Bush,CLhats[,1]),lwd=2,col="red")
lines(lowess(nes92$FT.Clinton,CLhats[,2]),lwd=2,col="blue")
lines(lowess(nes92$FT.Perot,CLhats[,3]),lwd=2,col="darkgreen")
legend("topleft",bty="n",c("Bush","Clinton","Perot"),
       col=c("red","blue","darkgreen"),pch=c(19,4,17))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Nominal Responses: IIA, HEV, MNP, etc...      ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Swedish (2002) election data:

Sweden<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/Sweden2002.csv")

# Multinomial logit:

Sweden.Long<-mlogit.data(Sweden,choice="partychoice",
                         shape="wide")
Sweden.MNL<-mlogit(partychoice~1|female+union+leftright+
                     age,data=Sweden.Long)
summary(Sweden.MNL)

# Restricted model (omitting Social Democrats)

Sweden.MNL.Restr<-mlogit(partychoice~1|female+union+leftright+age,
                         Sweden.Long,alt.subset=c("Conservatives","Liberals","Left Party"))

hmftest(Sweden.MNL,Sweden.MNL.Restr)

# Heteroscedastic logit / HEV:

Sweden.Het<-mlogit(partychoice~1|female+union+leftright+
                     age,data=Sweden.Long,heterosc=TRUE)
summary(Sweden.Het)

# Compare MNL and HEV coefficients:

MNLBetas<-data.frame(MNL = append(coef(Sweden.MNL),rep(NA,times=3)),
                     HEV = coef(Sweden.Het))
fit<-lm(HEV~MNL,data=MNLBetas)
B0<-round(coef(fit)[1],3)
B1<-round(coef(fit)[2],3)
R2<-round(summary(fit)$r.squared,3)

pdf("MNLvsHEV-Betas.pdf",7,6)
par(mar=c(4,4,2,2))
with(MNLBetas, 
     plot(MNL,HEV,pch=19,
          xlab="MNL Estimates",ylab="HEV Estimates",
          xlim=c(-5,20),ylim=c(-2.5,9)))
with(MNLBetas,
     text(MNL,HEV,labels=rownames(MNLBetas),pos=4,cex=0.8))
abline(h=0,lty=2)
abline(v=0,lty=2)
abline(fit)
text(3.7,8.2,labels=paste0("Intercept = ",B0))
text(3.7,7.6,labels=paste0("Slope = ",B1))
text(3.7,7,labels=paste0("R-Squared = ",R2))
arrows(3.7,6.7,7.4,4.6,length=0.1)
dev.off()

# Tests of HEV vs. MNL:

MNL.HEV.Wald <- waldtest(Sweden.Het, heterosc = FALSE) # Wald test
MNL.HEV.Wald
MNL.HEV.LR <- lrtest(Sweden.Het)         # LR test
MNL.HEV.LR
MNL.HEV.Score <- scoretest(Sweden.MNL, heterosc = TRUE)   # score test
MNL.HEV.Score

# Multinomial Probit:

Sweden.MNP<-mnp(partychoice~female+union+leftright+age,
                data=Sweden)
summary(Sweden.MNP)

# Alternative, using -mlogit-:
#
# Warning: This takes a long time...

Sweden.MNP2<-mlogit(partychoice~1|female+union+leftright+
                      age,data=Sweden.Long,probit=TRUE)
summary(Sweden.MNP2)

# Predictions:

MNL.hats<-data.frame(MNL.P = fitted(Sweden.MNL,outcome=FALSE)) # MNL
HEV.hats <- data.frame(HEV.P = fitted(Sweden.Het,outcome=FALSE)) # HEV
foo<-predict(Sweden.MNP,type="prob") # MNP
MNP.hats<-data.frame(MNP.P=foo$p)

hats<-cbind(MNL.hats,HEV.hats,MNP.hats) # combine...

# Correlation plot:

CorrMat <- cor(hats,use="pairwise.complete.obs")

pdf("AllCorrelations.pdf",8,7)
ggcorrplot(CorrMat, hc.order="TRUE",
           lab="TRUE",type="lower",
           legend.title="Correlations")
dev.off()

# fin
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=