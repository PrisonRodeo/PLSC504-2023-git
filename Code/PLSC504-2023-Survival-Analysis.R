#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things...                            ####
#
# PLSC 504 -- Fall 2023
#
# Survival Analysis (An Introduction)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Options:

options(scipen=5) # bias against scientific notation
options(digits=2) # show fewer decimal places

# Set as necessary:
#
# setwd("~/Dropbox (Personal)/PLSC 504/Notes")
#
# Packages, etc.:                           
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","gtools","nnet","mstate","plyr","texreg",
     "survival","eha","rms","flexsurv","pscl","smcure",
     "nltm","coxme")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 6-7 times until you get "Package count = 14"
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Basics                                     ####
#
# Read KABL data:

KABL<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/KABL.csv")

# Create survival object:

KABL.S<-Surv(KABL$durat,KABL$ciep12)

# Survival curve estimates:

KABL.fit<-survfit(KABL.S~1)

# Plot:

pdf("KABLKM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.fit,xlab="Time (in months)",
     ylab="Survival Estimate",
     lwd=c(3,1,1))
dev.off()

# Estimates of H(t):

KABL.FH<-survfit(KABL.S~1,type="fleming")

# Plot:

pdf("KABLNA.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.FH$time,-log(KABL.FH$surv),t="l",lwd=3,lty=1,
     xlab="Time (in months)",ylab="Cumulative Hazard",
     ylim=c(0,2.5))
lines(KABL.FH$time,-log(KABL.FH$upper),t="l",lwd=2,lty=2)
lines(KABL.FH$time,-log(KABL.FH$lower),t="l",lwd=2,lty=2)
points(KABL.FH$time[KABL.FH$n.censor>0],
       -log(KABL.FH$surv[KABL.FH$n.censor>0]),pch=19)
dev.off()

# Alternatively, w/no CIs...:
#
# NAalen<-cumsum(KABL.fit$n.event / KABL.fit$n.risk) 
# Ht<- -log(KABL.fit$surv) # Alternative estimate
# 
# pdf("KABLNA.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(NAalen~KABL.fit$time,t="l",lwd=3,xlab="Time (in months)",
#      ylab="Cumulative Hazard Estimate")
# points(KABL.fit$time[KABL.fit$n.censor>0],
#        NAalen[KABL.fit$n.censor>0],pch=4)
# dev.off()

# Log-rank test:

survdiff(KABL.S~invest,data=KABL,rho=0)

# Survival curve comparison:

KABL.fit2<-survfit(KABL.S~invest,data=KABL)

# Plot:

pdf("KABL-By-Invest.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.fit2,conf.int=TRUE,lty=c(1,2),lwd=c(3,1,1,3,1,1),
     col=c("black","red"),ylab="Survival Estimate",
     xlab="Time (in months)", mark.time=FALSE)
legend("topright",inset=.02,
       c("No Investiture Requirement","Investiture Requirement"),
       lty=c(1,2),lwd=c(3,3),col=c("black","red"),bty="n")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Parametric regression models...            ####
#
# Survival object (again):

KABL.S<-Surv(KABL$durat,KABL$ciep12)

# KM plot:

KABL.KapM<-survfit(KABL.S~1)

pdf("KABLKapM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.KapM,lwd=c(3,1,1),mark.time=FALSE,
     ylab="Survival Probability",xlab="Time (in months)")
dev.off()

# Covariates:

xvars<-c("fract","polar","format","invest","numst2","eltime2","caretk2")
MODEL<-as.formula(paste(paste("KABL.S ~ ", paste(xvars,collapse="+"))))

# Exponential:

KABL.exp<-phreg(MODEL,data=KABL,dist="weibull",shape=1)
KABL.exp

# Interpretation (using -flexsurvreg- + "fake" data):

KABL.exp2<-flexsurvreg(MODEL,data=KABL,dist="exp") # re-fit model
FakeInvest<-t(c(mean(KABL$fract),mean(KABL$polar),mean(KABL$format),1,
                mean(KABL$numst2),mean(KABL$eltime2),mean(KABL$caretk2)))
FakeNoInvest<-t(c(mean(KABL$fract),mean(KABL$polar),mean(KABL$format),0,
                  mean(KABL$numst2),mean(KABL$eltime2),mean(KABL$caretk2)))
colnames(FakeInvest)<-xvars
colnames(FakeNoInvest)<-xvars

# Plot:

pdf("ExpSurvCompare.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.exp2,FakeInvest,mark.time=FALSE,col.obs="black",
     lty.obs=c(0,0,0),xlab="Time (in months)",ylab="Survival Probability")
lines(KABL.exp2,FakeNoInvest,mark.time=FALSE,col.obs="black",
      lty.obs=c(0,0,0),col=c(rep("green",times=3)))
legend("topright",inset=0.05,bty="n",
       c("Investiture Requirement","No Investiture Requirement"),
       lty=c(1,1),lwd=c(2,2),col=c("red","green"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Weibulls...
#
# Plot of various hazard shapes:

t<-cbind(1:60,1:60,1:60)
P<-c(0.5,1,2)
WeibullHs<-t(apply(t,1,function(t) 0.02*P*((0.02*t)^(P-1))))
WeibullSs<-t(apply(t,1,function(t) (exp(-0.02*t))^P))

# Plots:

pdf("WeibullHSims.pdf",6,5)
par(mar=c(4,4,2,2))
plot(t[,1],WeibullHs[,1],t="l",lwd=3,lty=1,col="green",
     xlab="Time",ylab="Hazard",ylim=c(0,0.08))
lines(t[,2],WeibullHs[,2],t="l",lwd=3,lty=2,col="black")
lines(t[,3],WeibullHs[,3],t="l",lwd=3,lty=3,col="red")
legend("topright",inset=.02,
       c("p = 0.5","p = 1.0","p = 2.0"),
       lty=c(1,2,3),lwd=c(3,3,3),col=c("green","black","red"),
       cex=1.2,bty="n")
dev.off()

pdf("WeibullSSims.pdf",6,5)
par(mar=c(4,4,2,2))
plot(t[,1],WeibullSs[,1],t="l",lwd=3,lty=1,col="green",
     xlab="Time",ylab="Survival Probability",ylim=c(0,1))
lines(t[,2],WeibullSs[,2],t="l",lwd=3,lty=2,col="black")
lines(t[,3],WeibullSs[,3],t="l",lwd=3,lty=3,col="red")
legend("bottomleft",inset=.02,
       c("p = 0.5","p = 1.0","p = 2.0"),
       lty=c(1,2,3),lwd=c(3,3,3),col=c("green","black","red"),
       cex=1.2,bty="n")
dev.off()

# Weibull KABL:

KABL.weib<-phreg(MODEL,data=KABL,dist="weibull")
KABL.weib

# Comparing Weibull survival curves...

KABL.weib2<-survreg(MODEL,data=KABL,dist="weibull")  # re-fit model

KABL.weib.Ihat<-predict(KABL.weib2,newdata=as.data.frame(FakeInvest),
                        type="quantile",se.fit=TRUE,p=seq(.01,.99,by=.01))

KABL.weib.NoIhat<-predict(KABL.weib2,newdata=as.data.frame(FakeNoInvest),
                          type="quantile",se.fit=TRUE,p=seq(.01,.99,by=.01))

# Plot:

pdf("WeibSurvCompare.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.weib.NoIhat$fit,seq(.99,.01,by=-.01),t="l",lwd=3,col="green",
     xlab="Time (in months)",ylab="Survival Probability")
lines(KABL.weib.Ihat$fit,seq(.99,.01,by=-.01),lwd=3,col="red")
lines(KABL.weib.NoIhat$fit+1.96*(KABL.weib.NoIhat$se),
      seq(.99,.01,by=-.01),lty=2,lwd=1,col="green")
lines(KABL.weib.NoIhat$fit-1.96*(KABL.weib.NoIhat$se),
      seq(.99,.01,by=-.01),lty=2,lwd=1,col="green")
lines(KABL.weib.Ihat$fit+1.96*(KABL.weib.NoIhat$se),
      seq(.99,.01,by=-.01),lty=2,lwd=1,col="red")
lines(KABL.weib.Ihat$fit-1.96*(KABL.weib.NoIhat$se),
      seq(.99,.01,by=-.01),lty=2,lwd=1,col="red")
legend("topright",inset=0.05,bty="n",
       c("Investiture Requirement","No Investiture Requirement"),
       lty=c(1,1),lwd=c(2,2),col=c("red","green"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Cox proportional hazards model...          ####
#
# O'Neal/Russett data & examples:

OR<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/OR.csv")

summary(OR)

# Surv object...

OR.S<-Surv(OR$start,OR$stop,OR$dispute,type=c('counting'))
OR.KM<-survfit(OR.S~1)

# Kaplan-Meier:

pdf("ORKM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OR.KM,mark.time=FALSE,lwd=c(2,1,1),
     xlab="Time (in years)",ylab="Survival Probability")
dev.off()

# Cox model w/OR data (Breslow):

ORCox.br<-coxph(OR.S~allies+contig+capratio+growth+democracy+trade,
                data=OR,na.action=na.exclude, method="breslow")

# Scaling covariates:

OR$growthPct<-OR$growth*100
summary(coxph(OR.S~allies+contig+capratio+growthPct+democracy+trade,
              data=OR,na.action=na.exclude, method="breslow"))

# Baseline (cumulative) hazard:

OR.BH<-basehaz(ORCox.br,centered=FALSE)

# Plot:

pdf("ORCoxBaseH.pdf",10,5)
par(mar=c(4,4,2,2))
plot(OR.BH$time,OR.BH$hazard,t="l",lwd=4,col="red",
     xlab="Time (in years)",ylab="Baseline Integrated Hazard")
lines(abline(lm(OR.BH$hazard~0+OR.BH$time),lty=2,lwd=2))
legend("bottomright",inset=0.02,bty="n",
       c("Baseline Hazard","Linear Fit"),lty=c(1,2),
       lwd=c(4,2),col=c("red","black"))
dev.off()

# Comparing survival curves:

FakeContig<-as.data.frame(t(c(mean(OR$allies),1,mean(OR$capratio),mean(OR$growth),
                              mean(OR$democracy),mean(OR$trade))))
FakeApart<-as.data.frame(t(c(mean(OR$allies),0,mean(OR$capratio),mean(OR$growth),
                             mean(OR$democracy),mean(OR$trade))))
colnames(FakeContig)<-c("allies","contig","capratio","growth",
                        "democracy","trade")
colnames(FakeApart)<-c("allies","contig","capratio","growth",
                       "democracy","trade")

FCHat<-survfit(ORCox.br,FakeContig)
FAHat<-survfit(ORCox.br,FakeApart)

# Plot:

pdf("SurvCompare.pdf",6,5)
par(mar=c(4,4,2,2))
plot(FAHat,col=c(rep("black",times=3)),lwd=c(3,1,1),lty=c(1,2,2),
     xlab="Time (in years)",ylab="Survival Probability",
     mark.time=FALSE)
par(new=TRUE)
plot(FCHat,col=c(rep("red",times=3)),lwd=c(3,1,1),lty=c(2,2,2),
     mark.time=FALSE)
legend("bottomleft",inset=0.02,bty="n",
       c("Non-Contiguous","Contiguous"),lty=c(1,2),
       lwd=c(3,3),col=c("black","red"))
dev.off()

# /fin