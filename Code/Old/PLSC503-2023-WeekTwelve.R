#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                   ####
#
# PLSC 503 -- Spring 2023
#
# Regression models for nominal- and
# ordinal-level outcomes...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.

P<-c("readr","MASS","mlogit","nnet","VGAM","MNLpred",
     "aod","car","ggplot2","scales","margins")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd() as you like, or whatever, e.g.:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Multinomial logit, etc.                        ####

nes92<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/Election1992small.csv")

summary(nes92)

nes92.mlogit<-vglm(presvote~partyid, multinomial, nes92)
summary(nes92.mlogit)

Bush.nes92.mlogit<-vglm(formula=presvote~partyid, 
                        family=multinomial(refLevel=1),data=nes92) 
summary(Bush.nes92.mlogit)

Clinton.nes92.mlogit<-vglm(formula=presvote~partyid,
                           family=multinomial(refLevel=2),data=nes92)
summary(Clinton.nes92.mlogit)

# Conditional logit...

colnames(nes92)<-c("caseid","presvote","partyid","FT.Bush","FT.Clinton","FT.Perot")
nes92$PVote<-factor(nes92$presvote,labels=c("Bush","Clinton","Perot"))
head(nes92)

nes92CL<-mlogit.data(nes92,shape="wide",choice="PVote",varying=4:6)
head(nes92CL,6)

# Conditional logistic regression:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# Interpretation part (with more predictors...):

BigNES92<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/Election1992.csv")

NES.MNL<-vglm(presvote~partyid+age+white+female,data=BigNES92,
              multinomial(refLevel=1))
summaryvglm(NES.MNL)

wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(5,6))
wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(1,3,5,7,9))

# Marginal effects, via -margins-...
#
# Refit model using -multinom-:

BigNES92$PresVote<-cut(BigNES92$presvote,3,labels=c("Bush","Clinton","Perot"))
BigNES92$White<-ifelse(BigNES92$white=="White",1,0) # numeric
MNL.alt<-multinom(PresVote~partyid+age+White+female,data=BigNES92,
                  Hess=TRUE)
summary(marginal_effects(MNL.alt))

# Hats, yo:

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

# Predicted probabilities using -MNLpred-...
#
# Recall the -multinom- estimates:

summary(MNL.alt)

# Predictions:

Hats<-mnl_pred_ova(model=MNL.alt,data=BigNES92,
                   x="partyid",by=0.1,seed=7222009,nsim=500)

# Plotting predicted probabilities & CIs
# (via ggplot; can also be done easily with 
# base R):

cand.labs <- c("Bush", "Clinton", "Perot")
names(cand.labs) <- c("1", "2", "3")

pdf("MNLPredictedProbabilities.pdf",8,6)
ggplot(data=Hats$plotdata,aes(x=partyid,y=mean,
            ymin=lower,ymax=upper)) +
  geom_ribbon(alpha = 0.1) +
  geom_line() + theme_bw() +
  facet_wrap(PresVote~.,scales="fixed",
             labeller=labeller(presvote=cand.labs)) +
  scale_x_continuous(breaks=1:7) +
  labs(y = "Predicted Probabilities",x = "Party Identification")
dev.off()

# Plotting first differences for the FEMALE variable:

FDF<-mnl_fd2_ova(model=MNL.alt,data=BigNES92,x="White",
                 value1=min(BigNES92$White),
                 value2=max(BigNES92$White),nsim=500)

pdf("MNLFirstDifferences.pdf",7,5)
ggplot(FDF$plotdata_fd,aes(categories, y=mean,
                           ymin=lower,max=upper)) +
  geom_pointrange() + geom_hline(yintercept=0) +
  scale_y_continuous(name="First Difference: White") +
  labs(x = "Candidate") + theme_bw()
dev.off()

# Conditional logit example:

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

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now, ordinal-response models...                     ####
# 
# GOP Thermometer score plot:

ANES<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/ANES-pilot-2016.csv")

ANES$ftjeb<-ifelse(ANES$ftjeb==998,NA,ANES$ftjeb)

pdf("Notes/ANES-FT-Jeb-2016.pdf",7,6)
par(mar=c(4,4,2,2))
hist(ANES$ftjeb,breaks=seq(0,100,by=1),main="",
     xlab="Feeling Thermometer Score for Jeb!")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ordered simulation:

set.seed(7222009)
X<-runif(1000,0,10)
Ystar<-0 + 1*X + rnorm(1000)
Y1<-Ystar
Y1[Ystar<2.5]<-1
Y1[Ystar>=2.5 & Ystar<5]<-2
Y1[Ystar>=5 & Ystar<7.5]<-3
Y1[Ystar>=7.5]<-4
table(Y1)

summary(lm(Ystar~X))
summary(lm(Y1~X))

pdf("OrdinalOneR.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(X,Ystar,pch=20,xlab="X",ylab="Y*")
abline(lm(Ystar~X),lwd=3,col="red")
abline(h=c(2.5,5,7.5),lty=2)
plot(X,Y1,pch=20,xlab="X",ylab="Y1")
abline(lm(Y1~X),lwd=3,col="red")
dev.off()

Y2<-Ystar
Y2[Ystar<2]<-1
Y2[Ystar>=2 & Ystar<8]<-2
Y2[Ystar>=8 & Ystar<9]<-3
Y2[Ystar>9]<-4
table(Y2)

summary(lm(Y2~X))

pdf("OrdinalTwoR.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(X,Ystar,pch=20,xlab="X",ylab="Y*")
abline(lm(Ystar~X),lwd=3,col="red")
abline(h=c(2,8,9),lty=2)
plot(X,Y2,pch=20,xlab="X",ylab="Y2")
abline(lm(Y2~X),lwd=3,col="red")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Best Example Ever...                            ####

beer<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/Beer.csv")

summary(beer)

beer.logit<-polr(as.factor(quality)~price+calories+craftbeer
                 +bitter+malty,data=beer)
summary(beer.logit)

beer.probit<-polr(as.factor(quality)~price+calories+craftbeer+
                    bitter+malty,data=beer,method="probit")
summary(beer.probit)

# Odds Ratios

olreg.or <- function(model) { 
  coeffs <- coef(summary(beer.logit)) 
  lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2]) 
  or <- exp(coeffs[ ,1]) 
  uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2]) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

olreg.or(beer.logit)

# Predicted probs

calories<-seq(60,200,1)
price<-mean(beer$price)
craftbeer<-median(beer$craftbeer)
bitter<-mean(beer$bitter)
malty<-mean(beer$malty)
beersim<-cbind(calories,price,craftbeer,bitter,malty)
beer.hat<-predict(beer.logit,beersim,type='probs')

pdf("ROrdinalProbs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(c(60,200), c(0,1), type='n', xlab="Calories", ylab='Fitted 
     Probability')
lines(60:200, beer.hat[1:141, 1], lty=1, lwd=3)
lines(60:200, beer.hat[1:141, 2], lty=2, lwd=3)
lines(60:200, beer.hat[1:141, 3], lty=3, lwd=3)
lines(60:200, beer.hat[1:141, 4], lty=4, lwd=3)
dev.off()

# Cumulative probs:

xaxis<-c(60,60:200,200)
yaxis1<-c(0,beer.hat[,1],0)
yaxis2<-c(0,beer.hat[,2]+beer.hat[,1],0)
yaxis3<-c(0,beer.hat[,3]+beer.hat[,2]+beer.hat[,1],0)
yaxis4<-c(0,beer.hat[,4]+beer.hat[,3]+beer.hat[,2]+beer.hat[,1],0)

pdf("ROrdinalCumProbs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(c(60,200), c(0,1), type='n', xlab="Calories", 
     ylab="Cumulative Probability")
polygon(xaxis,yaxis4,col="white")
polygon(xaxis,yaxis3,col="grey80")
polygon(xaxis,yaxis2,col="grey50")
polygon(xaxis,yaxis1,col="grey10")
dev.off()

# /fin