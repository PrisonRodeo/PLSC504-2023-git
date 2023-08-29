#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                    ####
#
# Code for PLSC 503 - Spring 2023
#
# Binary Response Models II: Practicum
#
# Load packages:

P<-c("readr","car","plotrix","margins","gmodels",
     "ROCR","pROC")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# Set a working directory in here, probably, [shrug]
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# NAFTA example...                                ####

nafta<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/NAFTA.csv")

# Rescale some things:

NAFTA <- with(nafta, data.frame(Vote=vote,
                                PropHisp=pcthispc/100,
                                Democrat=democrat,
                                COPE=cope93/100))
NAFTA$DemXCOPE<-NAFTA$Democrat*NAFTA$COPE
summary(NAFTA)

rm(nafta)

# Probit:

NAFTA.probit<-glm(Vote~PropHisp+Democrat+COPE+DemXCOPE,
                   NAFTA,family=binomial(link="probit"))
summary(NAFTA.probit)

# Logit:

NAFTA.fit<-glm(Vote~PropHisp+Democrat+COPE+DemXCOPE,
                      NAFTA,family=binomial)
summary(NAFTA.fit)

# Compare logit vs. probit estimates:

comparedf <-data.frame(probit = coefficients(NAFTA.probit),
                       logit = coefficients(NAFTA.fit))
lpfit<-lm(logit~probit,data=comparedf)

pdf("NAFTAProbitVsLogit.pdf",7,6)
par(mar=c(4,4,2,2))
with(comparedf, 
     plot(probit,logit,pch=20,xlim=c(-3,3.5),
          ylim=c(-7,8),xlab="Logit Estimates",
          ylab="Probit Estimates"))
with(comparedf, text(probit,logit,labels=rownames(comparedf),
                     pos=c(1,3,3,1,4)))
abline(lpfit,lwd=2,lty=2,col="red")
text(-1,5.5,col="red",labels=paste0("Adj. R-squared = ",
                         round(summary(lpfit)$adj.r.squared,2)))
dev.off()

# Deviances...

LLR<-NAFTA.fit$null.deviance - NAFTA.fit$deviance
LLR
pchisq(LLR,4,lower.tail=FALSE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Interpreting interactions...                    ####
#
# phi-hat:

NAFTA.fit$coeff[4]+NAFTA.fit$coeff[5]

# z-statistic:

(NAFTA.fit$coeff[4]+NAFTA.fit$coeff[5]) / 
  (sqrt(vcov(NAFTA.fit)[4,4] + 
  (1)^2*vcov(NAFTA.fit)[5,5] + 
  2*1*vcov(NAFTA.fit)[4,5]))

# Square that, and it's a chi-square statistic:

((NAFTA.fit$coeff[4]+NAFTA.fit$coeff[5]) / 
  (sqrt(vcov(NAFTA.fit)[4,4] + 
          (1)^2*vcov(NAFTA.fit)[5,5] + 
          2*1*vcov(NAFTA.fit)[4,5])))^2

# Same thing, using -linear.hypothesis- in -car-:

linearHypothesis(NAFTA.fit,"COPE+DemXCOPE=0")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Marginal effects (not generally recommended):   ####

summary(margins(NAFTA.fit))

pdf("NAFTAMarginalEffects.pdf",7,6)
plot(margins(NAFTA.fit),
    labels=c("COPE","Democrat","Interaction","Percent Hispanic"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Odds Ratios:                                    ####

P<-qnorm(0.975)

lreg.or <- function(model)
{
  coeffs <- coef(summary(model))
  lowerCI <- exp(coeffs[ ,1] - P * coeffs[ ,2])
  OR <- exp(coeffs[ ,1])
  upperCI <- exp(coeffs[ ,1] + P * coeffs[ ,2])
  lreg.or <- cbind(OR,lowerCI,upperCI)        
  lreg.or
}

lreg.or(NAFTA.fit)

# Or you can use this:

exp(cbind(OR=coef(NAFTA.fit),confint.default(NAFTA.fit)))

# Plot of odds ratios against size of change in PropHispc:

Change<-seq(0.01,0.99,by=0.01)
PctChange<-((exp(2.09*Change)-1)*100)

pdf("ORIllustration.pdf",7,6)
plot(Change,PctChange,t="l",lwd=2,
     xlab="Size of Change in PropHisp",
     ylab="Percentage Change in Odds of a Pro-NAFTA Vote")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Predicted values...                             ####
#
# In-sample:

preds<-NAFTA.fit$fitted.values
hats<-predict(NAFTA.fit,se.fit=TRUE)

# Plotting in-sample predictions:

XBUB<-hats$fit + (1.96*hats$se.fit) 
XBLB<-hats$fit - (1.96*hats$se.fit)
plotdata<-cbind(as.data.frame(hats),XBUB,XBLB)
plotdata<-data.frame(lapply(plotdata,binomial(link="logit")$linkinv))
par(mfrow=c(1,2))

with(NAFTA, 
 plotCI(COPE[Democrat==1],plotdata$fit[Democrat==1],ui=plotdata$XBUB[Democrat==1],
         li=plotdata$XBLB[Democrat==1],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)"))
with(NAFTA, 
 plotCI(COPE[Democrat==0],plotdata$fit[Democrat==0],ui=plotdata$XBUB[Democrat==0],
         li=plotdata$XBLB[Democrat==0],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)"))

# Out-of-sample predictions:

sim.data<-data.frame(PropHisp=mean(NAFTA$PropHisp),Democrat=rep(0:1,101),
                       COPE=seq(from=0,to=1,length.out=101))
sim.data$DemXCOPE<-sim.data$Democrat*sim.data$COPE

OutHats<-predict(NAFTA.fit,se.fit=TRUE,newdata=sim.data)
OutHatsUB<-OutHats$fit+(1.96*OutHats$se.fit)
OutHatsLB<-OutHats$fit-(1.96*OutHats$se.fit)
OutHats<-cbind(as.data.frame(OutHats),OutHatsUB,OutHatsLB)
OutHats<-data.frame(lapply(OutHats,binomial(link="logit")$linkinv))

both<-cbind(sim.data,OutHats)
both<-both[order(both$COPE,both$Democrat),]
bothD<-both[both$Democrat==1,]
bothR<-both[both$Democrat==0,]

par(mfrow=c(1,2))

plot(bothD$COPE,bothD$fit,t="l",lwd=2,ylim=c(0,1),
     xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(bothD$COPE,bothD$OutHatsUB,lty=2)
lines(bothD$COPE,bothD$OutHatsLB,lty=2)
text(0.3,0.2,label="Democrats")

plot(bothR$COPE,bothR$fit,t="l",lwd=2,ylim=c(0,1),
     xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(bothR$COPE,bothR$OutHatsUB,lty=2)
lines(bothR$COPE,bothR$OutHatsLB,lty=2)
text(0.7,0.9,label="Republicans")

# Simpler version (for when there aren't interactions,
# and you re interested in a single continuous
# predictor), using -margins-:

pdf("PropHispPredPlot.pdf",7,6)
par(mar=c(4,4,2,2))
cplot(NAFTA.fit,"PropHisp",xlab="Proportion Hispanic")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Goodness of fit...                              ####
#
# PRE (with tau=0.5):

table(NAFTA.fit$fitted.values>0.5,NAFTA$Vote==1)
chisq.test(NAFTA.fit$fitted.values>0.5,NAFTA$Vote==1)

# PREs, with different cutoffs / taus...
#
# Tau = 0.2:

Hats02<-ifelse(NAFTA.fit$fitted.values>0.2,1,0)
CrossTable(NAFTA$Vote,Hats02,prop.r=FALSE,prop.c=FALSE,
           prop.t=FALSE,prop.chisq=FALSE)

# Tau = 0.8:

Hats08<-ifelse(NAFTA.fit$fitted.values>0.8,1,0)
CrossTable(NAFTA$Vote,Hats08,prop.r=FALSE,prop.c=FALSE,
           prop.t=FALSE,prop.chisq=FALSE)


# ROC curves, plots, etc. (using -ROCR-):

NAFTA.hats<-predict(NAFTA.fit,type="response")
preds<-ROCR::prediction(NAFTA.hats,NAFTA$Vote)
plot(performance(preds,"tpr","fpr"),lwd=2,lty=2,
       col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)

# "Bad" model:

NAFTA.bad<-with(NAFTA,
                glm(Vote~PropHisp,family=binomial(link="logit")))
NAFTA.bad.hats<-predict(NAFTA.bad,type="response")
bad.preds<-ROCR::prediction(NAFTA.bad.hats,NAFTA$Vote)
plot(performance(bad.preds,"tpr","fpr"),lwd=2,lty=2,
     col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)

# Comparing ROCs:

GoodROC<-roc(NAFTA$Vote,NAFTA.hats,ci=TRUE)
GoodAUC<-auc(GoodROC)
BadROC<-roc(NAFTA$Vote,NAFTA.bad.hats)
BadAUC<-auc(BadROC)

GoodAUC

BadAUC

# Comparison plot:

pdf("TwoROCs.pdf",5,5)
par(mar=c(4,4,2,2))
plot(GoodROC)
lines(BadROC,col="red",lty=2)
dev.off()


# fin
