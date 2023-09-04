#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                   ####
#
# PLSC 503 -- Spring 2023
#
# Event Count Models
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# Packages, etc.

P<-c("readr","MASS","psych","ggplot2","mfx","margins")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# setwd(), whatevz:
#
#setwd("~/Dropbox (Personal)/PLSC 503/Notes")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Various Poisson histograms                     ####

set.seed(7222009)
N<-1000
LP05<-rpois(N,0.5)
LP1<-rpois(N,1)
LP5<-rpois(N,5)
LP10<-rpois(N,10)

pdf("PoissonHistogramsR.pdf",7,6)
par(mfrow=c(2,2))
hist(LP05,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 0.5")
hist(LP1,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 1.0")
hist(LP5,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 5")
hist(LP10,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 10")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dahl / Poisson example...                      ####

NewDahl<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/NewDahl.csv")

NewDahl<-NewDahl[complete.cases(NewDahl),]
row.names(NewDahl)<-NewDahl$Year
summary(NewDahl)

pdf("NewDahlScatterplotMatrix.pdf",10,8)
with(NewDahl, scatterplotMatrix(~NNulls+Age+Tenure+Unified,
                                pch=19,col="black",
                                regLine=list(col="red"),
                                smooth=list(spread=FALSE)))
dev.off()

# Poisson regression:

nulls.poisson<-glm(NNulls~Age+Tenure+Unified,family="poisson",
                   data=NewDahl)
summary(nulls.poisson)

# IRRs:

nulls.poisson.IRR<-poissonirr(NNulls~Age+Tenure+Unified,
                              data=NewDahl)
nulls.poisson.IRR

# Predictions:

simdata<-data.frame(Age=seq(from=45,to=71,by=1),
                    Tenure=mean(NewDahl$Tenure,na.rm=TRUE),
                    Unified=1)
nullhats<-predict(nulls.poisson,newdata=simdata,se.fit=TRUE)

# NOTE: These are XBs, not predicted counts.
# Transforming:

nullhats$Yhat<-exp(nullhats$fit)
nullhats$UB<-exp(nullhats$fit + 1.96*(nullhats$se.fit))
nullhats$LB<-exp(nullhats$fit - 1.96*(nullhats$se.fit))

# Plot...

pdf("NewNullsOutOfSampleHatsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(simdata$Age,nullhats$Yhat,t="l",lwd=3,ylim=c(0,5),ylab=
       "Predicted Count", xlab="Mean Court Age")
lines(simdata$Age,nullhats$UB,lwd=2,lty=2)
lines(simdata$Age,nullhats$LB,lwd=2,lty=2)
dev.off()

# Predicted probabilities...
#
# Generate predicted probabilities for each possible 
# value of Y in {0,10} for two hypothetical cases:

LambdaLo<-exp(nullhats$fit[[13]]) # Age = 57
LambdaHi<-exp(nullhats$fit[[24]]) # Age = 67
PredPrs<-data.frame(Count=0:10,
                    Age57=dpois(0:10,LambdaLo),
                    Age67=dpois(0:10,LambdaHi))

# Plots:

pdf("NewPoissonPredProbs.pdf",10,5)
par(mfrow=c(1,2))
par(mar=c(4,4,4,2))
barplot(PredPrs$Age57,names.arg=0:10,
        xlab="Number of Nullifications",
        ylab="Predicted Probability",
        main="Mean Court w/Age = 57",
        ylim=c(0,0.9))
barplot(PredPrs$Age67,names.arg=0:10,
        xlab="Number of Nullifications",
        ylab="Predicted Probability",
        main="Mean Court w/Age = 67",
        ylim=c(0,0.9))
dev.off()

# Changes in predicted probabilities:

PredPrs$Change<-with(PredPrs,Age67-Age57)

pdf("PoissonChangesInPredPrs.pdf",8,5)
par(mfrow=c(1,1))
par(mar=c(4,4,4,2))
foo<-barplot(PredPrs$Change,horiz=TRUE,
        xlab="Change in Predicted Probability of Count",
        ylab="Count",xlim=c(-0.7,0.35),
        main="Changes: Mean Age = 57 to Mean Age = 67")
axis(2,foo,labels=0:10,cex=0.8)
text(PredPrs$Change,foo,labels=round(PredPrs$Change,3),
     cex=0.6,pos=c(2,rep(4,times=10)))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Offsets with dyadic data...                    ####
#
# Aggregated counts of conflicts between the countries 
# in each dyad, 1950-1985...

IR<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/offsetIR.csv")

summary(IR)

cor(IR,use="complete.obs")

IR.fit1<-glm(disputes~allies+openness,data=IR,family="poisson")
summary(IR.fit1)

IR.fit2<-glm(disputes~allies+openness,data=IR,family="poisson",
             offset=log(Ndyads))
summary(IR.fit2)

IR.fit3<-glm(disputes~allies+openness+log(Ndyads),data=IR,
             family="poisson")
summary(IR.fit3)

# z-test:
2*pnorm((0.811-1)/.071)

# Wald test:
wald.test(b=coef(IR.fit3),Sigma=vcov(IR.fit3),Terms=4,H0=1)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# CPB Max(Y) figure:

L.CPB <- seq(0.1,10,by=0.1)
MaxY8 <- (-L.CPB) / (0.8-1)
MaxY5 <- (-L.CPB) / (0.5-1)
MaxY2 <- (-L.CPB) / (0.2-1)

pdf("CPBMaxR.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
plot(L.CPB,MaxY8,t="l",lwd=3,col="black",xlab="Lambda",
     ylab="Maximum Value of Y")
lines(L.CPB,MaxY5,lwd=3,col="red",lty=2)
lines(L.CPB,MaxY2,lwd=3,col="darkgreen",lty=3)
legend("topleft",bty="n",lty=c(1,2,3),lwd=3,
       col=c("black","red","darkgreen"),
       legend=c("Alpha = 0.8","Alpha = 0.5","Alpha = 0.2"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simulated Poisson / NB example...              ####
#
# Poisson data (N=400):

N<-400
set.seed(7222009)
X <- runif(N,min=0,max=1)
YPois <- rpois(N,exp(0+1*X))           # Poisson
YNB <- rnbinom(N,size=1,mu=exp(0+1*X)) # NB with theta=1.0

describe(cbind(YPois,YNB))

# Density plots:

pdf("PoisNBDensities.pdf",8,6)
par(mar=c(4,4,2,2))
plot(density(YNB),lwd=2,lty=2,col="red",
     ylim=c(0,0.4),main="",xlab="Y")
lines(density(YPois),lwd=2,lty=1,col="black")
legend("topright",bty="n",lty=c(1,2),col=c("black","red"),
       lwd=c(2,2),legend=c("Poisson","Neg. Bin. (theta=1)"))
dev.off()


# Regressions:

summary(glm(YPois~X,family="poisson")) # Poisson
summary(glm.nb(YPois~X))               # NB

# More regressions:

summary(glm(YNB~X,family="poisson"))  # Poisson
summary(glm.nb(YNB~X))                # NB

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now, a bigger/better sim:

Sims <- 250 # (250 sims each)
theta <- seq(0.1,4,by=0.1) # values of theta
diffs<-matrix(nrow=Sims,ncol=length(theta))

set.seed(7222009)
for(j in 1:length(theta)) {
        for(i in 1:Sims) {
                X<-runif(N,min=0,max=1)
                Y<-rnbinom(N,size=theta[j],mu=exp(0+1*X))
                p<-glm(Y~X,family="poisson")
                nb<-glm.nb(Y~X)
                diffs[i,j]<- ((sqrt(vcov(p))[2,2]) / sqrt(vcov(nb))[2,2])*100
        }
}

Dmeans<-colMeans(diffs)
Dmin<-apply(diffs,2,min)
Dmax<-apply(diffs,2,max)

pdf("PoissonSEsWithNB.pdf",7,6)
par(mar=c(4,4,2,2))
plot(theta,Dmeans,xlim=c(0,4),ylim=c(0,100),
     pch=20,ylab="Percentage",xlab=expression(theta))
segments(theta,Dmin,theta,Dmax)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "Dahl" Poisson results, overdispersion 
# test "by hand":

Phats<-fitted.values(nulls.poisson)
Uhats<-((NewDahl$NNulls-Phats)^2 - NewDahl$NNulls) / (Phats * sqrt(2))
summary(lm(Uhats~Phats))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Negative Binomial regression                   ####         

nulls.NB<-glm.nb(NNulls~Age+Tenure+Unified,data=NewDahl)
summary(nulls.NB)

# alpha:

1 / nulls.NB$theta

# Alternative fitting:

library(msme)
nulls.nb2<-nbinomial(NNulls~Age+Tenure+Unified,data=NewDahl)
summary(nulls.nb2)

# Coefficient estimates:

cbind(nulls.poisson$coefficients,coef(nulls.NB))

# Estimated standard errors:

cbind(diag(sqrt(vcov(nulls.poisson))),diag(sqrt(vcov(nulls.NB))))

# Plot:

pdf("NewPoissonNBYHatsR.pdf",7,6)
par(mar=c(4,4,2,2))
plot(nulls.poisson$fitted.values,nulls.NB$fitted.values,pch=20,
     xlab="Poisson",ylab="Negative Binomial",main="",
     xlim=c(0,3),ylim=c(0,3))
abline(a=0,b=1,lwd=1,lty=2)
dev.off()
