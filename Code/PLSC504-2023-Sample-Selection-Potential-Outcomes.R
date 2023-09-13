#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                       ####
#
# PLSC 504 -- Fall 2023
#
# Models for Endogenous Sample Selection, and
# introduction to potential outcomes / causal
# inference...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","lme4","plm","gtools","gplots","plyr",
     "texreg","statmod","mvtnorm","sampleSelection",
     "stargazer")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 4-5 times until you get all smileys. :)
#
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# Set your working directory, too!...or not, whateverz.

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Sample Selection Bias + Selection Models           ####
#
# Truncation / selection bias plots:

set.seed(7222009)
N<-400
X<-runif(N,-3,3)
Y<-3*X+runif(N,-6,6)
YT<-ifelse(Y>0,Y,NA) # truncated @ zero
fit<-lm(Y~X)
fitT<-lm(YT~X)

pdf("TruncationBiasII.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y)
points(X,YT,pch=20,cex=0.8,col="darkgreen")
abline(fit,lwd=3)
abline(fitT,lwd=2,lty=2,col="darkgreen")
abline(h=0,lty=2,lwd=1)
abline(v=0,lty=2,lwd=1)
lines(gplots::lowess(YT~X,f=0.5),type="l",col="red",
      lwd=4,lty=4)
legend("topleft",bty="n",pch=c(1,20,NA,NA,NA),cex=0.9,
       lty=c(NA,NA,1,2,4),lwd=c(NA,NA,3,2,4),
       col=c("black","darkgreen","black","darkgreen","red"),
       legend=c("Untruncated Y","Truncated Y","Untruncated Fit",
                "Truncated Fit","Truncated Lowess"))
dev.off()

# Now do selection bias / stochastic truncation:

set.seed(7222009)
N<-400
X<-runif(N,-3,3)
Y<-3*X+runif(N,-6,6)
Tind<-rbinom(N,1,pnorm(Y/3)) # stochastic truncation
YT<-ifelse(Tind==1,Y,NA)
fit<-lm(Y~X)
fitT<-lm(YT~X)

pdf("SampleSelectionBiasII.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y)
points(X,YT,pch=20,cex=0.8,col="darkgreen")
abline(fit,lwd=3)
abline(fitT,lwd=2,lty=2,col="darkgreen")
abline(h=0,lty=2,lwd=1)
abline(v=0,lty=2,lwd=1)
lines(gplots::lowess(YT~X,f=0.5),type="l",col="red",
      lwd=4,lty=4)
legend("topleft",bty="n",pch=c(1,20,NA,NA,NA),cex=0.85,
       lty=c(NA,NA,1,2,4),lwd=c(NA,NA,3,2,4),
       col=c("black","darkgreen","black","darkgreen","red"),
       legend=c("All Y","Sample-Selected Y","All Observations Fit",
                "Sample-Selected Fit","Sample-Selected Lowess"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Example: Simulated (sorry...)
#
# First, the situation where Cov(X,Z)=0 but 
# Cov(u1,u2)=0.7:

set.seed(7222009)
N  <- 1000          # N of observations
# Bivariate normal us, correlated at r=0.7
us <- rmvnorm(N,c(0,0),matrix(c(1,0.7,0.7,1),2,2))
Z  <- runif(N)      # Sel. variable
Sel<- Z + us[,1]>0  # Selection eq.
X  <- runif(N)      # X
Y  <- X + us[,2]    # B0=0, B1=1
Yob<- ifelse(Sel==TRUE,Y,NA)     # Selected Y

# OLSs:

NoSel<-lm(Y~X)      # all data
WithSel<-lm(Yob~X)  # sample-selected data

# Plot that:

pdf("HeckmanExample1.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=1)
points(X,Yob,pch=20,cex=0.8,col="darkgreen")
abline(NoSel,lwd=2)
abline(WithSel,lwd=2,lty=2,col="darkgreen")
dev.off()

# Two-Step:

probit<-glm(Sel~Z,family=binomial(link="probit"))
IMR<-((1/sqrt(2*pi))*exp(-((probit$linear.predictors)^2/2))) / 
  pnorm(probit$linear.predictors)

OLS2step<-lm(Yob~X+IMR)
summary(OLS2step)

# FIML:

FIML<-selection(Sel~Z,Y~X,method="ml")
summary(FIML)

# Results:

y.labels<-c("X (true OLS = 1)","IMR","Constant (true = 0)")
cols<-c("OLS-All","OLS-Selected","Two-Stage","FIML")
stargazer(NoSel,WithSel,OLS2step,FIML,type="latex",
          covariate.labels=y.labels,
          column.labels=cols,model.names=FALSE,
          omit.stat=c("f","ser"),
          model.numbers=FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now the same thing, but with Cov(X,Z)=0.8 too:

set.seed(9021970)
N  <- 1000          # N of observations

# Bivariate normal us & Xs, correlated at r=0.7 / 0.8
us <- rmvnorm(N,c(0,0),matrix(c(1,0.7,0.7,1),2,2))
Xs <- rmvnorm(N,c(0,0),matrix(c(1,0.8,0.8,1),2,2))
Z <- Xs[,1]
X <- Xs[,2]
Sel<- Z + us[,1]>0       # Selection eq.
Y  <- X + us[,2]         # B0=0, B1=1
Yob<- ifelse(Sel==TRUE,Y,NA)  # Selected Y

# OLSs:

NoSel2<-lm(Y~X)      # all data
WithSel2<-lm(Yob~X)  # sample-selected data

# Plot that:

pdf("HeckmanExample2.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=1)
points(X,Yob,pch=20,cex=0.8,col="darkgreen")
abline(NoSel2,lwd=2)
abline(WithSel2,lwd=2,lty=2,col="darkgreen")
dev.off()

# Two-Step:

probit<-glm(Sel~Z,family=binomial(link="probit"))
IMR2<-((1/sqrt(2*pi))*exp(-((probit$linear.predictors)^2/2))) / 
  pnorm(probit$linear.predictors)

OLS2step2<-lm(Yob~X+IMR2)
summary(OLS2step2)

# FIML:

FIML2<-selection(Sel~Z,Y~X,method="ml")
summary(FIML2)

# Results:

y.labels<-c("X (true OLS = 1)","IMR","Constant (true = 0)")
cols<-c("OLS-All","OLS-Selected","Two-Stage","FIML")
stargazer(NoSel2,WithSel2,OLS2step2,FIML2,type="latex",
          covariate.labels=y.labels,
          column.labels=cols,model.names=FALSE,
          omit.stat=c("f","ser"),
          model.numbers=FALSE)


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Potential Outcomes...                              ####

Cov0<-data.frame(i=seq(1:6),
                 W=c(rep(0,3),rep(1,3)),
                 Y0=c(8,10,12,8,10,12),
                 Y1=c(10,12,14,10,12,14),
                 Ydiff=rep(2,6),
                 Y=c(8,10,12,10,12,14))

with(Cov0, t.test(Y~W))

PosCov<-data.frame(i=seq(1:6),
                 W=c(rep(0,3),rep(1,3)),
                 Y0=c(8,8,10,10,12,12),
                 Y1=c(10,10,12,12,14,14),
                 Ydiff=rep(2,6),
                 Y=c(8,8,10,12,14,14))

with(PosCov, t.test(Y~W))

NegCov<-data.frame(i=seq(1:6),
                   W=c(rep(0,3),rep(1,3)),
                   Y0=c(12,12,10,10,8,8),
                   Y1=c(14,14,12,12,10,10),
                   Ydiff=rep(2,6),
                   Y=c(12,12,10,12,10,10))

with(NegCov, t.test(Y~W))

# Not run:

Varying<-data.frame(i=seq(1:6),
                 W=c(rep(0,3),rep(1,3)),
                 Y0=c(8,10,12,8,10,12),
                 Y1=c(9,11,14,9,11,14),
                 Ydiff=c(1,2,3,1,2,3),
                 Y=c(8,10,12,9,11,14))

with(Varying, t.test(Y~W))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Bigger simulation (not in slides):

reps <- 1000 # number of sims
N<-1000      # sample size

ATE.U <- numeric(reps) # ATEs w/no confounding
ATE.C <- numeric(reps) # ATEs w/confounding by X
ATE.A <- numeric(reps) # Confounded + adjusted ATEs 

set.seed(7222009)

for(i in 1:reps) {
  W0 <- rep(0,N)
  W1 <- rep(1,N)
  Y0 <- 0 + 1*W0 + rnorm(N) # counterfactual Y for W=0; ATE=1
  Y1 <- 0 + 1*W1 + rnorm(N) # counterfactual Y for W=1; ATE=1
  X <- rnorm(N) # independent thing, for now
  W  <- numeric(N)
  Y  <- numeric(N)
  for(j in 1:N){
   Y[j] <- ifelse(X[j]<0,Y0[j],Y1[j])
   W[j] <- ifelse(X[j]<0,W0[j],W1[j])
   } # == effectively random selection of W, since Cov(X,Y)=0

  # boxplot(Y~W,ylim=c(-6,6))
  # t.test(Y~W) # works
  foo<-lm(Y~W) # no problem
  ATE.U[i] <- foo$coefficients[2] # stash the estimate
  # summary(lm(Y~W+X)) # also works; X is unnecessary
  #
  # Now make Y correlated with W **and** X:
  #
  W.cor  <- numeric(N)
  Y.cor  <- numeric(N)
  for(j in 1:N){
   Y.cor[j] <- ifelse(X[j]<0,Y0[j]+X[j],Y1[j]+X[j])
   W.cor[j] <- ifelse(Y.cor[j]==Y0[j]+X[j],W0[j],W1[j])
  } # Selection of W with Cov(X,Y)>0
  #
  # cor(X,W.cor)
  # cor(X,Y.cor)
  # boxplot(Y.cor~W.cor,ylim=c(-6,6))
  # t.test(Y.cor~W.cor) # wrong
  bar<-lm(Y.cor~W.cor) # bad upward bias; should be B~=1
  ATE.C[i] <- bar$coefficients[2] # stash the estimate
  baz<-lm(Y.cor~W.cor+X) # controlling for X fixes things
  ATE.A[i] <- baz$coefficients[2] # stash the estimate
}

# Plot the Betas!

pdf("ConfoundingSims.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(ATE.C),t="l",xlim=c(0.5,3),lwd=2,ylim=c(0,7),
     lty=2,col="red",main="",xlab="Estimated ATE")
lines(density(ATE.U),col="black",lwd=2,lty=1)
lines(density(ATE.A),col="darkgreen",lwd=3,lty=5)
abline(v=1,lty=3)
legend("topright",bty="n",lty=c(1,2,5),lwd=c(2,2,3),cex=0.8,
       col=c("black","red","darkgreen"),legend=c("No Confounding",
                      "Confounding w/o Adjustment",
                      "Confounding with Adjustment"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=