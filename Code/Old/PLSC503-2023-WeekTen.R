#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                   ####
#
# PLSC 503 -- Spring 2022: Code for Week Ten:
# Testing + Binary Response Models...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages:

P<-c("RCurl","readr","car","maxLik","mdscore","aod",
     "lmtest","stargazer","xtable")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Options:

options(scipen=999)
options(digits=4)
par(mar=c(4,4,2,2))

# Also: setwd() here...

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MLE/tests: COVID data example

COVID<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/COVID-PA.csv")

# log-lik function:

COVIDll <- function(param) {
  mu <- param[1]
  sigma <- param[2]
  ll <- -0.5*log(sigma^2) - (0.5*((x-mu)^2/sigma^2))
  ll
}

x<-log(COVID$CasesPer10K+0.055)

# What does that look like?

pdf("COVIDdensity.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(x),main="",lwd=2,
     xlab="log(COVID cases per 10K population)")
abline(v=0,lty=2)
dev.off()

# I left out the code for the cool map... sorry...
# Email me (zorn@psu.edu) if you want me to send it
# to you...
#
# Fit the model:

hats <- maxLik(COVIDll, start=c(0,1))
summary(hats)

# mean-only linear model for comparison:

COVIDLM<-lm(x~1)
summary(COVIDLM)

# components:

hats$estimate
hats$gradient
hats$hessian

-(solve(hats$hessian))
sqrt(-(solve(hats$hessian)))

# LR test...

COVIDllAlt <- function(param) {
  sigma <- param[1]
  ll <- -0.5*log(sigma^2) - (0.5*((x-0)^2/sigma^2))
  ll
}

hatsF <- maxLik(COVIDll, start=c(0,1))
hatsR <- maxLik(COVIDllAlt, start=c(1))

hatsF$maximum
hatsR$maximum

-2*(hatsR$maximum-hatsF$maximum)

pchisq(-2*(hatsR$maximum-hatsF$maximum),df=1,lower.tail=FALSE)

# Automatically:

# library(lmtest)
lrtest(hatsF,hatsR)

# Compare to Wald:

aod::wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:1,H0=0)

# Wald test (mu = sigma = 2):

aod::wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(2,2),verbose=TRUE)

# More Wald tests:

aod::wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(0,2))

aod::wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(-0.2,1.5))

# nonsensical:

aod::wald.test(Sigma=vcov(hats),b=coef(hats),Terms=2:2,H0=0)

# A nonsensical LR test of the same (insane) hypothesis...
#
# Attempt to fit the restricted model:

hatsDumb <- maxLik(COVIDll, start=c(mu=0,sigma=0),
                            fixed="sigma")
summary(hatsDumb)
lrtest(hatsF,hatsDumb)


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Binary response models...                             ####
#
# A picture of the LPM:

set.seed(7222009)
ystar<-rnorm(100) # 100 random N(0,1) draws
y<-ifelse(ystar>0,1,0) # Binary version of Y
x<-ystar+(0.5*rnorm(100)) # Create X from Y (w/slope=2)
data<-data.frame(ystar,y,x) # data frame

LPMfit<-lm(y~x)

pdf("LPM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,y,pch=20,main="",xlab="X",ylab="Y",
     ylim=c(-0.2,1.2))
abline(LPMfit,lwd=2.5)
dev.off()

# Residual plot:

pdf("LPMResids.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,LPMfit$residuals,pch=20,xlab="X",
     ylab="Residuals")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Standard normal / logistic

x<-seq(-5,5, by=0.01)
logistic<-dlogis(x)
lCDF<-plogis(x)
normal<-dnorm(x)
nCDF<-pnorm(x)

pdf("LogisticNormalPDFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,normal,xlab="u",t="l",lwd=2,lty=2,col="red",
     ylab="Probability")
lines(x,logistic,lwd=2)
legend("topright",bty="n",legend=c("Standard Logistic",
                                   "Standard Normal"),
       lwd=2,lty=c(1,2),col=c("black","red"))
abline(v=0,lty=3)
dev.off()

pdf("LogisticNormalCDFsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,nCDF,xlab="u",t="l",lwd=2,lty=2,col="red",
     ylab="Cumulative Probability")
lines(x,lCDF,lwd=2)
legend("bottomright",bty="n",legend=c("Standard Logistic",
                                      "Standard Normal"),
       lwd=2,lty=c(1,2),col=c("black","red"))
abline(v=0,lty=3)
dev.off()

# Odds and log-odds:

Z <- seq(0.02,0.98,by=0.001)
IZ <- 1 - Z
O = Z / IZ
lnO = log(O)

pdf("LogOddsR.pdf",8,5)
par(mar=c(5,4,2,2))
par(mfrow=c(1,2))
plot(O,Z,ylab="Probability of Z",t="l",lwd=2,
     xlab="Odds of Z: [P(Z) / (1-P(Z))]",
     ylim=c(0,1),main="Odds")
abline(h=c(0,1),lty=3)
plot(lnO,Z,ylab="Probability of Z",t="l",lwd=2,
     xlab="Log-Odds of Z",
     ylim=c(0,1),main="Log-Odds")
abline(h=c(0,1),lty=3)
dev.off()

# C-log-log

logit<-lCDF
cloglog1 <- 1 - exp(-exp(x))
cloglogneg1 <- 1 - exp(-exp(-x))

pdf("CLogLogCDFsR.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(5,4,2,2))
plot(x,lCDF,xlab="XB",t="l",lwd=2,lty=1,col="black",
     ylab="Pr(Y=1)")
lines(x,cloglog1,lwd=2,lty=2,col="red")
lines(x,cloglogneg1,lwd=2,lty=4,col="blue")
legend(1,0.6,bty="n",legend=c("Standard Logit (B=1)",
                              "C-Log-Log (B=1)",
                              "C-Log-Log (B=-1)"),
       lwd=2,lty=c(1,2,4),col=c("black","red","blue"),
       cex=0.8)
dev.off()

# # Scobit plot... [eyeroll]
# 
# # Deleted because seriously nobody has ever
# # fit a scobit model IRL...
# 
# SCOOBY <- 1 / ((1 + exp(-2*x))^(1))
# SCOOBY05 <- 1 / ((1 + exp(-2*x))^(0.5))
# SCOOBY02 <- 1 / ((1 + exp(-2*x))^(0.2))
# SCOOBY4 <- 1 / ((1 + exp(-2*x))^(4))
# 
# pdf("ScobitCDFsR.pdf",6,5)
# par(mar=c(5,4,2,2))
# plot(x,SCOOBY,xlab="XB",t="l",lwd=2,lty=1,col="black",
#      ylab="Pr(Y=1)")
# lines(x,SCOOBY05,lwd=2,lty=2,col="red")
# lines(x,SCOOBY02,lwd=2,lty=4,col="blue")
# lines(x,SCOOBY4,lwd=2,lty=5,col="orange")
# abline(v=0,lty=3)
# legend("bottomright",bty="n",legend=c("alpha = 1 (logit)",
#                                       "alpha = 0.5","alpha = 0.2","alpha = 4"),
#        lwd=2,lty=c(1,2,4,5),col=c("black","red","blue","orange"),
#        cex=0.8)
# dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Toy logit / probit example:

set.seed(7222009)
ystar<-rnorm(100,0.5,0.5)
y<-ifelse(ystar>0.5,1,0)
x<-ystar+(0.5*rnorm(100))
data<-data.frame(ystar,y,x)
head(data)

pdf("Notes/YstarYX-R.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,ystar,pch=19,ylab="Y* / Y",xlab="X")
points(x,y,pch=4,col="red")
abline(h=c(0,0.5,1),lty=c(1,2,1))
legend("topleft",bty="n",pch=c(19,4),col=c("black","red"),
       legend=c("Y*","Y"))
dev.off()

# Probits and logits and c-log-logs...

myprobit<-glm(y~x,family=binomial(link="probit"),
              data=data)
summary(myprobit)

mylogit<-glm(y~x,family=binomial(link="logit"),
             data=data)
summary(mylogit)

mycloglog<-glm(y~x,family=binomial(link="cloglog"),
               data=data)
summary(mycloglog)

pdf("Notes/LogitProbitHats.pdf",5,5)
par(mar=c(5,4,2,2))
plot(mylogit$fitted.values,myprobit$fitted.values,
     pch=20,xlab="Logit Predictions",
     ylab="Probit Predictions")
abline(a=0,b=1,lty=1,col="red")
dev.off()

# Reverse the outcome:

data$newY <- 1 - data$y

RP<-glm(newY~x,family=binomial(link="probit"),data=data)
RL<-glm(newY~x,family=binomial(link="logit"),data=data)
RC<-glm(newY~x,family=binomial(link="cloglog"),data=data)

# Gather coefficients & print:

Bs<-matrix(c(coef(myprobit)[1],coef(mylogit)[1],coef(mycloglog)[1],
      coef(RP)[1],coef(RL)[1],coef(RC)[1],coef(myprobit)[2],
      coef(mylogit)[2],coef(mycloglog)[2],coef(RP)[2],coef(RL)[2],
      coef(RC)[2]),nrow=3,ncol=4)

print(xtable(Bs))

