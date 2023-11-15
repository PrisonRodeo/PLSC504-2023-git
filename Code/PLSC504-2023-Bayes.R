#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries...                                   ####
#
# PLSC 504 -- Fall 2023
#
# Bayesian Statistics! (a micro-intro)
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","haven","psych","plyr","scales",
     "MASS","bayesm","bpr","cspp")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 2-3 times until you get "Package count: 9"
#
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also:
#
# setwd("~/Foo/Bar/Etc/")
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Note: Much of this is taken from Francisco Lima's 
# "Bayesian Models in R" blog post, at:
#
# https://www.r-bloggers.com/2019/05/bayesian-models-in-r-2/amp/
# 
# Frequentist vs. Bayes example...                     ####

rangeP <- seq(0, 1, length.out = 100)

pdf("FreqBinom.pdf",6,5)
par(mar=c(4,4,2,2))
plot(rangeP, dbinom(x=7,prob=rangeP,size=10),
     type="l",lwd=2,xlab="Pr(Heads)",ylab="Density")
abline(v=0.7,lwd=1,lty=2)
text(0.7,0.05,pos=2,cex=0.8,offset=0.3,
     labels="MLE = 0.7")
dev.off()

# Add a prior at Pr(Heads) = 0.5:

pdf("FreqBinom2.pdf",5,5)
par(mar=c(4,4,2,2))
plot(rangeP, dbinom(x=7,prob=rangeP,size=10),col="blue",
     type="l",lwd=2,xlab="Pr(Heads)",ylab="Density")
segments(0.7,0,0.7,0.265,lwd=1,lty=2,col="blue")
#text(0.7,0.05,col="red",pos=2,cex=0.8,offset=0.3,
#     labels="MLE = 0.7")
lines(rangeP,dnorm(x=rangeP,mean=.5,sd=.1)/16,
      lwd=2,col="cyan4")
segments(0.5,0,0.5,0.248,lty=3,col="cyan4")
legend("topleft",bty="n",col=c("cyan4","blue"),lwd=2,
       cex=0.8,legend=c("Prior","Likelihood | Data"))
dev.off()

# Add the posterior:

L<-dbinom(x=7,prob=rangeP,size=10)   # likelihood | data 
Prior<-dnorm(x=rangeP,mean=.5,sd=.1) # prior
Post<-L*Prior                        # unscaled posterior
Post2<-Post / 2.3                    # scaled posterior / density

pdf("FreqBinom3.pdf",5,5)
par(mar=c(4,4,2,2))
plot(rangeP, dbinom(x=7,prob=rangeP,size=10),col=alpha("blue",0.2),
     type="l",lwd=2,xlab="Pr(Heads)",ylab="Density")
segments(0.7,0,0.7,0.265,lwd=1,lty=2,col=alpha("blue",0.2))
#text(0.7,0.05,col="red",pos=2,cex=0.8,offset=0.3,
#     labels="MLE = 0.7")
lines(rangeP,dnorm(x=rangeP,mean=.5,sd=.1)/16,
      lwd=2,col=alpha("cyan4",0.2))
segments(0.5,0,0.5,0.248,lty=3,col=alpha("cyan4",0.2))
lines(rangeP,Post2,col="red",lwd=2)
segments(0.558,0,0.558,0.256,lty=3,col="red")
legend("topleft",bty="n",col=c(alpha("cyan4",0.2),alpha("blue",0.2),
                               "red"),lwd=2,cex=0.8,legend=c("Prior",
                               "Likelihood | Data","Posterior"))
dev.off()

# "Flat" priors:

pdf("FlatPriors.pdf",6,5)
par(mar=c(4,4,2,2))
plot(rangeP,dbinom(x=7,prob=rangeP,size=10),col=alpha("blue",0.2),
     type="l",lwd=3,xlab="Pr(Heads)",ylab="Density")
segments(0.7,0,0.7,0.265,lwd=1,lty=2,col="red")
segments(0,0.01,1,0.01,lwd=2,col=alpha("cyan4",0.2))
segments(0.5,0,0.5,0.3,lty=3,col=alpha("cyan4",0.2))
lines(rangeP,dbinom(x=7,prob=rangeP,size=10),col="red",lwd=1,lty=2)
legend("topleft",bty="n",col=c(alpha("cyan4",0.2),alpha("blue",0.2),
                               "red"),lwd=c(2,3,1),cex=0.8,lty=c(1,1,2),
       legend=c("(Flat) Prior","Likelihood | Data","Posterior"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MCMC "intuition" example...                        ####
#
# Taken from Kevin Ross, "An Introduction to Bayesian Reasoning 
# and Methods"
#
# https://bookdown.org/kevin_davisross/bayesian-reasoning-and-methods/mcmc.html

n_states = 5
theta = 1:n_states
pi_theta = c(.20,.05,.40,.10,.25)

n_steps = 1000 # 1000 visits
theta_sim = rep(NA, n_steps)
theta_sim[1] = 3 # initialize
set.seed(7222009)

for (i in 2:n_steps){
  current = theta_sim[i - 1]
  proposed = sample(c(current + 1, current - 1), size = 1, prob = c(0.5, 0.5))
  if (!(proposed %in% theta)){ # to correct for proposing moves outside of boundaries
    proposed = current
  }
  a = min(1, pi_theta[proposed] / pi_theta[current])
  theta_sim[i] = sample(c(proposed, current), size = 1, prob = c(a, 1-a))
}

# Plot of visits per island (1000 visits):

pdf("IslandHopping1.pdf",5,5)
par(mar=c(4,4,3,2))
plot(table(theta_sim)/n_steps,xlab="Island",ylab="Proportion of Days",
     ylim=c(0,0.4),main="1,000 Visits")
points(seq(1:5), pi_theta,pch=19,col="seagreen")
legend("topright",bty="n",pch=c(NA,19),col=c("black","seagreen"),
       cex=0.7,lty=c(1,NA),lwd=c(2,NA),legend=c("Actual Proportion",
                                        "Target Proportion"))
dev.off()

# Now do 10,000 visits...

n_states = 5
theta = 1:n_states
pi_theta = c(.20,.05,.40,.10,.25)

n_steps = 10000 # 10,000 visits
theta_sim = rep(NA, n_steps)
theta_sim[1] = 3 # initialize
set.seed(7222009)

for (i in 2:n_steps){
  current = theta_sim[i - 1]
  proposed = sample(c(current + 1, current - 1), size = 1, prob = c(0.5, 0.5))
  if (!(proposed %in% theta)){ # to correct for proposing moves outside of boundaries
    proposed = current
  }
  a = min(1, pi_theta[proposed] / pi_theta[current])
  theta_sim[i] = sample(c(proposed, current), size = 1, prob = c(a, 1-a))
}

# Plot of visits per island (10,000 visits):

pdf("IslandHopping2.pdf",5,5)
par(mar=c(4,4,3,2))
plot(table(theta_sim)/n_steps,xlab="Island",ylab="Proportion of Days",
     ylim=c(0,0.4),main="10,000 Visits")
points(seq(1:5), pi_theta,pch=19,col="seagreen")
legend("topright",bty="n",pch=c(NA,19),col=c("black","seagreen"),
       cex=0.7,lty=c(1,NA),lwd=c(2,NA),legend=c("Actual Proportion",
                                        "Target Proportion"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Bayesian regression models...                      ####
#
# Data from the CSPP at Michigan State...
#
# Source: (http://ippsr.msu.edu/public-policy/correlates-state-policy)
#
# Get + clean up data:

df<-get_cspp_data(vars=c("legcount_nsen_b","pctblack","total_expenditure",
                  "med_hhinc_int","poptotal"),years=seq(2013,2017))

DF<-data.frame(State=df$state,Year=df$year,
               BlackHouseMembers=df$legcount_nsen_b,
               PercentBlackPop=df$pctblack,
               TotalExpenditures=df$total_expenditure,
               MedianHHIncome=df$med_hhinc_int,
               Population=df$poptotal)
DF<-DF[complete.cases(DF),] # zap missing data to make life easier...
rm(df) # clean up...

# Reference: OLS...

OLSfit<-lm(BlackHouseMembers~PercentBlackPop+log(TotalExpenditures)+
               log(MedianHHIncome)+log(Population),data=DF)
summary(OLSfit)

# Same model, using -bayesm-...
# 
# Data:

DF$logTE<-log(DF$TotalExpenditures)
DF$logHHInc<-log(DF$MedianHHIncome)
DF$logPop<-log(DF$Population)
DF$One<-1  # constant...

# Model:

Data<-list(y=DF$BlackHouseMembers,X=as.matrix(DF[,c(11,4,8,9,10)]))
MCMC<-list(R=1e6,keep=10,nprint=0)

BayesFit<-runireg(Data=Data,Mcmc=MCMC)

# Summarize:

summary(BayesFit$betadraw)

# Plot posterior for Black Percentage of the Population:

B<-10000 # Discard 10K burn-in draws...
ND<-1e5  # Total kept draws after thinning...

pdf("LM-Beta-BlackPct.pdf",7,5)
par(mar=c(4,4,2,2))
hist(BayesFit$betadraw[B:ND,2],main="Posterior Distribution for Black Pop. Pct.",
     xlab="Beta")
abline(v=median(BayesFit$betadraw[B:ND,2]),lty=2,lwd=2,col="red")
dev.off()

# Alternative posterior + diagnostics plot:

devAskNewPage(ask = FALSE) # turn off interactive plotting

pdf("LM-Beta-BlackPct-2.pdf",10,5)
par(mar=c(4,4,2,2))
plot.bayesm.mat(BayesFit$betadraw[,2],names="Black Population",
                DEN=TRUE,INT=TRUE,TRACEPLOT=TRUE)


dev.off()

# Compare point estimates...

olsB<-OLSfit$coefficients[2:5]
bayesB<-colMeans(BayesFit$betadraw[,2:5])

pdf("BayesVsOLS.pdf",7,6)
par(mar=c(4,4,2,2))
plot(olsB,bayesB,pch=19,
     xlab="OLS Estimates",ylab="Bayesian Estimates")
text(olsB,bayesB,pos=c(2,4,4,2),cex=0.8,
     labels=c("Black Pop. Pct.","State Expenditures",
            "Median Income","Population"))
abline(a=0,b=1,lwd=1.5,lty=2)
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Next, fit negative binomials (overdispersed Poissons) to 
# the count data...
#
# First, using standard MLE:

NBfit<-glm.nb(BlackHouseMembers~PercentBlackPop+log(TotalExpenditures)+
             log(MedianHHIncome)+log(Population),data=DF,init.theta=4,
             maxit=1e4)
summary(NBfit)

# This suggests a Poisson would be just fine...

PoisFit<-glm(BlackHouseMembers~PercentBlackPop+log(TotalExpenditures)+
                  log(MedianHHIncome)+log(Population),data=DF,family=poisson)
summary(PoisFit)


# Bayesian version (using -bpr- package):

BayesPois<-sample_bpr(BlackHouseMembers~PercentBlackPop+log(TotalExpenditures)+
                      log(MedianHHIncome)+log(Population),data=DF,
                      iter=1e5,burnin=5e2,thin=5,perc_burnin=0)
summary(BayesPois)

# Some plots:

pdf("BayesianPoisson.pdf",9,6)
par(mar=c(3,3,3,3))
plot(BayesPois)


dev.off()

# Compare point estimates...

mleNB<-PoisFit$coefficients[2:5]
bayesNB<-colMeans(BayesPois$sim$beta)[2:5]

pdf("BayesVsMLE.pdf",7,6)
par(mar=c(4,4,2,2))
plot(mleNB,bayesNB,pch=19,
     xlab="MLE Estimates",ylab="Bayesian Estimates")
text(mleNB,bayesNB,pos=c(2,4,4,2),cex=0.8,
     labels=c("Black Pop. Pct.","State Expenditures",
              "Median Income","Population"))
abline(a=0,b=1,lwd=1.5,lty=2)
dev.off()

# \fin