####################################################
# PLSC 504 -- Fall 2022: Code for Intro / Review
# of likelihood and optimization... 
####################################################
# Load packages (run this a few times to make sure
# things take...):

P<-c("readr","maxLik","distr")

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

###################################
# Toy example (for slide figures):

data<-c(64,63,59,71,68) # Data
Lis<-dnorm(data,mean=68,sd=4) # Likelihoods for m=68,s=4
L68.4<-prod(Lis) # The likelihood (joint product) for m=68,s=4

Mus<-seq(62,68,by=0.1) # possible values of mu [62,68]
L<-numeric(length(Mus)) # a place to put the likelihoods

# Calculate likelihoods for different values of mu:

for (i in 1:length(Mus)) {
  L[i]<-prod(dnorm(data,mean=Mus[i],sd=4)) # using -dnorm-
}

# Plot:

pdf("SalaryLR.pdf",5,4)
plot(Mus,L,t="l",lwd=2,xlab=expression(hat(mu)),
     ylab="Likelihood")
dev.off()

# Log-L Plot:

lnL<-numeric(length(Mus)) # a place to put the lnLs

for (i in 1:length(Mus)) {
  lnL[i]<-sum(log(dnorm(data,mean=Mus[i],sd=4)))
}

pdf("SalarylnLR.pdf",5,4)
plot(Mus,lnL,t="l",lwd=2,xlab=expression(hat(mu)),
     ylab="Log-Likelihood")
dev.off()

##################################
# Optimization, etc.
#
# # Rayleigh density plot, b = {1,2,3}

X<-seq(0,5,by=.01)
B1<-(X/(1^2))*exp((-X^2)/(2*1^2)) # b=1
B2<-(X/(2^2))*exp((-X^2)/(2*2^2)) # b=2
B3<-(X/(3^2))*exp((-X^2)/(2*3^2)) # b=3

# Plot:

pdf("Notes/Rayleigh.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,B1,t="l",lwd=3,xlab="X",ylab="Density")
lines(X,B2,lwd=3,lty=2,col="green")
lines(X,B3,lwd=3,lty=3,col="red")
legend("topright",inset=0.05,legend=c("b=1","b=2","b=3"),
       lty=c(1,2,3),col=c("black","green","red"),
       lwd=3,cex=1.2,bty="n")
dev.off()

# Simulate some Rayleigh data:

set.seed(7222009)
U<-runif(100)
rayleigh<-3*sqrt(-2*log(1-U)) # b = 3
loglike <- function(param) {
  b <- param[1]
  ll <- (log(x)-log(b^2)) + ((-x^2)/(2*b^2))
  ll
}

# Fit...

x<-rayleigh
hatsNR <- maxLik(loglike, start=c(1))
summary(hatsNR)

# Comparing optimizers:

hatsBFGS <- maxLik(loglike, start=c(1),method="BFGS") # BFGS
hatsBHHH <- maxLik(loglike, start=c(1),method="BHHH") # BHHH
labels<-c("Newton-Raphson","BFGS","BHHH")

hats<-c(hatsNR$estimate,hatsBFGS$estimate,
        hatsBHHH$estimate) # Estimates
ses<-c(sqrt(-(1/(hatsNR$hessian))),
       sqrt(-(1/(hatsBFGS$hessian))),
       sqrt(-(1/(hatsBHHH$hessian)))) # SEs
its<-c(hatsNR$iterations,hatsBFGS$iterations,
       hatsBHHH$iterations) # Iterations

pdf("Notes/RayleighOptims.pdf",7,6)
par(mfrow=c(3,1))
dotchart(hats,labels=labels,groups=NULL,pch=16,
         xlab="Point Estimate")
dotchart(ses,labels=labels,pch=16,
         xlab="Estimated Standard Error")
dotchart(its,labels=labels,pch=16,
         xlab="Iterations to Convergence")
dev.off()

# Bad juju:

Y<-c(0,0,0,0,0,1,1,1,1,1)
X<-c(0,1,0,1,0,1,1,1,1,1) # No obs. where Y=1 and X=0
logL <- function(param) {
  b0<-param[1]
  b1<-param[2]
  ll<-Y*log(exp(b0+b1*X)/(1+exp(b0+b1*X))) + 
    (1-Y)*log(1-(exp(b0+b1*X)/(1+exp(b0+b1*X)))) # lnL for binary logit
  ll
} # Logit regression function

Bhat<-maxLik(logL,start=c(0,0))
summary(Bhat)

# fin