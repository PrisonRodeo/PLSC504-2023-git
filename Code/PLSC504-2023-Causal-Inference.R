#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro material...                                 ####
#
# PLSC 504 -- Fall 2023
#
# Models for Causal Inference with 
# Observational Data
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# set working directory 

setwd("~/Dropbox (Personal)/PLSC 504") # change as needed

# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","RCurl","plm","lme4","gtools","plyr",
     "texreg","statmod","corrplot","rgenoud",
     "MatchIt","Matching")

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
options(digits = 4) # show fewer decimal places

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simulations for balance:                          ####

set.seed(7222009)
N <- 10000
W <- c(rep(0,N/2),rep(1,N/2))
Xbal <- rnorm(N)
Xunbal <- rnorm(N,mean=(-1+(2*W)))
dd<-data.frame(W=W,Xbal=Xbal,Xunbal=Xunbal)

pdf("Notes/Balance.pdf",7,5)
par(mar=c(4,4,4,2))
par(mfrow=c(1,2))
# Balanced X:
plot(density(dd[dd$W==0,]$Xbal),lwd=2,main="Balanced X",
     xlab="X")
lines(density(dd[dd$W==1,]$Xbal),lwd=2,lty=2,col="red")
abline(v=mean(dd[dd$W==0,]$Xbal),lty=3)
abline(v=mean(dd[dd$W==1,]$Xbal),lty=3,col="red")
legend("topleft",bty="n",lwd=2,lty=c(1,2),col=c("black","red"),
       legend=c("W=0","W=1"),cex=0.9)
# Unbalanced X:
plot(density(dd[dd$W==0,]$Xunbal),lwd=2,main="Unbalanced X",
     xlab="X",xlim=c(-4,4))
lines(density(dd[dd$W==1,]$Xunbal),lwd=2,lty=2,col="red")
abline(v=mean(dd[dd$W==0,]$Xunbal),lty=3)
abline(v=mean(dd[dd$W==1,]$Xunbal),lty=3,col="red")
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simulations for overlap:                          ####

set.seed(7222009)
N <- 100
X <- rnorm(N,0,1) # Pre-treatment confounder
WO <- rbinom(N,1,pnorm(0.1*X)) # overlapping treatment
WP <- rbinom(N,1,pnorm(1*X)) # partial overlapping treatment
WN <- rbinom(N,1,pnorm(8*X)) # non-overlapping treatment
YO <- 3 + 2*WO + 1*X + rnorm(N)
YP <- 3 + 2*WP + 1*X + rnorm(N)
YN <- 3 + 2*WN + 1*X + rnorm(N)
df<-data.frame(X=X,YO=YO,YP=YP,YN=YN,WO=WO,WP=WP,WN=WN)

pdf("Notes/Overlap.pdf",8,5)
par(mfrow=c(1,3))
par(mar=c(4,4,4,2))
with(df, plot(X,YO,pch=16+WO,col=WO+1,main="Complete Overlap",
              xlab="X",ylab="Y"))
legend("topleft",bty="n",pch=c(16,17),col=c("black","red"),
       legend=c("W=0","W=1"),cex=1.2)
with(df, plot(X,YP,pch=16+WP,col=WP+1,main="Moderate Overlap",
              xlab="X",ylab="Y"))
with(df, plot(X,YN,pch=16+WN,col=WN+1,main="No Overlap",
              xlab="X",ylab="Y"))
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# RDD sim for figure:                               ####

N <- 100
set.seed(7222009)
A <- runif(N,0,100)
T <- ifelse(A>50,1,0)
Y <- 8 + 0.1*A + 10*T + rnorm(N,0,4)
drdd<-data.frame(Y=Y,T=T,A=A)
fit0<-lm(Y~A,data=drdd[drdd$T==0,])
fit1<-lm(Y~A,data=drdd[drdd$T==1,])

pdf("Notes/RDD.pdf",5,6)
plot(A,Y,pch=16+T,col=T+1,ylim=c(0,35),
     xlim=c(0,100))
abline(v=50,lty=3)
clip(0,50,0,35)
abline(fit0,lwd=2)
clip(50,100,0,35)
abline(fit1,lwd=2,col="red")
clip(0,100,0,35)
text(20,30,labels=c("T=0"))
text(80,5,labels=c("T=1"),col="red")
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SPORTS example!                                   ####

sports <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/HS-Sports.csv")

summary(sports)

# Correlation plot:

pdf("Notes/Sports-CorrPlot.pdf",6,5)
corrplot(cor(sports), method="number",
         number.cex=0.8,tl.col="black")
dev.off()

# Simple OLS-ish models:

Xs <- c("sports","fincome","ses","workage","female",
         "academic","remedial","advanced")
Model <- paste("grades", paste(Xs,collapse=" + "),sep="~")

with(sports, t.test(grades~sports))
summary(lm(Model,data=sports))

# Checking for covariate balance:

set.seed(7222009)
Sports.bal.pre<-MatchBalance(sports~fincome+ses+workage+female+
                             academic+remedial+advanced,
                             data=sports,nboots=1000,digits=3)

# Gather P-values for balance statistics:

PreMatch.Ps<-numeric(7)
for(i in 1:7){
  PreMatch.Ps[i]<-print(Sports.bal.pre$BeforeMatching[[i]]$p.value)
}

# Show them:

pdf("Notes/PreMatch-Balance.pdf",8,5)
par(mar=c(4,7,2,2))
dotchart(PreMatch.Ps,labels=Xs[2:8],xlim=c(0,1),pch=19,
         xlab="Balance: P-Value")
abline(v=0.05,lty=3)
dev.off()

# Now, matching...

# Exact matching:

M.exact <- matchit(sports~fincome+ses+workage+female+academic+
                   remedial+advanced,data=sports,
                   method="exact")
M.exact

# Output matched data:

sports.exact <- match.data(M.exact,group="all")
dim(sports.exact)

# Balance?

set.seed(7222009)
Exact.bal<-MatchBalance(sports~fincome+ses+workage+female+
                               academic+remedial+advanced,
                             data=sports.exact,nboots=1000,digits=3)

# Gather P-values for balance statistics:

Exact.Ps<-numeric(7)
for(i in 1:7){
  Exact.Ps[i]<-print(Exact.bal$BeforeMatching[[i]]$p.value)
}

# Show them:

pdf("Notes/Exact-Balance.pdf",8,5)
par(mar=c(4,7,2,2))
dotchart(PreMatch.Ps,labels=Xs[2:8],xlim=c(0,1),pch=19,
         xlab="Balance: P-Value")
points(Exact.Ps,seq(1:7),pch=17,col="darkgreen")
abline(v=0.05,lty=3)
dev.off()

# Propensity Scores:

PSfit <- glm(sports~fincome+ses+workage+female+academic+remedial+
               advanced,data=sports,family=binomial(link="logit"))
# summary(PSfit)

# Generate scores & check common support:

PS.df <- data.frame(PS = predict(PSfit,type="response"),
                    sports = PSfit$model$sports)

pdf("Notes/PScore-Support.pdf",8,5)
par(mfrow=c(1,2))
with(PS.df[PS.df$sports==0,],
     plot(density(PS),main="Non-Athletes",lwd=2,
          xlab="Propensity Score",xlim=c(0,1)))
with(PS.df[PS.df$sports==1,],
     plot(density(PS),main="Athletes",lwd=2,col="red",
          xlab="Propensity Score",xlim=c(0,1)))
dev.off()

# Matching with propensity scores:

M.prop <- matchit(sports~fincome+ses+workage+female+academic+
                    remedial+advanced,data=sports,
                  method="nearest")
summary(M.prop)

# Get the data:

sports.prop <- match.data(M.prop,group="all")

# Balance:

set.seed(7222009)
Prop.bal<-MatchBalance(sports~fincome+ses+workage+female+
                        academic+remedial+advanced,
                        data=sports.prop,nboots=1000,digits=3)

# Gather P-values for balance statistics:

Prop.Ps<-numeric(7)
for(i in 1:7){
  Prop.Ps[i]<-print(Prop.bal$BeforeMatching[[i]]$p.value)
}

# Show them:

pdf("Notes/Exact-Prop-Balance.pdf",8,5)
par(mar=c(4,7,2,2))
dotchart(PreMatch.Ps,labels=Xs[2:8],xlim=c(0,1),pch=19,
         xlab="Balance: P-Value")
points(Exact.Ps,seq(1:7),pch=17,col="darkgreen")
points(Prop.Ps,seq(1:7),pch=15,col="red")
abline(v=0.05,lty=3)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Genetic matching (not shown...)

M.genetic <- matchit(sports~fincome+ses+workage+female+academic+
                     remedial+advanced,data=sports,
                     method="genetic")
sports.genetic <- match.data(M.genetic,group="all")

# Re-fit the t-tests:

with(sports, t.test(grades~sports))$statistic # No matching
with(sports.exact, t.test(grades~sports))$statistic # Exact
with(sports.prop, t.test(grades~sports,paired=TRUE))$statistic # PS
with(sports.genetic, t.test(grades~sports))$statistic # Genetic

# Regressions (and a minimal table):

fit.lm <- lm(Model,data=sports)
fit.exact <- lm(Model,data=sports.exact)
fit.prop <- lm(Model,data=sports.prop)
fit.genetic <- lm(Model,data=sports.genetic)

regs <- texreg(l=list(fit.lm,fit.exact,fit.prop,fit.genetic),
               stars=c(0.05),caption=" ",
               custom.model.names=c("No Matching","Exact",
                                    "Propensity Score","Genetic"))

regs

# fin