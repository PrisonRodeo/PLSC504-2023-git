#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro materials...                                   ####
#
# PLSC 504 -- Fall 2023
#
# Introduction to Time Series Analysis
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Set working directory (or not, whatever...)

setwd("~/Dropbox (Personal)/PLSC 504") # change as needed

# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","RCurl","gtools","texreg","lmtest","plyr",
     "zoo","tseries","forecast","dyn","egcm","urca")

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
options(digits = 2) # show fewer decimal places

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Time-series plot example: Democratic House membership,
# 1789-2019...

DCong <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/DCongPct.csv")

DCong$DemHousePct <- DCong$DemHousePct*100 # percentages
DCong$DemSenatePct <- DCong$DemSenatePct*100 # percentages

summary(DCong)

# Make it a time series:

DH.TS <- ts(DCong$DemHousePct,start=1789,end=2019,
            frequency=0.5)

# Time series plot:

pdf("DemHousePct.pdf",6,5)
par(mar=c(4,4,2,2))
plot(DH.TS, t="l",lwd=2,
     xlab="Congress",ylab="Percent Democratic")
abline(h=50,lwd=1,lty=2)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Basics...                                            ####
#
# Rho-Theta plot:

theta <- seq(from=-1,to=1,by=0.01)
rho <- theta / ((1 + theta^2))

pdf("MA-Series-Theta-Rho.pdf",6,5)
par(mar=c(4,4,2,2))
plot(theta, rho, t="l",lwd=2,
     xlab=expression(theta),ylab=expression(rho))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some simulations: ARIMA series, etc.
#
# Generate and plot an I(1) series:

set.seed(7222009)

T <- 200
I1 <- arima.sim(n=T,list(order=c(0,1,0)))

pdf("I1-Series.pdf",6,5)
par(mar=c(4,4,2,2))
plot(I1,ylab="Values of Y",lwd=2)
abline(h=0,lty=2)
dev.off()

# Same series, differenced:

I1D <- diff(I1)

pdf("I1-Series-Differenced.pdf",6,5)
par(mar=c(4,4,2,2))
plot(I1D,lwd=2,
     ylab=expression(paste("Values of ",Delta,"Y")))
abline(h=0,lty=2)
dev.off()

# Generate and plot some AR(1) series:

set.seed(7222009)
T <- 200

AR01 <- arima.sim(n=T,list(ar=c(-0.8),ma=c(0)))
AR05 <- arima.sim(n=T,list(ar=c(0.1),ma=c(0)))
AR09 <- arima.sim(n=T,list(ar=c(0.8),ma=c(0)))

pdf("AR-Series.pdf",6,5)
par(mar=c(2,4,4,2))
par(mfrow=c(3,1))
plot(AR01,lwd=2,ylab="Values of Y",
     main=expression(paste(phi," = -0.80")))
abline(h=0,lty=2)
#text()
plot(AR05,lwd=2,col="red",ylab="Values of Y",
     main=expression(paste(phi," = 0.10")))
abline(h=0,lty=2)
par(mar=c(4,4,4,2)) # reset X margin
plot(AR09,lwd=2,col="darkgreen",xlab="Time",ylab="Values of Y",
     main=expression(paste(phi," = 0.80")))
abline(h=0,lty=2)
dev.off()

# Generate and plot some MA(1) series:

set.seed(7222009)
T <- 200

MA01 <- arima.sim(n=T,list(ar=c(0),ma=c(-0.8)))
MA05 <- arima.sim(n=T,list(ar=c(0),ma=c(0.1)))
MA09 <- arima.sim(n=T,list(ar=c(0),ma=c(0.8)))

pdf("MA-Series.pdf",6,5)
par(mar=c(2,4,4,2))
par(mfrow=c(3,1))
plot(MA01,lwd=2,ylab="Values of Y",
     main=expression(paste(theta," = -0.80")))
abline(h=0,lty=2)
#text()
plot(MA05,lwd=2,col="red",ylab="Values of Y",
     main=expression(paste(theta," = 0.10")))
abline(h=0,lty=2)
par(mar=c(4,4,4,2)) # reset X margin
plot(MA09,lwd=2,col="darkgreen",xlab="Time",ylab="Values of Y",
     main=expression(paste(theta," = 0.80")))
abline(h=0,lty=2)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Compare some ARIMA series...

set.seed(7222009)
ar1 <- arima.sim(n=T,list(ar=c(0.9),ma=c(0)))
ma1 <- arima.sim(n=T,list(ar=c(0),ma=c(0.9)))
arma11 <- arima.sim(n=T,list(ar=c(0.9),ma=c(0.9)))
i1 <- arima.sim(n=T,list(order=c(0,1,0)))

pdf("ARIMAcomparison.pdf",7,6)
par(mar=c(4,4,2,2))
plot(i1,ylim=c(-11,22),lwd=2,ylab="Values of Y")
lines(ar1,lwd=2,col="red")
lines(ma1,lwd=2,col="blue")
lines(arma11,lwd=2,col="darkgreen")
abline(h=0,lwd=0.8,lty=2)
legend(150,22,legend=c("I(1)","AR(1)","MA(1)","ARMA(1,1)"),
       col=c("black","red","blue","darkgreen"),
       lwd=2,bty="n")
dev.off()

# ACFs and PACFs:
acfi1 <- acf(i1)
acfar1 <- acf(ar1)
acfma1 <- acf(ma1)
acfarma11 <- acf(arma11)

pacfi1 <- pacf(i1)
pacfar1 <- pacf(ar1)
pacfma1 <- pacf(ma1)
pacfarma11 <- pacf(arma11)

pdf("ARIMA-ACFs.pdf",8,6)
par(mfrow=c(2,2))
plot(acfi1,main="I(1) series")
plot(acfar1,main="AR(1) series")
plot(acfma1,main="MA(1) series")
plot(acfarma11,main="ARMA(1,1) series")
dev.off()

pdf("ARIMA-PACFs.pdf",8,6)
par(mfrow=c(2,2))
plot(pacfi1,main="I(1) series")
plot(pacfar1,main="AR(1) series")
plot(pacfma1,main="MA(1) series")
plot(pacfarma11,main="ARMA(1,1) series")
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Democratic Percent of Congress data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/DCongPct.csv")
DCong <- read.csv(text = url) 
rm(url)

DCong$DemHousePct <- DCong$DemHousePct*100 # percentages
DCong$DemSenatePct <- DCong$DemSenatePct*100 # percentages

summary(DCong)

# Make it a time series:

DH.TS <- ts(DCong$DemHousePct,start=1789,end=2019,
            frequency=0.5)

# Time series plot:

pdf("DemHousePct.pdf",6,5)
par(mar=c(4,4,2,2))
plot(DH.TS, t="l",lwd=2,
     xlab="Congress",ylab="Percent Democratic")
abline(h=50,lwd=1,lty=2)
dev.off()

# Plots of univariate ACFs and PACFs for the House data:

D.acf <- acf(DH.TS) # ACF

pdf("DemHouseACF.pdf",6,5)
par(mar=c(4,4,2,2))
plot(D.acf,main="")
dev.off()

D.pacf <- pacf(DH.TS) # PACF

pdf("DemHousePACF.pdf",6,5)
par(mar=c(4,4,2,2))
plot(D.pacf, main="")
dev.off()

# Fit ARIMA models:

DH.AR1 <- arima(DH.TS,order=c(1,0,0),method="ML") # AR(1)
summary(DH.AR1)

DH.ARMA11 <- arima(DH.TS,order=c(1,0,1),method="ML") # ARMA(1,1)
summary(DH.ARMA11)

# Model selection via LR test:

lrtest(DH.AR1,DH.ARMA11)

# Automated version:

DH.robot <- auto.arima(DH.TS)
summary(DH.robot)

# Plot residuals vs. time:

pdf("DH-ARIMA-residuals.pdf",6,3)
par(mar=c(4,4,2,2))
plot(DH.AR1$residuals, ylab="AR(1) residuals",
       lwd=2)
abline(h=0,lty=2)
dev.off()

# Box-Pierce and Ljung-Box tests:

Box.test(DH.AR1$residuals)
Box.test(DH.AR1$residuals,type="Ljung")

# Forecasting:

DH.forecast <- forecast(DH.AR1,h=20,model="Arima")

pdf("DH-forecast.pdf",7,6)
par(mar=c(4,4,2,2))
plot(DH.forecast, main="",lwd=2,xlab="Year",
     ylab="Democratic House Percentage",
     showgap=FALSE,col="black",fcol="red")
abline(h=50,lty=2)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# UNIT ROOTS...                                        ####
#
# Unit Root vs. trend:

T <- 200
time <- seq(from=1,to=T)

set.seed(7222009)
us <- rnorm(T)
UnitRoot <- cumsum(us)
Trend <- 0.05*time + us
UR.TS <- ts(UnitRoot,start=1,end=T)
Trend.TS <- ts(Trend,start=1,end=T)

pdf("URvsTrend.pdf",7,6)
par(mar=c(4,4,2,2))
plot(UR.TS,xlab="Time",ylab="Y",
     lwd=2,ylim=c(-12,12))
lines(Trend,col="red",lwd=2,lty=5)
abline(h=0,lty=2)
legend(1,-8,bty="n",legend=c("Unit Root","Trend"),
       lwd=2,col=c("black","red"), lty=c(1,5))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data (House and Senate votes, 1947-2013):

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/CongressVotes.csv")
Congress <- read.csv(text = url) 
rm(url)

HVotes.TS <- ts(Congress$HouseVotes,start=1947,end=2013,
                frequency=1)
SVotes.TS <- ts(Congress$SenateVotes,start=1947,end=2013,
                frequency=1)

# Plot:

pdf("CongressVotes.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HVotes.TS,xlab="Year",ylab="Number of Floor Votes",
     lwd=2)
lines(SVotes.TS,col="red",lwd=2,lty=5)
legend(1948,1200,bty="n",legend=c("U.S. House","U.S. Senate"),
       lwd=2,col=c("black","red"), lty=c(1,5))
dev.off()

# Various unit root tests...
#
# "old" D-F:

HDF<-ur.df(HVotes.TS,type="none",lags=0)
summary(HDF)

HDF.D<-ur.df(HVotes.TS,type="drift",lags=0)
summary(HDF.D)

HDF.T<-ur.df(HVotes.TS,type="trend",lags=0)
summary(HDF.T)

# plot the D-F object:

pdf("H-DF-Trend.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HDF.T)
dev.off()

# Augmented D-F tests:

HADF.T1<-ur.df(HVotes.TS,type="trend",lags=1)
summary(HADF.T1)

pdf("H-ADF-TrendL1.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HADF.T1)
dev.off()

HADF.BIC<-ur.df(HVotes.TS,type="trend",selectlags="BIC")
summary(HADF.BIC)

# Phillips-Perron tests:

HPP <- ur.pp(HVotes.TS, type="Z-tau",model="trend",lags="short")
summary(HPP)

pdf("H-PP.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HPP)
dev.off()

# KPSS test:

HKPSS <- ur.kpss(HVotes.TS,type="tau",lags="short")
summary(HKPSS)

pdf("H-KPSS.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HKPSS)
dev.off()

# Variance-Ratio test:

library(egcm)
H.VRatio <- bvr.test(HVotes.TS, detrend=TRUE)
H.VRatio

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# COINTEGRATION AND ECMs                               ####
#
# Simulating some cointegrated series...

T <- 150          # long-ish series
set.seed(7222009)
W <- cumsum(rnorm(T)) # the common bit - I(1)
X <- 2 + 0.8*W + 2*rnorm(T)
Y <- -2 + 0.8*W + 2*rnorm(T)

W.TS <- ts(W,start=1,end=T) # time series objects
X.TS <- ts(X,start=1,end=T)
Y.TS <- ts(Y,start=1,end=T)

pdf("CI-Series.pdf",7,6)
par(mar=c(4,4,2,2))
plot(X.TS,xlab="Time",ylab="X / Y",
     lwd=2,ylim=c(-12,12))
lines(Y.TS,col="red",lwd=2)
abline(h=0,lty=2)
legend(1,11,bty="n",legend=c("X","Y"),
       lwd=2,col=c("black","red"), lty=c(1,1))
dev.off()

# Test for unit roots in X and Y:

summary(ur.df(X.TS,type="trend",lags=1))
summary(ur.df(Y.TS,type="trend",lags=1))
summary(ur.kpss(X.TS,type="tau",lags="short"))
summary(ur.kpss(Y.TS,type="tau",lags="short"))

# Cointegrating regression:

CI.reg <- lm(X~Y)
# summary(CI.reg)
Zhats.TS <- ts(CI.reg$residuals,start=1,end=T)
summary(ur.df(Zhats.TS,type="trend",lags=1))
summary(ur.kpss(Zhats.TS,type="tau",lags="short"))

# Residual plot:

pdf("CI-Resids.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Zhats.TS,xlab="Time",ylab="Residuals Z",
     lwd=2,ylim=c(-8,8))
abline(h=0,lty=2)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Spurious regressions:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/Mustangs.csv")
Stangs <- read.csv(text = url) 
rm(url)

Y.TS<-ts(Stangs$price,start=1964,end=2019) # mustang prices
X.TS<-ts(Stangs$paraguay,start=1964,end=2019) # Paraguay's POLITY

pdf("Spurious.pdf",7,6)
par(mar=c(4,2,2,2))
plot(X.TS,xlab="Year",ylab=" ",
     lwd=2,yaxt="n")
par(new=T)
plot(Y.TS,xlab="Year",ylab=" ",col="red",
     lty=5,lwd=2,yaxt="n")
legend(1965,24000,bty="n",legend=c("X","Y"),
       lwd=2,col=c("black","red"), lty=c(1,1))
dev.off()

# Regressions:

summary(lm(Y.TS~X.TS))
summary(dyn$lm(Y.TS~lag(X.TS,-1)))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ECMs: Example

X<-c(10,10,10,20,10,10,10,10,10,12,14,16,18,20,20,20,20,20,20,20)
X.TS <- ts(X,start=1,end=length(X))
DX.TS<-ts(diff(X),start=2,end=length(X))

pdf("ECM-Example.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X.TS,xlab="Time",ylab="Values",
     lwd=2,ylim=c(-10,20))
lines(DX.TS,col="red",yaxt="n",xaxt="n",
      lty=5,lwd=2,xlim=c(1,20),xaxt="n")
legend(12,-4,bty="n",legend=c("X","Changes in X"),
       lwd=2,col=c("black","red"), lty=c(1,1))
dev.off()

DY1<-rep(12,times=length(X))
DY2<-rep(6,times=length(X))

for(i in 2:length(X)) {
        DY1[i] <- 1*DX.TS[i-1] - 0.8*(Y1[i-1] - 5 - 1*X[i-1])
        DY2[i] <- 0.25*DX.TS[i-1] - 0.2*(Y1[i-1] - 10 - 2*X[i-1])
}

DY1.TS <- ts(DY1, start=2,end=length(X))
DY2.TS <- ts(DY2, start=2,end=length(X))


pdf("ECM-Example2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(X.TS,xlab="Time",ylab="Values",
     lwd=3,ylim=c(0,25))
lines(DY1.TS,xlab="",ylab="",col="red",yaxt="n",
      lty=5,lwd=2,xaxt="n")
lines(DY2.TS,col="blue",yaxt="n",
      lty=3,lwd=2,xaxt="n")
legend(14,4,bty="n",legend=c("X","Y1","Y2"),
       lwd=c(3,2,2),col=c("black","red","blue"),lty=c(1,5,3))
dev.off()

# Fitting ECMs: Congressional vote data...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/CongressVotes.csv")
Congress <- read.csv(text = url) 
rm(url)

HVotes.TS <- ts(Congress$HouseVotes,start=1947,end=2013,
                frequency=1)
SVotes.TS <- ts(Congress$SenateVotes,start=1947,end=2013,
                frequency=1)

# Plot:

pdf("CongressVotes.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HVotes.TS,xlab="Year",ylab="Number of Floor Votes",
     lwd=2)
lines(SVotes.TS,col="red",lwd=2,lty=5)
legend(1948,1200,bty="n",legend=c("U.S. House","U.S. Senate"),
       lwd=2,col=c("black","red"), lty=c(1,5))
dev.off()

# Unit roots?

summary(ur.df(HVotes.TS,type="trend",lags=1))
summary(ur.df(SVotes.TS,type="trend",lags=1))
summary(ur.kpss(HVotes.TS,type="tau",lags="short"))
summary(ur.kpss(SVotes.TS,type="tau",lags="short"))

# Two-step ECM: Step one...

StepOne <- lm(SVotes.TS~HVotes.TS) # CI regression
summary(StepOne)

# Check residuals:

Zt.TS <- ts(StepOne$residuals,
            start=1947,end=2013) # make residual time series
summary(ur.df(Zt.TS,type="trend",lags=1))       # ADF
summary(ur.kpss(Zt.TS,type="tau",lags="short")) # KPSS

pdf("StepOneResiduals.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Zt.TS,xlab="Year",ylab="Step One Residuals",
     lwd=2)
abline(h=0,lty=2)
dev.off()

# Step Two:

DSV.TS <- diff(SVotes.TS)  # difference Y
DHVLag.TS <- lag(diff(HVotes.TS),k=-1) # Lag differenced X
Ztminus1.TS <- lag(Zt.TS,k=-1) # Lag residuals
df <- ts.intersect(DSV.TS,DHVLag.TS,Ztminus1.TS)

StepTwo <- lm(DSV.TS ~ DHVLag.TS + Ztminus1.TS, 
              data = df)
summary(StepTwo)

# One-Step ECM:

SVLag.TS <- lag(SVotes.TS,k=-1) # Lag Y
HVLag.TS <- lag(HVotes.TS,k=-1) # Lag X
df2 <- ts.intersect(DSV.TS,DHVLag.TS,SVLag.TS,HVLag.TS)
OneStep <- lm(DSV.TS~DHVLag.TS+SVLag.TS+HVLag.TS, 
              data=df2)
summary(OneStep)

# \fin