#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries...                                   ####
#
# PLSC 504 -- Fall 2023
#
# Non-Continuous Response Panel Data models
# (including GEEs)
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","haven","psych","plyr","lme4","plm",
     "car","boot","gtools","texreg","statmod","pscl",
     "stargazer","sandwich","prais","nlme","tseries",
     "pcse","pgmm","dynpanel","panelView","OrthoPanels",
     "dotwhisker","performance","countrycode","jstable",
     "peacesciencer","corrplot","pglm","glmmML","bife",
     "censReg","geepack","fixest")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 12-15 times until you get "Package count = 34"
#
# ALSO:
# Download panelAR from the archive, then:
# install.packages("Downloads/panelAR_0.1.tar.gz", 
#                 type= "source", 
#                 repos= NULL)
library(panelAR)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# setwd("~/Foo/Bar/Etc/")
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Example: WDI (again) plus data on wars and things... ####
#
# Pull the WDI data...

WDI<-read_csv("https://github.com/PrisonRodeo/PLSC504-2023-git/raw/main/Data/WDI.csv")

# Add a "Cold War" variable:

WDI$ColdWar <- with(WDI,ifelse(Year<1990,1,0))

# Keep a numeric year variable (for -panelAR-):

WDI$YearNumeric<-WDI$Year

# summary(WDI)
#
# Add some political data... (note: this is why
# you loaded the -peacesciencer- package above):

create_stateyears(system="gw",
                  subset_years=c(1960:2021)) %>%
  add_ccode_to_gw() %>%
  add_ucdp_acd(type="intrastate", only_wars = FALSE) %>%
  add_ucdp_onsets() %>%
  add_democracy() -> DF

DF$Year<-DF$year
DF$ISO3<-countrycode(DF$gwcode,"gwn","iso3c")
nc<-ncol(DF)
nd<-nc-1
ne<-nc-2 # kludgey af

DF<-DF[,c(nc,nd,1:ne)] # order variables
DF<-DF[order(DF$ISO3,DF$Year),] # sort data

# Merge:

Data<-merge(WDI,DF,by=c("ISO3","Year"),
            all=TRUE)

Data<-Data[order(Data$ISO3,Data$Year),] # sort
rm(DF) # clean up

# Zap some missingness...

Data<-Data[is.na(Data$ISO3)==FALSE,]

# Make the data a panel dataframe:

Data<-pdata.frame(Data,index=c("ISO3","Year"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simulation: unit effects and binary outcomes ####

set.seed(7222009)

reps<-100
N<-100
T<-100
NT<-N*T

MSlogit<-matrix(data=NA,nrow=reps,ncol=3)
FElogit<-matrix(data=NA,nrow=reps,ncol=3)
MSClogit<-matrix(data=NA,nrow=reps,ncol=3)
FEClogit<-matrix(data=NA,nrow=reps,ncol=3)
FECprobit<-matrix(data=NA,nrow=reps,ncol=3)
REBs<-matrix(data=NA,nrow=reps,ncol=3)

for(i in 1:reps){
  
  alpha <- rnorm(N)
  alphas <- rep(alpha, 100)
  X <- rnorm(NT) # Uncorrelated X
  XCorr <- 0.5*alphas + 0.5*rnorm(NT) # X, alpha correlated
  D <- rbinom(NT,1,0.5) # binary predictor
  Ystar <- 0 + 1*X + 1*D + alphas # latent Y, Cov(X,alpha)=0
  YCstar <- 0 + 1*XCorr + 1*D + alphas # latent Y, Cov(X,alpha)>0
  
  Y <- rbinom(NT,1,plogis(Ystar))
  YC <-  rbinom(NT,1,plogis(YCstar))
  YPC <-  rbinom(NT,1,pnorm(YCstar))
  
  fool<-glm(Y~X+D,family="binomial")
  foolFE<-glm(Y~X+D+as.factor(alphas),family="binomial")
  foolC<-glm(YC~XCorr+D,family="binomial")
  foolCFE<-glm(YC~XCorr+D+as.factor(alphas),family="binomial")
  probitFEC<-glm(YPC~XCorr+D+as.factor(alphas),
                 family="binomial"(link="probit"))
  RE<-glmmML(YC~XCorr+D, family="binomial",
             cluster=as.factor(alphas))
  
  MSlogit[i,]<-fool$coefficients[1:3]
  FElogit[i,]<-foolFE$coefficients[1:3]
  MSClogit[i,]<-foolC$coefficients[1:3]  
  FEClogit[i,]<-foolCFE$coefficients[1:3]  
  FECprobit[i,]<-probitFEC$coefficients[1:3]
  REBs[i,]<-RE$coefficients[1:3]
  
}

pdf("BinaryPanelSimsUncorrAlphas.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(MSlogit[,2]),xlim=c(0.6,1.1),lwd=2,
     lty=3,col="red",main="",ylim=c(0,17),
     xlab="Estimated Betas (True value = 1.0)")
lines(density(FElogit[,2]),lwd=2)
abline(v=1,lty=2,lwd=2)
legend("topleft",lwd=2,col=c("black","red"),lty=c(1,3),
       legend=c("Fixed Effects","No Fixed Effects"),
       bty="n")
dev.off()

pdf("BinaryPanelSimsCorrAlphas.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(MSClogit[,2]),xlim=c(0.6,2.1),ylim=c(0,10),
     lwd=2,lty=3,col="red",main="",
     xlab="Estimated Betas (True value = 1.0)")
lines(density(FEClogit[,2]),lwd=2)
abline(v=1,lty=2,lwd=2)
legend("topright",lwd=2,col=c("black","red"),lty=c(1,3),
       legend=c("Fixed Effects","No Fixed Effects"),
       bty="n")
dev.off()

pdf("BinaryPanelSimsCorrProbitRE.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(REBs[,2]),xlim=c(0.8,1.4),ylim=c(0,10),
     lwd=2,lty=3,col="red",main="",
     xlab="Estimated Betas (True value = 1.0)")
lines(density(FEClogit[,2]),lwd=2)
abline(v=1,lty=2,lwd=2)
legend("topright",lwd=2,col=c("black","red"),lty=c(1,3),
       legend=c("Fixed Effects Logit","Random Effects Logit"),
       bty="n")
dev.off()

# Now; What if we do that last bit with T=5?

set.seed(7222009)

reps<-100
N<-100
T<-5
NT<-N*T

MSlogit<-matrix(data=NA,nrow=reps,ncol=3)
FElogit<-matrix(data=NA,nrow=reps,ncol=3)
MSClogit<-matrix(data=NA,nrow=reps,ncol=3)
FEClogit<-matrix(data=NA,nrow=reps,ncol=3)
FECprobit<-matrix(data=NA,nrow=reps,ncol=3)
REBs<-matrix(data=NA,nrow=reps,ncol=3)

for(i in 1:reps){
  
  alpha <- rnorm(N)
  alphas <- rep(alpha,T)
  X <- rnorm(NT) # Uncorrelated X
  XCorr <- 0.5*alphas + 0.5*rnorm(NT) # X, alpha correlated
  D <- rbinom(NT,1,0.5) # binary predictor
  Ystar <- 0 + 1*X + 1*D + alphas # latent Y, Cov(X,alpha)=0
  YCstar <- 0 + 1*XCorr + 1*D + alphas # latent Y, Cov(X,alpha)>0
  
  Y <- rbinom(NT,1,plogis(Ystar))
  YC <-  rbinom(NT,1,plogis(YCstar))
  YPC <-  rbinom(NT,1,pnorm(YCstar))
  
  fool<-glm(Y~X+D,family="binomial")
  foolFE<-glm(Y~X+D+as.factor(alphas),family="binomial")
  foolC<-glm(YC~XCorr+D,family="binomial")
  foolCFE<-glm(YC~XCorr+D+as.factor(alphas),family="binomial")
  probitFEC<-glm(YPC~XCorr+D+as.factor(alphas),
                 family="binomial"(link="probit"))
  RE<-glmmML(YC~XCorr+D, family="binomial",
             cluster=as.factor(alphas))
  
  MSlogit[i,]<-fool$coefficients[1:3]
  FElogit[i,]<-foolFE$coefficients[1:3]
  MSClogit[i,]<-foolC$coefficients[1:3]  
  FEClogit[i,]<-foolCFE$coefficients[1:3]  
  FECprobit[i,]<-probitFEC$coefficients[1:3]
  REBs[i,]<-RE$coefficients[1:3]
  
}

# Plot:

pdf("BinaryPanelSimsCorrProbitRET5.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(REBs[,2]),xlim=c(0,2.5),ylim=c(0,2.5),
     lwd=2,lty=3,col="red",main="",
     xlab="Estimated Betas (True value = 1.0)")
lines(density(FEClogit[,2]),lwd=2)
abline(v=1,lty=2,lwd=2)
legend("topright",lwd=2,col=c("black","red"),lty=c(1,3),
       legend=c("Fixed Effects Logit","Random Effects Logit"),
       bty="n")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Back to the WDI + other data...                ####
#
# Prep things:

Data$PopMillions<-Data$Population/1000000
Data$CivilWar<-Data$ucdpongoing
Data$OnsetCount<-Data$sumonset1
Data$POLITY<-(Data$polity2+10)/2
Data$POLITYSquared<-Data$POLITY^2
Data$PostColdWar<-1-Data$ColdWar

# Summary statistics on the variables we're using...

vars<-c("ISO3","Year","country","CivilWar","OnsetCount",
        "LandArea","PopMillions","UrbanPopulation",
        "GDPPerCapita","GDPPerCapGrowth","PostColdWar",
        "POLITY","POLITYSquared")

DF<-Data[,vars]

describe(DF,skew=FALSE)

# Make panel data:

DF<-pdata.frame(DF,index=c("ISO3","Year"))

# Variation in civil wars:

CW<-DF$CivilWar
CW.B<-Between(CW,effect="individual",na.rm=TRUE)
CW.W<-Within(CW,effect="individual",na.rm=TRUE)

# Pooled logit model of civil war:

Logit<-glm(CivilWar~log(LandArea)+log(PopMillions)+
             UrbanPopulation+log(GDPPerCapita)+
             GDPPerCapGrowth+PostColdWar+POLITY+
             POLITYSquared,data=DF,family="binomial")
summary(Logit)

# Fixed Effects logit model:

FELogit<-bife(CivilWar~log(LandArea)+log(PopMillions)+
                UrbanPopulation+log(GDPPerCapita)+
                GDPPerCapGrowth+PostColdWar+POLITY+
                POLITYSquared|ISO3,data=DF,model="logit")

summary(FELogit)

# Random Effects:

RELogit<-pglm(CivilWar~log(LandArea)+log(PopMillions)+
                UrbanPopulation+log(GDPPerCapita)+
                GDPPerCapGrowth+PostColdWar+POLITY+
                POLITYSquared|ISO3,data=DF,family=binomial,
              effect="individual",model="random")

summary(RELogit)

# A table:

texreg(list(Logit,FELogit,RELogit),
       custom.model.names=c("Logit","FE Logit","RE Logit"),
       custom.coef.names=c("Intercept","ln(Land Area)","ln(Population)",
                           "Urban Population","ln(GDP Per Capita)","GDP Growth",
                           "Post-Cold War","POLITY","POLITY Squared",
                           "Estimated Sigma"), 
       stars=0.05)


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Panel Data & Event counts...                       ####

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Event Counts                                  ####

xtabs(~DF$OnsetCount)

# Basic Poisson (no panel effects...)

Poisson<-glm(OnsetCount~log(LandArea)+log(PopMillions)+
               UrbanPopulation+log(GDPPerCapita)+
               GDPPerCapGrowth+PostColdWar+POLITY+
               POLITYSquared,data=DF,family="poisson")
summary(Poisson)


# Fixed effects Poisson:

FEPoisson<-pglm(OnsetCount~log(LandArea)+log(PopMillions)+
                  UrbanPopulation+log(GDPPerCapita)+
                  GDPPerCapGrowth+PostColdWar+POLITY+
                  POLITYSquared,data=DF,family="poisson",
                effect="individual",model="within")
summary(FEPoisson)

# alternative, using -fixest-:

FEPoisson2<-feglm(OnsetCount~log(LandArea)+log(PopMillions)+
                    UrbanPopulation+log(GDPPerCapita)+
                    GDPPerCapGrowth+PostColdWar+POLITY+
                    POLITYSquared|ISO3,data=DF,family="poisson")
summary(FEPoisson2,cluster="ISO3")

# Random effects Poisson

REPoisson<-glmer(OnsetCount~log(LandArea)+log(PopMillions)+
                   UrbanPopulation+log(GDPPerCapita)+
                   GDPPerCapGrowth+PostColdWar+POLITY+
                   POLITYSquared+(1|ISO3),data=DF,family="poisson")
summary(REPoisson)

# Another RE Poisson, using pglm (slightly different from the
# previous two):

REPoisson2<-pglm(OnsetCount~log(LandArea)+log(PopMillions)+
                   UrbanPopulation+log(GDPPerCapita)+
                   GDPPerCapGrowth+PostColdWar+POLITY+
                   POLITYSquared,data=DF,family="poisson",
                 effect="individual",model="random")
summary(REPoisson2)

# Can also do RE Poisson using glmmML...
#
# Basic / pooled negative binomial:

NegBin<-pglm(OnsetCount~log(LandArea)+log(PopMillions)+
               UrbanPopulation+log(GDPPerCapita)+
               GDPPerCapGrowth+PostColdWar+POLITY+
               POLITYSquared,data=DF,
             family="negbin",model="pooling")
summary(NegBin)

# Negative binomial with fixed effects:

FENegBin<-pglm(OnsetCount~log(LandArea)+log(PopMillions)+
                 UrbanPopulation+log(GDPPerCapita)+
                 GDPPerCapGrowth+PostColdWar+POLITY+
                 POLITYSquared,data=DF,family="negbin",
               effect="individual",model="within")
summary(FENegBin)

# (Can also use -fenegbin-...)
#
# Same, with random effects:

RENegBin<-glmer.nb(OnsetCount~log(LandArea)+log(PopMillions)+
                     UrbanPopulation+log(GDPPerCapita)+
                     GDPPerCapGrowth+PostColdWar+POLITY+
                     POLITYSquared+(1|ISO3),data=DF,
                   verbose=TRUE)
summary(RENegBin)

# Table?

texreg(list(Poisson,FEPoisson,REPoisson,NegBin,FENegBin,RENegBin),
       custom.model.names=c("Poisson","FE Poisson","RE Poisson",
                            "Neg. Bin.","FE N.B.","RE N.B."),
       custom.coef.names=c("Intercept","ln(Land Area)","ln(Population)",
                           "Urban Population","ln(GDP Per Capita)","GDP Growth",
                           "Post-Cold War","POLITY","POLITY Squared",
                           "Estimated Sigma"), 
       stars=0.05)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Generalized Estimating Equations (GEEs)            ####
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Back to the binary Civil War variable...
#
# Zap all the missingness, to make our life
# a little easier:

DF<-DF[complete.cases(DF),]

# Independence:

GEE.ind<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                  log(GDPPerCapita)+GDPPerCapGrowth+PostColdWar+POLITY+
                  POLITYSquared,data=DF,id=ISO3,family="binomial",
                corstr="independence")
summary(GEE.ind)

# Exchangeable correlation:

GEE.exc<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                  log(GDPPerCapita)+GDPPerCapGrowth+PostColdWar+POLITY+
                  POLITYSquared,data=DF,id=ISO3,family="binomial",
                corstr="exchangeable")
summary(GEE.exc)


# AR(1) correlation:

GEE.ar1<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                  log(GDPPerCapita)+GDPPerCapGrowth+PostColdWar+POLITY+
                  POLITYSquared,data=DF,id=ISO3,family="binomial",
                corstr="ar1")
summary(GEE.ar1)

# Unstructured correlation...
#
# This is a *bad* idea with a big T, so let's
# subset the data by extracting five semi-recent
# years and just looking at those:

DF$flag<-ifelse(as.numeric(as.character(DF$Year))<2018 & 
                  as.numeric(as.character(DF$Year))>2012,1,0)
DF5<-DF[DF$flag==1,]

# Fit the model (now removing -PostColdWar-):

GEE.unstr<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                    log(GDPPerCapita)+GDPPerCapGrowth+POLITY+
                    POLITYSquared,data=DF5,id=ISO3,family="binomial",
                  corstr="unstructured")
summary(GEE.unstr)

# Plot the betas / SEs:

betas<-cbind(GEE.ind$coefficients,GEE.exc$coefficients,
             GEE.ar1$coefficients,
             GEE.unstr$coefficients)
betas<-betas[-1,]

pdf("GEE-NewBetasR.pdf",7,7)
par(mar=c(4,4,2,2))
scatterplotMatrix(betas[-1,],smooth=FALSE,
                  var.labels=c("Indep.","Exch.","AR1","Unstr."),
                  pch=19,diagonal=FALSE)
dev.off()

# SEs:

ses<-cbind(sqrt(diag(GEE.ind$geese$vbeta)),
           sqrt(diag(GEE.exc$geese$vbeta)),
           sqrt(diag(GEE.ar1$geese$vbeta)),
           sqrt(diag(GEE.unstr$geese$vbeta)))
ses<-ses[-1,]

pdf("GEE-NewSEsR.pdf",7,7)
par(mar=c(4,4,2,2))
scatterplotMatrix(ses[-1,],smooth=FALSE,
                  var.labels=c("Ind","Exch","AR1","Unstr"),
                  pch=19,diagonal=FALSE,xlim=c())
dev.off()

# \fin