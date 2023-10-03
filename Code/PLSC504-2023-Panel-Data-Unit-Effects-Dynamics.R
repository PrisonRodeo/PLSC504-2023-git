#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PLSC 504 -- Fall 2023
#
# Panel / TSCS data: Unit effects +dynamics...
#
# Preamble...                                        ####
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                               ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","haven","psych","plyr","lme4","plm",
     "car","boot","gtools","texreg","statmod","pscl",
     "stargazer","sandwich","prais","nlme","tseries",
     "pcse","pgmm","dynpanel","panelView","OrthoPanels",
     "dotwhisker","performance")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 9-10 times until you get "Package count = 24"
#
# ALSO:
#
# library(panelAR) # This package is currently (10/3/23) archived; see
# https://stackoverflow.com/questions/71664861/r-package-panelar-not-available-to-install
# for installation instructions.

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Unit Effects...                                    ####
#
# FE plot:

i<-1:4
t<-20
NT<-t*max(i)
set.seed(7222009)
df<-data.frame(i=rep(i,t),
               t=rep(1:t,max(i)),
               X=runif(NT))
df$Y=1+2*i+4*df$X+runif(NT)
df<-df[order(df$i,df$t),] # sort

pdf("FEIntuition.pdf",7,6)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=i+14,col=i,
              xlim=c(0,1),ylim=c(3,14)))
abline(a=3.5,b=4,lwd=2,lty=1,col=1)
abline(a=5.5,b=4,lwd=2,lty=2,col=2)
abline(a=7.5,b=4,lwd=2,lty=3,col=3)
abline(a=9.5,b=4,lwd=2,lty=4,col=4)
legend("bottomright",bty="n",col=1:4,lty=1:4,
       lwd=2,legend=c("i=1","i=2","i=3","i=4"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# World Development Indicators (WDI) data      ####
#
# The WDI data we'll be using for this class can
# be accessed by running the "WDI-MakeData-2023.R"
# script found on the Github repository. You can 
# modify that script to add additional variables
# if you choose to. The data are also available
# in ready-to-use format in the "Data" folder
# on the Github repo, which is what we'll use here.
#
# Get the data:

wdi<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/WDI.csv")

# Add a "Cold War" variable:

wdi$ColdWar <- with(wdi,ifelse(Year<1990,1,0))

# Summarize:

psych::describe(wdi,fast=TRUE,ranges=FALSE,check=TRUE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Visualizing WDI data...                           ####

pdf("PanelWBLIViz.pdf",7,5)
panelview(WomenBusLawIndex~1,data=wdi,theme.bw=TRUE,
          outcome.type="continuous",type="outcome",
          by.timing=TRUE,index=c("ISO3","Year"),
          main=" ",ylab="Women's Business Law Index",
          legendOff=TRUE)
dev.off()

pdf("PanelPLeaveViz.pdf",7,5)
panelview(WomenBusLawIndex~PaidParentalLeave,data=wdi,theme.bw=TRUE,
          by.timing=FALSE,index=c("ISO3","Year"),
          color=c("orange","darkgreen"),
          legend.labs=c("No Paid Leave","Paid Leave"),
          main=" ",ylab="Country Code",axis.lab.gap=c(5,5),
          background="white")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Variation: Total, within, and between             ####
#
# Create "panel series"...

WDI<-pdata.frame(wdi)
WBLI<-WDI$WomenBusLawIndex
class(WBLI)

describe(WBLI,na.rm=TRUE) # all variation

pdf("WBLIAll.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(WDI$WomenBusLawIndex,na.rm=TRUE),
     main="",xlab="WBL Index",lwd=2)
abline(v=mean(WDI$WomenBusLawIndex,na.rm=TRUE),
       lwd=1,lty=2)
dev.off()

# "Between" variation:

describe(plm::between(WBLI,effect="individual",na.rm=TRUE)) # "between" variation

WBLIMeans<-plm::between(WBLI,effect="individual",na.rm=TRUE)

WBLIMeans<-ddply(WDI,.(ISO3),summarise,
                 WBLIMean=mean(WomenBusLawIndex,na.rm=TRUE))

pdf("WBLIBetween.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(WBLIMeans,na.rm=TRUE),
     main="",xlab="Mean WBLI",lwd=2)
abline(v=mean(WBLIMeans,na.rm=TRUE),
       lwd=1,lty=2)
dev.off()

# "Within" variation:

describe(Within(WBLI,na.rm=TRUE)) # "within" variation

pdf("WBLIWithin.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(Within(WBLI,na.rm=TRUE),na.rm=TRUE),
     main="",xlab="WBLI: Within-Country Variation",
     lwd=2)
abline(v=0,lty=2)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Regression! One-Way Unit Effects                  ####

# Subset (just for descriptives):

vars<-c("WomenBusLawIndex","PopGrowth","UrbanPopulation","FertilityRate",
        "GDPPerCapita","NaturalResourceRents","ColdWar")
subset<-WDI[vars]
subset<-subset[complete.cases(subset),] # listwise deletion
subset$lnGDPPerCap<-log(subset$GDPPerCapita)
subset$GDPPerCapita<-NULL
describe(subset,fast=TRUE)

# Pooled OLS:

OLS<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
           log(GDPPerCapita)+NaturalResourceRents+ColdWar, 
         data=WDI,model="pooling")

summary(OLS)

# "Fixed" / within effects:

FE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
          log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
        effect="individual",model="within")

summary(FE)

# Make a table:

Table1 <- stargazer(OLS,FE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX11.tex")

# Time-period Fixed Effects:

FE.Time<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
               log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
             effect="time",model="within")

# summary(FET)

# A comparison table:

FE.Units <- FE

CompFETable <- stargazer(FE.Units,FE.Time,
                         title="FE Models of WBLI (Units vs. Time)",
                         column.separate=c(1,1),align=TRUE,
                         dep.var.labels.include=FALSE,
                         dep.var.caption="",
                         covariate.labels=c("Population Growth","Urban Population",
                                            "Fertility Rate","ln(GDP Per Capita)",
                                            "Natural Resource Rents","Cold War"),
                         header=FALSE,model.names=FALSE,
                         model.numbers=FALSE,multicolumn=FALSE,
                         object.names=TRUE,notes.label="",
                         out="FEComp1.tex")

# Test for \alpha_i = 0:

pFtest(FE,OLS)
plmtest(FE,effect=c("individual"),type=c("bp"))
plmtest(FE,effect=c("individual"),type=c("kw"))

pFtest(FE.Time,OLS)
plmtest(FE.Time,effect=c("time"),type=c("bp"))
plmtest(FE.Time,effect=c("time"),type=c("kw"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Interpretation...

with(WDI, sd(UrbanPopulation,na.rm=TRUE)) # all variation

WDI<-ddply(WDI, .(ISO3), mutate,
           UPMean = mean(UrbanPopulation,na.rm=TRUE))
WDI$UPWithin<-with(WDI, UrbanPopulation-UPMean)

with(WDI, sd(UPWithin,na.rm=TRUE)) # "within" variation

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Between effects:

BE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
          log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
        effect="individual",model="between")

summary(BE)

Table2 <- stargazer(OLS,FE,BE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX21.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Random effects:

RE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
          log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
        effect="individual",model="random")

summary(RE)

Table3 <- stargazer(OLS,FE,BE,RE,
                    title="Models of WBLI",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX31.tex")

# Hausman test:

phtest(FE, RE)  # ugh...

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# A bit of HLMs...                                  ####

AltRE<-lmer(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
              log(GDPPerCapita)+NaturalResourceRents+ColdWar+(1|ISO3),
            data=WDI)

summary(AltRE)

# Are they the same?

TableHLM <- stargazer(RE,AltRE,
                      title="RE and HLM Models of WBLI",
                      column.separate=c(1,1,1,1),align=TRUE,
                      dep.var.labels.include=FALSE,
                      dep.var.caption="",
                      covariate.labels=c("Population Growth","Urban Population",
                                         "Fertility Rate","ln(GDP Per Capita)",
                                         "Natural Resource Rents","Cold War"),
                      header=FALSE,model.names=FALSE,
                      model.numbers=FALSE,multicolumn=FALSE,
                      object.names=TRUE,notes.label="",
                      out="HLMTable1.tex")

# ... yes. More or less.
#
# More HLM fun...

HLM1<-lmer(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
             log(GDPPerCapita)+NaturalResourceRents+ColdWar+(ColdWar|ISO3),
           data=WDI,control=lmerControl(optimizer="bobyqa"))

summary(HLM1)


# Testing:

anova(AltRE,HLM1)
VarCorr(HLM1)

# Get some of those sweeeet random slopes:

Bs<-data.frame(coef(HLM1)[1])

head(Bs)
mean(Bs$ISO3..Intercept.)
mean(Bs$ISO3.ColdWar)


pdf("WBLI-RandomIntercepts.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(density(ISO3..Intercept.),lwd=3,
              main="",xlab="Intercept Values"))
abline(v=mean(Bs$ISO3..Intercept.),lty=2)
dev.off()

pdf("WBLI-CWRandomSlopes.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(density(ISO3.ColdWar),lwd=3,
              main="",xlab="Cold War Slopes"))
abline(v=mean(Bs$ISO3.ColdWar),lty=2)
dev.off()

REcorr<-with(Bs,cor(ISO3..Intercept.,ISO3.ColdWar))

pdf("WBLI-HLMScatter.pdf",6,5)
par(mar=c(4,4,2,2))
with(Bs, plot(ISO3..Intercept.,ISO3.ColdWar,
              pch=19,main="",xlab="Intercept Values",
              ylab="Cold War Slopes"))
text(-90,-25,cex=1.2,
     labels=paste0("r = ",round(REcorr,3)))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Separating "within" and "between" effects...      ####
#
# WBLI and Natural Resources:

WDI<-ddply(WDI,.(ISO3),mutate,
           NRR.Between=mean(NaturalResourceRents,na.rm=TRUE))
WDI$NRR.Within<- (WDI$NaturalResourceRents - WDI$NRR.Between) 

WEBE.OLS<-lm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
               log(GDPPerCapita)+NRR.Within+NRR.Between+ColdWar,
             data=WDI)

summary(WEBE.OLS)

# Nice table:

Table4 <- stargazer(WEBE.OLS,
                    title="BE + WE Model of WBLI",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Within-Country Nat. Resource Rents",
                                       "Between-Country Nat. Resource Rents",
                                       "Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="WEBE1.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Two-way unit effects...                           ####

TwoWayFE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
              effect="twoway",model="within")

summary(TwoWayFE)


# Testing...
#
# Two-way effects:

pFtest(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
         log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
       effect="twoway",model="within")
plmtest(TwoWayFE,c("twoways"),type=("kw"))

# One-way effects in the two-way model:

plmtest(TwoWayFE,c("individual"),type=("kw"))
plmtest(TwoWayFE,c("time"),type=("kw"))

# Equivalence to -lm-:

TwoWayFE.BF<-lm(WomenBusLawIndex~PopGrowth+UrbanPopulation+
                  FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                  factor(ISO3)+factor(Year),data=WDI)

summary(TwoWayFE.BF)

# Two-way random effects:

TwoWayRE<-plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,data=WDI,
              effect="twoway",model="random")

summary(TwoWayRE)

# Here's a nicer table:

Table5 <- stargazer(OLS,FE,BE,RE,TwoWayFE,TwoWayRE,
                    title="Models of WBLI",
                    column.separate=c(1,1,1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("Population Growth","Urban Population",
                                       "Fertility Rate","ln(GDP Per Capita)",
                                       "Natural Resource Rents","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    column.sep.width="1pt",
                    omit.stat=c("f"),out="UFX51.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# DYNAMICS!                                         ####
#
# # Pull the WDI data (again; don't ask...):

WDI<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/WDI.csv")

# Add a "Cold War" variable:

WDI$ColdWar <- with(WDI,ifelse(Year<1990,1,0))

# Keep a numeric year variable (for -panelAR-):

WDI$YearNumeric<-WDI$Year

# summary(WDI)
#
# Make the data a panel dataframe:

WDI<-pdata.frame(WDI,index=c("ISO3","Year"))

# Summary statistics:

vars<-c("WomenBusLawIndex","PopGrowth","UrbanPopulation","FertilityRate",
        "GDPPerCapita","NaturalResourceRents","ColdWar")
subset<-WDI[vars]
subset<-subset[complete.cases(subset),] # listwise deletion
subset$lnGDPPerCap<-log(subset$GDPPerCapita)
subset$GDPPerCapita<-NULL
describe(subset,fast=TRUE)

# How much autocorrelation in those variables?

PG<-pdwtest(PopGrowth~1,data=subset)
UP<-pdwtest(UrbanPopulation~1,data=subset)
FR<-pdwtest(FertilityRate~1,data=subset)
GDP<-pdwtest(lnGDPPerCap~1,data=subset)
NRR<-pdwtest(NaturalResourceRents~1,data=subset)
CW<-pdwtest(ColdWar~1,data=subset)

rhos<-data.frame(Variable=c("Population Growth","Urban Population",
                            "Fertility Rate","GDP Per Capita",
                            "Natural Resource Rents","Cold War"),
                 Rho = c(1-(PG$statistic/2),1-(UP$statistic/2),
                         1-(FR$statistic/2),1-(GDP$statistic/2),
                         1-(NRR$statistic/2),1-(CW$statistic/2)))

stargazer(rhos,summary=FALSE,out="Rhos.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Panel unit root tests...                    ####
#
# Data:

WBLI<-data.frame(ISO3=WDI$ISO3,Year=WDI$Year,
                 WBLI=WDI$WomenBusLawIndex)
WBLI<-na.omit(WBLI)  # remove missing
WBLI<-pdata.frame(WBLI,index=c("ISO3","Year")) # panel data
WBLI.W<-data.frame(split(WBLI$WBLI,WBLI$ISO3)) # "wide" data

purtest(WBLI.W,exo="trend",test="levinlin",pmax=2)
purtest(WBLI.W,exo="trend",test="hadri",pmax=2)
purtest(WBLI.W,exo="trend",test="madwu",pmax=2)
purtest(WBLI.W,exo="trend",test="ips",pmax=2)

# Gather the statistics:

ur1<-purtest(WBLI.W,exo="trend",test="levinlin",pmax=2)
ur2<-purtest(WBLI.W,exo="trend",test="hadri",pmax=2)
ur3<-purtest(WBLI.W,exo="trend",test="madwu",pmax=2)
ur4<-purtest(WBLI.W,exo="trend",test="ips",pmax=2)

urs<-matrix(nrow=4,ncol=5)
for(i in 1:4){
  nom<-get(paste0("ur",i))
  urs[i,1]<-nom$statistic$method
  urs[i,2]<-nom$statistic$alternative
  urs[i,3]<-names(nom$statistic$statistic)
  urs[i,4]<-round(nom$statistic$statistic,3)
  urs[i,5]<-round(nom$statistic$p.value,4)
}

urs<-data.frame(urs)
colnames(urs)<-c("Test","Alternative","Statistic",
                 "Estimate","P-Value")
URTests<-stargazer(urs,summary=FALSE,
                   title="Panel Unit Root Tests: WBRI",
                   out="URTests.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some regression models, with dynamics... ####
#
# Lagged -dependent-variable model:

WDI$WBLI.L <- plm::lag(WDI$WomenBusLawIndex,n=1) # be sure to use the
# -plm- version of -lag-

LDV.fit <- lm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
                FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                ColdWar,data=WDI)

FD.fit <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,
              data=WDI,effect="individual",model="fd")

FE.fit <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                log(GDPPerCapita)+NaturalResourceRents+ColdWar,
              data=WDI,effect="individual",model="within")

LDV.FE.fit <- plm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
                    FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                    ColdWar,data=WDI,effect="individual",model="within")

# Next: Arellano-Bond model. Do not run, unless
# you are young, and have lots of time on your
# hands...

AB.fit<-pgmm(WomenBusLawIndex~WBLI.L+PopGrowth+UrbanPopulation+
               FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
               ColdWar|lag(WomenBusLawIndex,2:20),data=WDI,
             effect="individual",model="twosteps")

# Table:

texreg(list(LDV.fit,FD.fit,FE.fit,LDV.FE.fit),
       custom.model.names=c("Lagged Y","First Difference","FE","Lagged Y + FE"),
       custom.coef.names=c("Intercept","Lagged WBLI",
                           "Population Growth","Urban Population",
                           "Fertility Rate","ln(GDP Per Capita)",
                           "Natural Resource Rents","Cold War"),
       digits=3,stars=0.05)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Trend things...                               ####
#
# Trend illustration simulation:

set.seed(2719)
Tobs<-40 
X<-cumsum(rnorm(Tobs))+5
u<-rnorm(T,0,2)
T<-1:Tobs
Y<-10+X+u
Yt<-5+X+0.5*T+u

pdf("TrendPlot.pdf",5,6)
par(mar=c(4,4,2,2))
plot(T,Yt,t="l",lwd=2,lty=2,ylim=c(0,35),
     ylab="Y",col="blue")
lines(T,Y,lty=3,lwd=2,col="orange")
lines(T,X,lwd=2,col="black")
legend("topleft",bty="n",lwd=2,lty=c(2,3,1),
       col=c("blue","orange","black"),
       legend=c("Y2","Y1","X"))
dev.off()

# Regressions:

f1<-lm(Y~X)
f2<-lm(Yt~X)
f3<-lm(Yt~X+T)

stargazer(f1,f2,f3,omit.stat="f")

# Make a better trend variable in the WDI data:

WDI$Trend <- WDI$YearNumeric-1950

# FE w/o trend:

FE  <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
             log(GDPPerCapita)+NaturalResourceRents+ColdWar,
           data=WDI,effect="individual",model="within")

# FE with trend:

FE.trend <- plm(WomenBusLawIndex~PopGrowth+UrbanPopulation+
                  FertilityRate+log(GDPPerCapita)+NaturalResourceRents+
                  ColdWar+Trend,data=WDI,effect="individual",
                model="within")

# FE with trend + interaction:

FE.intx <- plm(WomenBusLawIndex~PopGrowth+
                 UrbanPopulation+FertilityRate+
                 log(GDPPerCapita)+NaturalResourceRents+
                 ColdWar+Trend+ColdWar*Trend,
               data=WDI,effect="individual",model="within")

# A table:

TableTrend <- stargazer(FE,FE.trend,FE.intx,
                        title="FE Models of WBLI",
                        column.separate=c(1,1),align=TRUE,
                        dep.var.labels.include=FALSE,
                        dep.var.caption="",
                        covariate.labels=c("Population Growth","Urban Population",
                                           "Fertility Rate","ln(GDP Per Capita)",
                                           "Natural Resource Rents","Cold War",
                                           "Trend (1950=0)","Cold War x Trend"),
                        header=FALSE,model.names=FALSE,
                        model.numbers=FALSE,multicolumn=FALSE,
                        object.names=TRUE,notes.label="",
                        out="Trendy.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Orthogonal Parameters Model                             ####

WDI$lnGDPPerCap<-log(WDI$GDPPerCapita) # create this variable

set.seed(7222009)
OPM.fit <- opm(WomenBusLawIndex~PopGrowth+UrbanPopulation+FertilityRate+
                 lnGDPPerCap+NaturalResourceRents+ColdWar,
               data=WDI,index=c("ISO3","Year"),n.samp=1000)

# Ladder plot of estimates & CIs:

pdf("OPM-Ladder.pdf",8,6)
par(mar=c(4,12,2,2))
caterplot(OPM.fit,parm=c("beta","rho"),
          main=c(""),xlab="Parameter Estimate",
          labels=c("Population Growth",
                   "Urban Population","Fertility Rate",
                   "ln(GDP Per Capita)","Natural Resource Rents",
                   "Cold War","Rho"))
abline(v=c(0),lty=2)
dev.off()

# Short- and long-run effects:

SREs<-summary(OPM.fit)$quants[3:8,3]
LREs<-numeric(6)
for(i in 1:6){
  LREs[i]<-quantile(OPM.fit$samples$beta[,i]/(1-OPM.fit$samples$rho),
                    probs=c(0.50))
}

print(cbind(round(SREs,4),round(LREs,4)))

# /fin