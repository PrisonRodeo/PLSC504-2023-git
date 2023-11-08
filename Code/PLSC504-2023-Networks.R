#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things...                                  ####
#
# PLSC 504 -- Fall 2023
#
# Introduction to network analysis...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","ggplot2","gridExtra","plyr","texreg",
     "randomNames","RColorBrewer","network","statnet","sna",
     "igraph","intergraph","GGally","ergm","devtools")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 4-5 times until you get "Package count = 15"
#
# NOTE: The GERGM package (for fitting generalized ERGMs) is currently
# (11/7/2023) BROKEN, and does not work with any reasonably up-to-
# date version of R. This is bad, because GERGM is a pretty awesome
# package. With that said, the code below that makes use of the GERGM
# package / routines will not currently work, so I've moved it to 
# the end of the file. I realize that this is suboptimal, but I haven't
# been able to get in touch with Matt Denny. to figure out what to do
# about it.
#
# Options:

options(scipen = 99) # bias against scientific notation
options(digits = 4) # show fewer decimal places
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Make some example plots...                       ####
#
# Random five-node undirected network:

N<-5
set.seed(7222009)
N5 <- rgraph(N,mode="graph",tprob=0.4)
N5 <- network(N5,directed=FALSE)
network.vertex.names(N5) <- randomNames(N,which.names="first")

# plot:

pdf("N5Nondirected.pdf",7,4)
par(mar=c(2,2,2,2))
ggnet2(N5,size=12,label=TRUE,color="grey",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5)
dev.off()

# Adjacency matrix:

N5[1:5,1:5]

# Same, but circle plot:

pdf("N5Nondirected-Circle.pdf",5,6)
par(mar=c(2,2,2,2))
ggnet2(N5,size=12,label=TRUE,color="grey",mode="circle",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5)
dev.off()

# Directed network:

set.seed(9021970)
N5D <- rgraph(N,mode="digraph",tprob=0.5)
N5D <- network(N5D,directed=TRUE)
network.vertex.names(N5D) <- network.vertex.names(N5)

# plot:

pdf("N5Directed.pdf",7,4)
par(mar=c(2,2,2,2))
ggnet2(N5D,size=10,label=TRUE,color="grey",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5,
       arrow.size=8,arrow.gap=0.05)
dev.off()

# Adjacency matrix:

N5D[1:5,1:5]

# Same, only circle plot:

pdf("N5Directed-Circle.pdf",7,6)
par(mar=c(2,2,2,2))
ggnet2(N5D,size=10,label=TRUE,color="grey",mode="circle",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5,
       arrow.size=8,arrow.gap=0.05)
dev.off()

# Add gender data to nodes:

N5 %v% "Gender"<-ifelse(network.vertex.names(N5) %in% c("Savannah","Claire","Dawn","Erin"),"Female","Male")
N5D %v% "Gender"<-ifelse(network.vertex.names(N5D) %in% c("Savannah","Claire","Dawn","Erin"),"Female","Male")

# Plot node characteristics:

pdf("N5D-Gender.pdf",7,6)
par(mar=c(2,2,2,2))
ggnet2(N5D,size=10,label=TRUE,color="Gender",palette="Set1",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5,
       arrow.size=8,arrow.gap=0.05)
dev.off()

# Add name length data to nodes:

N5 %v% "NameLength"<-nchar(network.vertex.names(N5))
N5D %v% "NameLength"<-nchar(network.vertex.names(N5D))

# Plot again:

pdf("N5D-NameLength.pdf",7,6)
par(mar=c(2,2,2,2))
ggnet2(N5D,size="NameLength",size.legend="Name Length",
       label=TRUE,color="Gender",palette="Set1",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5,
       arrow.size=8,arrow.gap=0.05)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Edge characteristics: Differences in name length...
#
# Most of this code doesn't work thanks to -network- messing
# up the ordering of nodes. It's here so I can try to fix it
# later:
# 
# N5D.edges<-as.edgelist(N5D)
# NameLengths<-nchar(attributes(N5D.edges)$vnames)
# NameDiffs<-numeric(length(N5D$mel))
# for (i in 1:length(N5D$mel)) {
#    NameDiffs[i]<-abs(nchar(attributes(N5D.edges)$vnames)[N5D$mel[[i]]$inl] -
#                          nchar(attributes(N5D.edges)$vnames)[N5D$mel[[i]]$outl])
# }
# 
# 
# for (i in 1:length(N5D$mel)) {
#   NameDiffs[i]<-abs(N5D$val[[(N5D$mel[[i]]$inl)]]$NameLength - N5D$val[[(N5D$mel[[i]]$outl)]]$NameLength)
#   }
# 
# Fix it by-hand:

NameDiffs<-c(2,4,0,0,0)
set.edge.value(N5D,"NameDiffs",NameDiffs)

# Plot with edge labels = abs(Difference in name lengths):

pdf("N5D-NameLength2.pdf",7,6)
par(mar=c(2,2,2,2))
ggnet2(N5D,size="NameLength",size.legend="Name Length",
       label=TRUE,color="Gender",palette="Set1",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5,
       edge.label="NameDiffs",
       arrow.size=8,arrow.gap=0.05)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Pic for "centrality" slide:

Z<-10
set.seed(501)
Z10 <- rgraph(Z,mode="graph",tprob=0.7)
Z10 <- network(Z,directed=FALSE)
network.vertex.names(Z10) <- LETTERS[1:10]

# Note that these two plots probably won't look 
# exactly like those in the slides...

pdf("Centrality101.pdf",7,4)
par(mar=c(2,2,2,2))
ggnet2(Z10,size=12,label=TRUE,color="grey",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5)
dev.off()

pdf("Centrality102.pdf",7,4)
par(mar=c(2,2,2,2))
ggnet2(Z10,size=12,label=TRUE,color="grey",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Basic statistics on networks...                  ####
#
# Degree:

cbind(network.vertex.names(N5D),sna::degree(N5D),
      sna::degree(N5D,cmode="indegree"),
      sna::degree(N5D,cmode="outdegree"))

# Betweenness:

cbind(network.vertex.names(N5D),sna::betweenness(N5D))


# Closeness:

cbind(network.vertex.names(N5D),
      sna::closeness(N5D),
      igraph::closeness(asIgraph(N5D)))

# Eigenvector centrality:

EC<-igraph::eigen_centrality(asIgraph(N5))
cbind(network.vertex.names(N5),
      EC$vector)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Transitivity plots                               ####

W<-3
set.seed(2345)
W3a <- rgraph(W,mode="graph",tprob=0.66)
W3a <- network(W3a,directed=FALSE)
network.vertex.names(W3a) <- LETTERS[1:W]
W3b <- rgraph(W,mode="graph",tprob=1)
W3b <- network(W3b,directed=FALSE)
network.vertex.names(W3b) <- LETTERS[1:W]

W1<-ggnet2(W3a,size=12,label=TRUE,color="grey",
           label.color="black",label.size=3.5,
           edge.color="black",edge.size=0.5,
           mode="circle")
W2<-ggnet2(W3b,size=12,label=TRUE,color="grey",
           label.color="black",label.size=3.5,
           edge.color="black",edge.size=0.5,
           mode="circle")

pdf("Transitivity.pdf",10,4)
grid.arrange(W1,W2,ncol=2)
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SCOTUS data example                              ####

SCOTUS <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2023-git/master/Data/SCOTUS-IRT.csv")
SCOTUS <- SCOTUS[complete.cases(SCOTUS),] # no missingness

# Agreement matrix:

SCAgree<- (1 - dist(t(SCOTUS[,2:10]),method="binary",
                    upper=TRUE))

# Network things:

SCnet<-network(as.matrix(SCAgree),matrix.type="adjacency",
               directed=FALSE)
network.vertex.names(SCnet) <- colnames(SCOTUS[,2:10])
SC.edges<-as.matrix(SCnet,matrix.type="edgelist") # edges
to.lower<-function(X) X[lower.tri(X,diag=FALSE)] # function
SCAg <- to.lower(as.matrix(SCAgree))
SCAg3 <- to.lower(as.matrix(SCAgree))*3
network::set.edge.attribute(SCnet,"Agreement",SCAg)
network::set.edge.attribute(SCnet,"Agreement3",SCAg3) # for plots

# Add Segal-Cover scores to nodes - "by hand,"
# because I'm lazy...:

SCnet %v% "SegalCover"<-c(0.045,0.250,0.415,0,0.365,0.325,0.160,0.680,0.475)
SeCoData <- data.frame(id = network.vertex.names(SCnet),
                       SegalCover = c(0.045,0.250,0.415,0,0.365,0.325,0.160,0.680,0.475),
                       GOPAppt = c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","No","No"))
rownames(SeCoData) <- network.vertex.names(SCnet)

# Plot:

pdf("SCOTUSnetwork.pdf",7,6)
par(mar=c(3,3,3,3))
ggnet2(SCnet,size=22,label=TRUE,color="grey90",
       label.color="black",label.size=3.5,mode="circle",
       edge.color="black",edge.size="Agreement3")
dev.off()

# Create a binary network object with edges between two 
# justices IFF their agreement was greater than 50%:

SCA <- matrix(,9,9)
SCA[lower.tri(SCA, diag=FALSE)] <- ifelse(SCAgree>0.5,1,0)
diag(SCA)<-1
SCA<-pmax(SCA,t(SCA),na.rm=TRUE)
SCA

# Network object:

SCN<-network(as.matrix(SCA),matrix.type="adjacency",
          directed=FALSE)
network.vertex.names(SCN) <- colnames(SCOTUS[,2:10])
SCN.edges<-as.matrix(SCN,matrix.type="edgelist") # edges

# Add Segal-Cover scores + GOP appointing president:

SCN %v% "SegalCover"<-c(0.045,0.250,0.415,0,0.365,0.325,0.160,0.680,0.475)
SCN %v% "GOPPres"<-c(1,1,1,1,1,1,1,0,0)
summary(SCN)

# Plot the network:

pdf("SCOTUSnetwork2.pdf",7,6)
par(mar=c(3,3,3,3))
ggnet2(SCN,size=15,label=TRUE,color="grey90",
       label.color="black",label.size=3.5,mode="circle",
       edge.color="black")
dev.off()

# Fit some ERGMs...
#
# Edges: "constant only"

fitE<-ergm(SCN~edges,estimate="MPLE")
summary(fitE)

# Same as density / proportion of edges:

plogis(fitE$coefficients[1])

# Edges + triangles:

fitET<-ergm(SCN~edges+triangle,estimate="MPLE",
           control=control.ergm(MPLE.covariance.method='Godambe'))
summary(fitET)

# Do justices appointed by the same party president tend to
# vote similarly to each other?

fit1<-ergm(SCN~nodematch("GOPPres")+edges+triangle,estimate="MPLE",
           control=control.ergm(MPLE.covariance.method='Godambe'))
summary(fit1)

# Do ideologically similar justices tend to vote similarly to 
# each other?

fit2<-ergm(SCN~absdiff("SegalCover")+edges+triangle,estimate="MPLE",
           control=control.ergm(MPLE.covariance.method='Godambe'))
summary(fit2)

# Both:

fit3<-ergm(SCN~nodematch("GOPPres")+absdiff("SegalCover")+edges+triangle,
           estimate="MPLE",control=control.ergm(MPLE.covariance.method='Godambe'))
summary(fit3)

# END!                                          ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# GERGM things (currently defunct!)             ####  
#
# Redo plot, using GERGM plotter:
# 
# SCAmat <- as.matrix(SCAgree)
# 
# pdf("altSCOTUSnetwork.pdf",7,6)
# plot_network(SCAmat, white_background = TRUE)
# dev.off()
# 
# # GERGM of the strength of ties (agreement) as a
# # function of ideological distance (difference between
# # Segal-Cover scores) and same-party appointing president:
# 
# form <- SCAmat ~ edges +
#   absdiff(covariate="SegalCover") +
#   nodematch("GOPAppt",base="Yes")
# 
# GERGM <- gergm(form, covariate_data = SeCoData,
#                seed = 7222009,
#                convergence_tolerance = 0.5)
# 
# # Plot the estimates:
# 
# pdf("SCOTUS-GERGM-Estimates.pdf",7,5)
# Estimate_Plot(GERGM,
#               normalize_coefficients = FALSE,
#               coefficients_to_plot = "both",
#               coefficient_names = c("Edges",
#                                     "Segal-Cover Difference",
#                                     "Same-Party Appointer",
#                                     "Intercept",
#                                     "Dispersion Parameter"))
# dev.off()