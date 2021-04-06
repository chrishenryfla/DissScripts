#Plotting tip labels with 2 different colors based on a discrete character
ss.df<-read.csv("labeltest.csv")
Origin<-unique(ss.df$Origin)
flavors<-palette(rainbow(length(Origin)))
tmp = factor(ss.df$Origin)
tmp = as.numeric(tmp)
tmp = rainbow(5)[tmp]
tmp = flavors[tmp]
ss.df$Color<-tmp
head(ss.df)

plot(as.phylo(eel.tree), 
     tip.color=tmp,
     type='fan',label.offset=0.1,no.margin=TRUE, cex = .1, edge.width = .5)

eel.tree<-read.tree("pruned_tree.tre")
eel.data<-read.csv("labeltest.csv",row.names=1)
fmode<-as.factor(setNames(eel.data[,1],rownames(eel.data)))
dotTree(eel.tree,fmode,colors=setNames(c("blue","red","yellow","purple","orange"),
                                       c("END","IND","PC","PI","NA")),lwd=.5,ftype="i",label.offset=0.1,no.margin=TRUE, fsize = .1, edge.width = .5)
#Discrete Character Mapping
setwd("~/Desktop/PhyloFolder")
install.packages("geiger")
install.packages("phytools")
library(geiger)
library(phytools)
tree1 <- read.newick("pruned_tree.tre") # load tree
labelz<-read.csv("labels_endemic.csv") # load sp. names into a vector

cbind(labelz$Species, tree1$tip.label) # check traits and tip order
rownames(labelz) <- labelz$Species # set rownames of traits
bigdata <- labelz[match(tree1$tip.label, rownames(labelz)),] # match traits and tips
cbind(labelz$Species, tree1$tip.label) # verify traits and tips are same order

Pheno <- bigdata$Hair # create vector of trait states
names(Pheno) <- bigdata$Species # set names for trait states
Pheno <- as.factor(Pheno) # convert discrtete trait to factor
Pheno

myTree.bifurcating<-multi2di(tree1)
fitER <- fitDiscrete(phy = myTree.bifurcating, dat = Pheno, model = "ER") # equal transition rates
fitSYM <- fitDiscrete(phy = myTree.bifurcating, dat = Pheno, model = "SYM") # symmetric transition rates
fitARD <- fitDiscrete(phy = myTree.bifurcating, dat = Pheno, model = "ARD") # different transition rates

origin.est <- c(fitER$opt$aic, fitSYM$opt$aic, fitARD$opt$aic) # concatenate AIC scores from fitted models
names(origin.est) <- c("Equal rates", "Symmetric rates", "All rates different") # set names of vector
origin.est

mtrees <- make.simmap(tree = myTree.bifurcating, x = Pheno, model = "ER", nsim = 1, drop=FALSE) # simulate 100 histories
par(mfrow = c(1, 1)) # set plotting window for 100 plots
cols <- setNames(object = palette()[1:length(unique(Pheno))], nm = sort(unique(Pheno)))
null <- sapply(X = mtrees, FUN = plotSimmap, colors = cols, lwd = 1, ftype = "off") # plot

pd <- describe.simmap(tree = mtrees, plot = FALSE)

