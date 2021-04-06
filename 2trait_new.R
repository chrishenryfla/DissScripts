setwd("~/Desktop/PhyloFolder")

library(phytools)
tree1 <- read.newick("pruned_tree_LA.tre")
ContVals<-read.csv("labels_LA.csv", row.names = 1)


myTree.bifurcating<-multi2di(tree1)
myTree.bifurcating$edge.length[myTree.bifurcating$edge.length==0]<-1e-8


cbind(ContVals$Species, tree1$tip.label) 
rownames(ContVals) <- ContVals$Species
bigdata <- ContVals[match(tree1$tip.label, rownames(ContVals)),] 
cbind(ContVals$Species, tree1$tip.label)

Pheno <- bigdata$Hair_A
names(Pheno) <- bigdata$Species
Pheno <- as.factor(Pheno)


hp<-as.factor(setNames(ContVals[,4],rownames(ContVals)))
avg_la<-setNames(ContVals[,2],rownames(ContVals))

newtrees<-make.simmap(myTree.bifurcating,hp,nsim=3)

obj<-densityMap(newtrees,states=c("0","1"),plot = FALSE)

plot(obj,outline=FALSE,fsize = .14)

# subset the tree and leaf area
avg_la_sub <- avg_la[1:20]
tree_sub <- drop.tip(tree1, names(avg_la)[21:529])
dotTree(tree_sub,avg_la_sub,length=1,fsize=0.14,lwd=.5,ftype="i",type="fan")

# save a big pdf

pdf(file = "demo-pdf.pdf", height = 50, width = 10)
dotTree(tree1,avg_la,fsize=0.14,lwd=.5,ftype="i", k = 1)
plot(obj$tree,colors=obj$cols,add=TRUE,ftype="off",lwd=5,
     xlim=get("last_plot.phylo",envir=.PlotPhyloEnv)$x.lim,
     ylim=get("last_plot.phylo",envir=.PlotPhyloEnv)$y.lim)
dev.off()
