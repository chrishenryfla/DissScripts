#Setting WD & Loading Files
{
  setwd("/u/home/c/chrishen/PhytoPhylo")
  install.packages("geiger",repos="http://cran.us.r-project.org")
  install.packages("phytools",repos="http://cran.us.r-project.org")
  library(geiger)
  library(phytools)
  tree1 <- read.newick("pruned_tree.tre") 
  labelz<-read.csv("labels_endemic.csv")
}

#Assigning Discrete Characters to respective tips
{
  cbind(labelz$Species, tree1$tip.label) 
  rownames(labelz) <- labelz$Species 
  bigdata <- labelz[match(tree1$tip.label, rownames(labelz)),] 
  cbind(labelz$Species, tree1$tip.label)
  
  Pheno <- bigdata$Hair
  names(Pheno) <- bigdata$Species 
  Pheno <- as.factor(Pheno)
}

#Creating a simmap based on a number of simulations
{
  myTree.bifurcating<-multi2di(tree1)
  
  myTree.bifurcating$edge.length[myTree.bifurcating$edge.length==0]<-1e-8
  
  mtrees <- make.simmap(tree = myTree.bifurcating, x = Pheno, model = "ARD", nsim = 3, drop=FALSE) 
  
  x<-getStates(mtrees,type = "tips")
  
  fitARD<-ace(x,myTree.bifurcating,model="ARD", type="discrete")
  
  cols <- setNames(object = palette()[1:length(unique(Pheno))], nm = sort(unique(Pheno)))
  
  
  #Output for n simulations
  pd<-summary(mtrees, plot = FALSE)
}

#Create pdf
{
  pdf("HIPhylo.pdf", width = 8.5, height = 11)
  
  #Plot tree without model info
  plotTree(tree1, type= "fan", fsize = .08, lwd = 0.09, ftype = "i")
  
  add.simmap.legend(colors = cols, prompt = F, x=0.9*par()$usr[1],
                    y=-max(nodeHeights(tree1)), fsize = 0.7)
  
  #parameter setting the outline of the pies at the nodes and tips
  par(fg = "transparent")
  
  #pasting Bayesian posterior probabilities at each node
  nodelabels(node=1:tree1$Nnode+Ntip(tree1),
             pie=pd$ace,piecol=cols,cex=0.08)
  
  #pasting species at the tips by hair type
  tiplabels(pie=to.matrix(Pheno,sort(unique(Pheno))),
            piecol=cols,cex=0.06)
  
  dev.off()
}

