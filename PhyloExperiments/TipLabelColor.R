#Set Working Directory
setwd("~/Desktop/PhyloFolder")
library(phytools)

#Read in tre. file, labels/trait data associated with the tree and match the
tree1 <- read.newick("pruned_tree_END.tre")
labelz<-read.csv("labels_H.csv")
TipsInOrder<-labelz[match(tree1$tip.label, labelz$X),]

#Plotting tip labels with 2 different colors based on a discrete character

Hair<-unique(labelz$Hair)

tmp = factor(labelz$Hair)

tmp = as.numeric(tmp)

#Assigning colors for each character
tmp = c("#000000","#FF0000FF")[tmp]

Hair$Color<-tmp

head(Hair)

#Make pdf for plot
pdf(file = "demo-pdf.pdf", height = 9, width = 8)
plot(as.phylo(tree1), 
     tip.color=tmp,
     type='fan',label.offset=0.1,no.margin=TRUE, cex = .2, edge.width = .5)

#Add Legend
legend("bottomleft",title = "Hair Presence", c("N","Y"), fill = c("#000000","#FF0000FF"),horiz = TRUE, cex = .4)
dev.off()
