
setwd("/Users/lilchainey/Dropbox/Christian's Science Stuff/Surface Conductance/Archive/CorrelationMatrix/Archive")
library(ape)
library(nlme)
library(geiger)
library(phytools)
library(phylolm)

# Read in the tree- read.newick was a script that dealt with phylocom files more easily
# but I don't know whether it's deprecated or suitable for your tree format or not. 
# I recommend using whatever command ape uses to read in trees instead.
#source("read.newick.R")
#tree<-read.newick(file="gsmetanalysis_allspecies_1415_ages.new")
#tree<-collapse.singles(tree)
#tree -> allspeciestree

# Read in the data
read.csv("pgls_hair.csv")-> gsdata
read.newick("nutree.tre") -> tree  # input your tree


#######My tree has too more species than I have data, so I pruned it with 
#the next block of code (ends line 58)
plot(tree, use.edge.length=T, font=3, cex=0.6, direction="r")
tree<-ladderize(tree,right=T)
plot(tree, use.edge.length=T, font=3, cex=0.6, direction="r")
plot(tree,type="p", use.edge.length=T, font=3,cex=0.25,direction="r") 
axisPhylo()

length(tree$tip.label) 
library(stringr)
tree2<-tree
tree2$tip.label<-str_replace_all(tree$tip.label,pattern="'",replacement="")
par(mfrow=c(1,2))
plot(tree)
plot(tree2)
#Use tree2 because it has quotes
tree<-tree2
plot(tree, show.tip.label=F, type="fan")
axisPhylo()
plot(tree, show.tip.label = F, type = "fan")

plot(tree, use.edge.length=T, font=3, cex=0.3, direction="r") 
axisPhylo()
head(gsdata)
str(gsdata)
species<-gsdata$Species
species

treebutnottrait<- tree$tip.label[which(!(tree$tip.label %in% species))]
tree10 <- drop.tip(tree, treebutnottrait) #Make tree with ten species
plot(tree10)
axisPhylo()

cbind(gsdata$Species, tree10$tip.label)
rownames(gsdata)<-gsdata$Species
gsdata
gsdata <- gsdata[match(tree10$tip.label, rownames(gsdata)),]
cbind(gsdata$Species, tree10$tip.label)

allspeciestree<-tree


########
# Tree is ready
Brownian = matrix(NA, 15625, 7)
Pagel = matrix(NA, 15625, 8)
OU = matrix(NA, 15625, 8)


for (ii in 1:113){
	for (kk in (ii+1):114){
		
    paste(names(gsdata)[ii+1], "~", names(gsdata)[kk+1]) -> ff	# define model

    if(anyNA(gsdata[ii +1]) | anyNA(gsdata[kk+1])) {
      next
    }	
	  
    # Brownian motion
    try(phylolm(as.formula(ff), data = gsdata, phy = allspeciestree, model = "BM")) -> modelBR
    names(gsdata)[ii+1] -> Brownian[(ii-1)*114 +kk-1,1]
    names(gsdata)[kk+1] -> Brownian[(ii-1)*114 +kk-1,2]

    # Catch failures to converge
    if (is(modelBR,"try-error")){
      -999 -> Brownian[(ii-1)*114 +kk-1,3]
      -999  -> Brownian[(ii-1)*114 +kk-1,4]
      -999  -> Brownian[(ii-1)*114 +kk-1,5]
      -999  -> Brownian[(ii-1)*114 +kk-1,6]
      -999 -> Brownian[(ii-1)*114 +kk-1,7]
      -999 -> Brownian[(ii-1)*114 +kk-1,8] # AICc	
    } else {
      summary(modelBR)$tTable -> fita
      summary(modelBR)$coef[2,1] -> Brownian[(ii-1)*114+kk-1,3]
      summary(modelBR)$coef[2,2] -> Brownian[(ii-1)*114+kk-1,4]
      summary(modelBR)$coef[2,4] -> Brownian[(ii-1)*114 +kk-1,5]
      summary(modelBR)$aic -> Brownian[(ii-1)*114 +kk-1,6]
      summary(modelBR)$aic+ 2*3*4/(10-3-1) -> Brownian[(ii-1)*114+kk-1,7] # AICc
    }
  		
    # corBrownian(1,phy=allspeciestree) -> bm
    # modelBR <- gls(as.formula(ff), data = gsdata, correlation = bm)
    # summary(modelBR)$tTable -> fita
    # names(gsdata)[ii+1] -> Brownian[(ii-1)*30 +kk-1,1]
    # names(gsdata)[kk+1] -> Brownian[(ii-1)*30 +kk-1,2]
    # fita[2,1] -> Brownian[(ii-1)*30 +kk-1,3]
    # fita[2,2] -> Brownian[(ii-1)*30 +kk-1,4]
    # fita[2,4] -> Brownian[(ii-1)*30 +kk-1,5]
    # summary(modelBR)$AIC -> Brownian[(ii-1)*30 +kk-1,6]
    # summary(modelBR)$AIC  + 2*3*4/(10-3-1) -> Brownian[(ii-1)*30 +kk-1,7] # AICc
    
    # Pagel
    try(phylolm(as.formula(ff), data = gsdata, phy = allspeciestree, model = "lambda")) -> modelPL
    names(gsdata)[ii+1] -> Pagel[(ii-1)*114 +kk-1,1]
    names(gsdata)[kk+1] -> Pagel[(ii-1)*114 +kk-1,2]

    # Pagel seems to fail to converge for some correlations
    if (is(modelPL,"try-error")){
      -999 -> Pagel[(ii-1)*114 +kk-1,3]
      -999  -> Pagel[(ii-1)*114 +kk-1,4]
      -999  -> Pagel[(ii-1)*114 +kk-1,5]
      -999  -> Pagel[(ii-1)*114 +kk-1,6]
      -999 -> Pagel[(ii-1)*114 +kk-1,7]
      -999 -> Pagel[(ii-1)*114 +kk-1,8] # AICc	
    } else {
      summary(modelPL)$tTable -> fita
      summary(modelPL)$coef[2,1] -> Pagel[(ii-1)*114 +kk-1,3]
      summary(modelPL)$coef[2,2] -> Pagel[(ii-1)*114+kk-1,4]
      summary(modelPL)$coef[2,4] -> Pagel[(ii-1)*114 +kk-1,5]
      summary(modelPL)$aic -> Pagel[(ii-1)*114 +kk-1,6]
      modelPL$optpar-> Pagel[(ii-1)*114 +kk-1,7]
      summary(modelPL)$aic+ 2*3*4/(10-3-1) -> Pagel[(ii-1)*114 +kk-1,8] # AICc
    } # maybe it has one more parameter because it fits lambda?
  
    # modelPL <- gls(as.formula(ff), data = gsdata, correlation = corPagel(1,allspeciestree))
    # summary(modelPL)$tTable -> fita
    # names(gsdata)[ii+1] -> Pagel[(ii-1)*30 +kk-1,1]
    # names(gsdata)[kk+1] -> Pagel[(ii-1)*30 +kk-1,2]
    # fita[2,1] -> Pagel[(ii-1)*30 +kk-1,3]
    # fita[2,2] -> Pagel[(ii-1)*30 +kk-1,4]
    # fita[2,4] -> Pagel[(ii-1)*30 +kk-1,5]
    # summary(modelPL)$AIC -> Pagel[(ii-1)*30 +kk-1,6]
    # attr(modelPL$apVar,"Pars")["corStruct"]-> Pagel[(ii-1)*30 +kk-1,7]
    # summary(modelPL)$AIC  + 2*3*4/(10-3-1) -> Pagel[(ii-1)*30 +kk-1,8] # AICc
    
    OU
    #corMartins(1,phy=allspeciestree) -> ou
    
    try(phylolm(as.formula(ff), data = gsdata, phy = allspeciestree, model = "OUfixedRoot")) -> modelOU
    
    names(gsdata)[ii+1] -> OU[(ii-1)*114 +kk-1,1]
    names(gsdata)[kk+1] -> OU[(ii-1)*114 +kk-1,2]
    
    # OU seems to fail to converge for some correlations
    if (is(modelOU,"try-error")){
      -999 -> OU[(ii-1)*114 +kk-1,3]
      -999  -> OU[(ii-1)*114 +kk-1,4]
      -999  -> OU[(ii-1)*114 +kk-1,5]
      -999  -> OU[(ii-1)*114 +kk-1,6]
      -999 -> OU[(ii-1)*114 +kk-1,7]
      -999 -> OU[(ii-1)*114 +kk-1,8] # AICc	
    } else {
      summary(modelOU)$tTable -> fita
      summary(modelOU)$coef[2,1] -> OU[(ii-1)*114 +kk-1,3]
      summary(modelOU)$coef[2,2] -> OU[(ii-1)*114 +kk-1,4]
      summary(modelOU)$coef[2,4] -> OU[(ii-1)*114 +kk-1,5]
      summary(modelOU)$aic -> OU[(ii-1)*114 +kk-1,6]
      modelOU$optpar-> OU[(ii-1)*114 +kk-1,7]
      summary(modelOU)$aic+ 2*3*4/(10-3-1) -> OU[(ii-1)*114 +kk-1,8] # AICc
    } 
  }
}

colnames(Brownian) <- c("Y", "X", "Slope", "SE", "P", "AIC", "AICc")
Brownian[-which(is.na(Brownian[,1])=="TRUE"),] -> Brownian2
write.csv(Brownian2, file = "Brownian.csv")

colnames(Pagel) <- c("Y", "X", "Slope", "SE", "P", "AIC", "lambda", "AICc")
Pagel[-which(is.na(Pagel[,1])=="TRUE"),] -> Pagel2
write.csv(Pagel2, file = "Pagel.csv")

colnames(OU) <- c("Y", "X", "Slope", "SE", "P", "AIC", "alpha", "AICc")
OU[-which(is.na(OU[,1])=="TRUE"),] -> OU2
write.csv(OU2, file = "OU.csv")
