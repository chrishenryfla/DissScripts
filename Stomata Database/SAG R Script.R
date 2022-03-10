input<-read.csv("s_alpha_GYMNO.csv",header = TRUE,stringsAsFactors = FALSE)
n<-66
attach(input)
c<-1

l_out<-choose(n,2)
out<-matrix(nrow = l_out,ncol = 32)

for(i in c(1:(n-1)))
{
  j<-i+1
  for (j in c(j:n))
  { #Species 1
    out[c,1]<-input[i,1] 
    #Species 2
    out[c,2]<-input[j,1]
    
    #Species with highest g
    b<-ifelse(input[i,4]>input[j,4],i,j)
    out[c,3]<-input[b,1]
    #High S-value
    out[c,4]<-input[b,2]
    #High A-value
    out[c,5]<-input[b,3]
    
    #Species with the lowest g
    d<-ifelse(input[i,4]>input[j,4],j,i)
    out[c,6]<-input[d,1]
    #Low S-value
    out[c,7]<-input[d,2]
    #Low A-value
    out[c,8]<-input[d,3]
    
    #Does the species with the highest g have the highest s?
    a<-ifelse(input[i,4]<input[j,4],i,j)
    out[c,9]<-ifelse(input[b,2]>input[a,2],1,0) 
    
    #Does the species with the highest g have the highest alpha?
    out[c,10]<-ifelse(input[b,3]>input[a,3],1,0) 
    
    #g for high g species divded by g for low g species
    out[c,11]<-input[b,4]/input[a,4]
    
    #s for high g species divded by s for low g species
    out[c,12]<-input[b,2]/input[a,2]
    
    #alpha for high g species divded by alpha for low g species
    out[c,13]<-input[b,3]/input[a,3]
    
    #s for Species with highest g
    hs<-ifelse(input[i,4]>input[j,4],input[i,2],input[j,2])
    out[c,22]<-hs
    #alpha for Species with highest g
    ha<-ifelse(input[i,4]>input[j,4],input[i,3],input[j,3])
    out[c,23]<-ha
    #s for Species with lowest g
    ls<-ifelse(input[i,4]>input[j,4],input[j,2],input[i,2])
    out[c,24]<-ls
    #alpha for Species with lowest g
    la<-ifelse(input[i,4]>input[j,4],input[j,3],input[i,3])
    out[c,25]<-la
    
    #Species with low g
    lg<-ifelse(input[i,4]>input[j,4],input[j,4],input[i,4])
    #Species with high g
    hg<-ifelse(input[i,4]>input[j,4],input[i,4],input[j,4])
    
    m<-0.000449688
    
    gda<-function (m,ha,ls){(m*(ls^.5))*1000000/(ls+ha)}
    out[c,26]<-gda(m,ha,ls)
  
    gds<-function (m,la,hs){(m*(hs^.5))*1000000/(hs+la)}
    out[c,27]<-gds(m,la,hs)
    
    hgc<-function (m,ha,hs){(m*(hs^.5))*1000000/(hs+ha)}
    out[c,28]<-hgc(m,ha,hs)
    
    lgc<-function (m,la,ls){(m*(ls^.5))*1000000/(ls+la)}
    out[c,29]<-lgc(m,la,ls)
    
    #low g/high g
    out[c,30]<-hgc(m,ha,hs)/lgc(m,la,ls)
    
    #change in g due to high a/low g
    out[c,31]<-(gda(m,ha,ls))/lgc(m,la,ls)
    
    #change in g due to high s/low g
    out[c,32]<-(gds(m,la,hs))/lgc(m,la,ls)
  
    c<-c+1
  }
  #Number of species with high g and high s
  out[1,14]<-sum(as.numeric(out[,9]))
  
  #Number of species with high g and high alpha
  out[1,15]<-sum(as.numeric(out[,10]))
  
  #Average g for g high divided by g for g low
  out[1,16]<-mean(as.numeric(out[,11]))
  
  #Average s for g high divided by s for g low
  out[1,17]<-mean(as.numeric(out[,12]))
  
  #Average alpha for g high divided by alpha for g low
  out[1,18]<-mean(as.numeric(out[,13]))
  
  #Standard Error for average g
  w<-out[,11]
  SEg<-function(w){sd(w)/sqrt(length(w))}
  out[1,19]<-SEg(w)
  
  #Standard Error for average s
  x<-out[,12]
  SEs<-function(x){sd(x)/sqrt(length(x))}
  out[1,20]<-SEs(x)
  
  #Standard Error for average alpha
  y<-out[,13]
  SEa<-function(y){sd(y)/sqrt(length(y))}
  out[1,21]<-SEa(y)
  
}
columnames<-c("Species 1",
              "Species 2",
              "Species with highest g",
              "High S-value",
              "High A-value",
              "Species with the lowest g",
              "Low S-value",
              "Low A-value",
              "Does the species with the highest g have the highest s?",
              "Does the species with the highest g have the highest alpha?",
              "g for high g species divded by g for low g species",
              "s for high g species divded by s for low g species",
              "alpha for high g species divded by alpha for low g species",
              "Number of species with high g and high s",
              "Number of species with high g and high alpha",
              "Average g for g high divided by g for g low",
              "Average s for g high divided by s for g low",
              "Average alpha for g high divided by alpha for g low",
              "Standard Error for average g",
              "Standard Error for average s",
              "Standard Error for average alpha",
              "s for Species with highest g",
              "alpha for Species with highest g",
              "s for Species with lowest g",
              "alpha for Species with lowest g",
              "Change in g due to alpha",
              "Change in g due to s",
              "g with high alpha and s",
              "g with low alpha and s",
              "low g divided by high g",
              "change in g due to high a divided by low g",
              "change in g due to high s/low g")
colnames(out)<-columnames
View(out)
write.csv(out, "SAG_GYMNO_Output.csv")
