input<-read.csv("s_f_ANGIO.csv",header = TRUE)
n<-632
attach(input)
c<-1

l_out<-choose(n,2)
out<-matrix(nrow = l_out,ncol = 16)

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
    
    #Does the species with the highest g have the highest s?
    a<-ifelse(input[i,4]<input[j,4],i,j)
    out[c,4]<-ifelse(input[b,2]>input[a,2],1,0) 
    
    #Does the species with the highest g have the highest f?
    out[c,5]<-ifelse(input[b,3]>input[a,3],1,0) 
    
    #g for high g species divded by g for low g species
    out[c,6]<-input[b,4]/input[a,4]
    
    #s for high g species divded by s for low g species
    out[c,7]<-input[b,2]/input[a,2]
    
    #f for high g species divded by f for low g species
    out[c,8]<-input[b,3]/input[a,3]
    c<-c+1
    
  }
  #Number of species with high g and high s
  out[1,9]<-sum(out[,4])
  
  #Number of species with high g and high f
  out[1,10]<-sum(out[,5])
  
  #Average g for g high divided by g for g low
  out[1,11]<-mean(out[,6])
  
  #Average s for g high divided by s for g low
  out[1,12]<-mean(out[,7])
  
  #Average f for g high divided by f for g low
  out[1,13]<-mean(out[,8])
  
  #Standard Error for average g
  x<-out[,6]
  SEg<-function(x){sd(x)/sqrt(length(x))}
  out[1,14]<-SEg(x)
  
  #Standard Error for average s
  y<-out[,7]
  SEs<-function(y){sd(y)/sqrt(length(y))}
  out[1,15]<-SEs(y)
  
  #Standard Error for average f
  z<-out[,8]
  SEf<-function(z){sd(z)/sqrt(length(z))}
  out[1,16]<-SEf(z)
  
}
View(out)
write.csv(out, "SFG_ANGIO_Output.csv")

