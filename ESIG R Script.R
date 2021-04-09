input<-read.csv("e_i_s_ANGIO_cc.csv",header = TRUE,stringsAsFactors = FALSE)
n<-632
c<-1

l_out<-choose(n,2)
out<-matrix(nrow = l_out,ncol = 44)

for(i in c(1:(n-1)))
{
  j<-i+1
  for (j in c(j:n))
  { #Species 1
    out[c,1]<-input[i,1] 
    #Species 2
    out[c,2]<-input[j,1]
    
    #Species with highest g
    b<-ifelse(input[i,5]>input[j,5],i,j)
    out[c,3]<-input[b,1]
    #High g s-value
    out[c,4]<-input[b,2]
    #High g i-value
    out[c,5]<-input[b,3]
    #High g e-value
    out[c,6]<-input[b,4]
    
    #Species with low g
    d<-ifelse(input[i,5]<input[j,5],j,i)
    #Low g s-value
    out[c,7]<-input[b,2]
    #Low g i-value
    out[c,8]<-input[b,3]
    #Low g e-value
    out[c,9]<-input[b,4]
    
    #Does the species with the highest g have the highest s?
    a<-ifelse(input[i,5]<input[j,5],i,j)
    out[c,10]<-ifelse(input[b,2]>input[a,2],1,0) 
    
    #Does the species with the highest g have the highest i?
    out[c,11]<-ifelse(input[b,3]>input[a,3],1,0) 
    
    #Does the species with the highest g have the highest e?
    out[c,12]<-ifelse(input[b,4]>input[a,4],1,0) 
    
    #g for high g species divded by g for low g species
    out[c,13]<-input[b,5]/input[a,5]
    
    #s for high g species divded by s for low g species
    out[c,14]<-input[b,2]/input[a,2]
    
    #i for high g species divded by i for low g species
    out[c,15]<-input[b,3]/input[a,3]
    
    #e for high g species divded by e for low g species
    out[c,16]<-input[b,4]/input[a,4]
    
    #e for Species with highest g
    he<-ifelse(input[i,5]>input[j,5],input[i,4],input[j,4])
    out[c,28]<-he
    #s for Species with highest g
    hs<-ifelse(input[i,5]>input[j,5],input[i,2],input[j,2])
    out[c,29]<-hs
    #i for Species with highest g
    hi<-ifelse(input[i,5]>input[j,5],input[i,3],input[j,3])
    out[c,30]<-hi
    #e for Species with lowest g
    le<-ifelse(input[i,5]>input[j,5],input[j,4],input[i,4])
    out[c,31]<-le
    #s for Species with lowest g
    ls<-ifelse(input[i,5]>input[j,5],input[j,2],input[i,2])
    out[c,32]<-ls
    #i for Species with lowest g
    li<-ifelse(input[i,5]>input[j,5],input[j,3],input[i,3])
    out[c,33]<-li
    
    #Species with low g
    lg<-ifelse(input[i,5]>input[j,5],input[j,5],input[i,5])
    
    #Species with high g
    hg<-ifelse(input[i,5]>input[j,5],input[i,5],input[j,5])
    
    m<-0.000449688
    
    gdi<-function (m,hi,le,ls){(m*hi*(ls^.5))*1000000/(hi*ls+(1-hi)*le)}
    out[c,34]<-gdi(m,hi,le,ls)
    
    gds<-function (m,hs,le,li){(m*li*(hs^.5))*1000000/(li*hs+(1-li)*le)}
    out[c,35]<-gds(m,hs,le,li)
    
    gde<-function (m,ls,li,he){(m*li*(ls^.5))*1000000/(li*ls+(1-li)*he)}
    out[c,36]<-gde(m,ls,li,he)
    
    gdie<-function (m,ls,hi,he){(m*hi*(ls^.5))*1000000/(hi*ls+(1-hi)*he)}
    out[c,37]<-gdie(m,ls,hi,he)
  
    hgc<-function (m,hi,he,hs){(m*hi*(hs^.5))*1000000/(hi*hs+(1-hi)*he)}
    out[c,38]<-hgc(m,hi,he,hs)
    lgc<-function (m,li,le,ls){(m*li*(ls^.5))*1000000/(li*ls+(1-li)*le)}
    out[c,39]<-lgc(m,li,le,ls)
    
    #High g/Low g
    out[c,40]<-hgc(m,hi,he,hs)/lgc(m,li,le,ls)
    
    #change in g due to high i/low g
    out[c,41]<-(gdi(m,hi,le,ls))/lgc(m,li,le,ls)
    #change in g due to high s/low g
    out[c,42]<-(gds(m,hs,le,li))/lgc(m,li,le,ls)
    #Change in g due to high e/low g
    out[c,43]<-(gde(m,ls,li,he))/lgc(m,li,le,ls)
    #Change in g due to high ie/low g
    out[c,44]<-(gdie(m,ls,hi,he))/lgc(m,li,le,ls)

    c<-c+1
    
  }
  #Number of species with high g and high s
  out[1,17]<-sum(as.numeric(out[,10]))
  
  #Number of species with high g and high i
  out[1,18]<-sum(out[,11])
  
  #Number of species with high g and high e
  out[1,19]<-sum(out[,12])
  
  #Average g for g high divided by g for g low
  out[1,20]<-mean(out[,13])
  
  #Average s for g high divided by s for g low
  out[1,21]<-mean(out[,14])
  
  #Average i for g high divided by i for g low
  out[1,22]<-mean(out[,15])
  
  #Average e for g high divided by e for g low
  out[1,23]<-mean(out[,16])
  
  #Standard Error for average g
  w<-out[,13]
  SEg<-function(w){sd(w)/sqrt(length(w))}
  out[1,24]<-SEg(w)
  
  #Standard Error for average s
  x<-out[,14]
  SEs<-function(x){sd(x)/sqrt(length(x))}
  out[1,25]<-SEs(x)
  
  #Standard Error for average i
  y<-out[,15]
  SEf<-function(y){sd(y)/sqrt(length(y))}
  out[1,26]<-SEf(y)
  
  #Standard Error for average e
  z<-out[,16]
  SEf<-function(z){sd(z)/sqrt(length(z))}
  out[1,27]<-SEf(z)
}
columnames<-c("Species 1,Species 2,Species with highest g,High g s-value,High g i-value,
              High g e-value,Does the species with the highest g have the highest s?,
              Does the species with the highest g have the highest i?,
              Does the species with the highest g have the highest e?,
              g for high g species divded by g for low g species,
              s for high g species divded by s for low g species,
              i for high g species divded by i for low g species,
              e for high g species divded by e for low g species,
              Number of species with high g and high s, Number of species with high g and high i,
              Number of species with high g and high e,Average g for g high divided by g for g low,
              Average s for g high divided by s for g low,Average i for g high divided by i for g low,
              Average e for g high divided by e for g low,Standard Error for average g,
              Standard Error for average s,Standard Error for average i,Standard Error for average e,
              e for Species with highest g,s for Species with highest g,i for Species with highest g,
              Species with low g,Species with high g,g with a change in i,g with a change in s,g with a change in e
              ,g with a change in ie,g with high ies,g with low ies,g with high ies/g with low ies,
              Change in g due to high i/low g,Change in g due to high s/low g,Change in g due to high e/low g,
              Change in g due to high ie/low g")
colnames(out)<-columnames
View(out)
write.csv(out, "EIS_ANGIO_OUTPUT.csv")
