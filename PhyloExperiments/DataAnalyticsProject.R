library(tidyverse)
library(modelr)
phylobase<-read.csv("Phylobase.csv")

#Manipulate database

traitbase<-tibble(phylobase %>%
  rename(FullSpecies = X) %>%
  select(FullSpecies,
         Leaf.Length..cm..max,
         Leaf.Width..cm..max,
         Fruit.length..mm..max,
         P.A..both.) %>%
  filter(!is.na(Leaf.Length..cm..max) & 
           !is.na(Leaf.Width..cm..max) & 
           !is.na(P.A..both.)
         ) %>%
  subset(Fruit.length..mm..max != ".") %>%
  transform(Fruit.length..mm..max = as.numeric(Fruit.length..mm..max)) %>%
  filter(Fruit.length..mm..max < 50 & 
           Leaf.Length..cm..max < 125)
)

#Plot Variable
ggplot(traitbase, aes(Leaf.Length..cm..max,Fruit.length..mm..max))+
         geom_point()

modelpars <- tibble(
  p1 = runif(250, 10, 30),
  p2 = runif(250, -10, 0)
)

#Make a model to describe the data
newmodel <- function(p, data) {
  p[1]+(p[2]*data$Leaf.Length..cm..max)
  #p[1]*(p[2]*exp(data$Leaf.Length..cm..max)
}
newmodel(c(7, 1.5), traitbase)

measure_distance <- function(mod, data) {
  diff <- data$Fruit.length..mm..max - newmodel(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), traitbase)

traitbase_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), traitbase)
}

Exponential <-function (A,B,psi){A*exp(-B*psi)}
define_parsE <- function(inputCol){
  
  parsE <- list(A=max(inputCol),B=1, sd=2)
  par_loE =list(A=0, B=0.01,sd=0.0005) 
  par_highE =list(A=max(inputCol)*2,B=10,sd=100)
  return(list(parsE=parsE, par_loE=par_loE,par_highE=par_highE))
} 


modelpars <- modelpars %>% 
  mutate(dist = purrr::map2_dbl(p1, p2, traitbase_dist))
modelpars

ggplot(traitbase, aes(Leaf.Length..cm..max,Fruit.length..mm..max)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = p1, slope = p2, colour = -dist), 
    data = filter(modelpars, rank(dist) <= 10)
  ) +
  theme_bw()
