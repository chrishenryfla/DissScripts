{
setwd("~/Desktop")
library(ggplot2)  
HabitatDiversity<-c(5,3,1)
TraitConservatism1<-c(1,3,5)
TraitConservatism2<-c(2,3,4)
TraitConservatism3<-c(3,3,3)
TraitConservatism4<-c(2.2,3.2,4.2)
TraitConservatism5<-c(1.5,2.5,3.5)
LeafArea<-c(4.5,3.5,2.5)
VeinDensity<-c(1.2,2.2,3.2)
LeafTeeth<-c(.6,1.6,2.6)
LeafMA<-c(1,2,3)
StomatalDensity<-c(2.5,3.5,4.5)
TraitVal<-c(1,3,5)

df<-data.frame(HabitatDiversity,TraitConservatism1,TraitConservatism2,
               TraitConservatism3,TraitConservatism4,TraitConservatism5,
               LeafArea,TraitVal,VeinDensity,LeafTeeth,LeafMA,StomatalDensity)

p1<-ggplot(df,aes(HabitatDiversity))+
  geom_line(aes(y = TraitConservatism1), color = "dark red",size = 1.36)+
  geom_line(aes(y = TraitConservatism2), color = "blue",size = 1.36)+
  geom_line(aes(y = TraitConservatism3), color = "green",size = 1.36)+
  geom_line(aes(y = TraitConservatism4), color = "purple",size = 1.36)+
  geom_line(aes(y = TraitConservatism5), color = "orange",size = 1.36)
pp<-(p1 + theme(
  plot.title = element_text(size =30, face = "bold"),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_text(size = 30, face = "bold"),
  axis.title.y = element_text(size = 30, face = "bold"))+
  xlab("Habitat Diversity") +ylab("Trait Conservatism"))+ggtitle("A")

p2<-ggplot(df,aes(TraitVal))+ 
  geom_line(aes(y = LeafArea), color = "dark red",size = 1.36)+
  geom_line(aes(y = VeinDensity), color = "blue",size = 1.36)+
  geom_line(aes(y = LeafTeeth), color = "green",size = 1.36)+
  geom_line(aes(y = LeafMA), color = "purple",size = 1.36)+
  geom_line(aes(y = StomatalDensity), color = "orange",size = 1.36)
ppp<-(p2 + theme(
  plot.title = element_text(size = 30, face = "bold"),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_text(size = 30, face = "bold"),
  axis.title.y = element_text(size = 30, face = "bold"))+
  xlab("Aridity") +ylab("Trait Value"))+ggtitle("B")

library(gridExtra)
ggsave("p.jpeg",arrangeGrob(pp,ppp,ncol = 2),width = 11, height = 8)
}
