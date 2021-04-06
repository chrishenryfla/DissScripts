setwd("~/Documents/K_Vals")
library(ggplot2)
KValSheet<-read.csv("ARPR.csv",header = TRUE)
KValavgSheet<-read.csv("ARPR_avg.csv",header = TRUE)

#all points

dff<-as.data.frame(KValSheet[,1:2])
dfr<-as.data.frame(KValSheet[,3:4])
ggplot()+
  geom_point(data=dff, aes(x=time,y=K), color = "blue")+
  geom_line(data=dfr, aes(x=time_pow,y=k_pow), color = "red")

#average value per time bar graph

df<-data.frame(
  time=KValavgSheet$time, 
  k=KValavgSheet$K_avg_no_neg, 
  min=KValavgSheet$MIN, 
  max=KValavgSheet$MAX)

ggplot(df) +
  geom_bar( aes(x=time, y=k), stat="identity", fill="skyblue", alpha=0.7)+ 
  geom_errorbar( aes(x=time, ymin=min, ymax=max), width=0.4, colour="orange", alpha=0.9, size=1.3)
