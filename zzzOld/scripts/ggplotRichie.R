#goal is to recreate Derek's graph of 1 meter avg temp for 3 meters,
#then create graph showing avg temp at all depths in one graph

library(ggplot2)
library(dplyr)

#must make R recognize DateTime as date time, POSIXct needed

ot<-read.csv("data/clean/Oxythermal_Cleaned.csv") %>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S"))
str(ot)

#pipe to save creating two arrows?Edit understand pipe now

otrich3<- ot%>%
  filter(Lake=="Richie", Depth=="3")

head(otrich3)
#raw temp of 3 meter Richie

temp3<-ggplot(data=otrich3, mapping=aes(x=DateTime,y=Temp))+
  geom_line()

temp3

#daily avg of 3m temp

avg3<- otrich3%>%
  group_by(Date)%>%
  summarize(mntemp=mean(Temp),
            mnDO=mean(DO)) %>%
  mutate(Date=as.Date(Date))

#plot of avg temp

avg3temp<- ggplot(data=avg3, mapping=aes(x=Date, y=mntemp))+
  geom_line()

avg3temp

# making depth a factor? trying to show all depths in one plot

allD<-ot%>%
  filter(Lake=="Richie")%>%
  mutate(Depth=factor(Depth,levels=c("1","3","5","7","9","11")))

#group by both to have date and depth variables saved

allDtemp<-allD %>%
  group_by(Date,Depth)%>%
  summarize(mntemp=mean(Temp),
            mnDO=mean(DO)) %>%
  mutate(Date=as.Date(Date))

all<-ggplot(data=allD, mapping=aes(x=DateTime,y=Temp, color=Depth))+
  geom_line()

all
  
avgAll<-ggplot(data=allDtemp, mapping=aes(x=Date, y=mntemp, color=Depth))+
  geom_line()+
  scale_y_continuous(name="Average Daily Water Temp (C)")
avgAll

#individual plots for depth
avgAll+
  facet_wrap(vars(Depth))+
  theme_bw()

