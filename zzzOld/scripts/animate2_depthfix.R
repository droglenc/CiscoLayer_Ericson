#loading packages----
library(gganimate)
library(dplyr)
library(FSA)

#filtering data----
ot<-read.csv("data/clean/Oxythermal_Cleaned.csv")%>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S"))

dayavg<-ot%>%
  group_by(Lake,Depth,Date)%>%
  summarize(mnDO=mean(DO),
            mntemp=mean(Temp))%>%
  mutate(Date=as.Date(Date))

days<-subset(dayavg,Date>="2018-06-02"&Date<="2018-09-04")
headtail(dayavg)

richie<-days %>%
  filter(Lake=="Richie")

sargent<-days %>%
  filter(Lake=="Sargent")

siskiwit<-days %>%
  filter(Lake=="Siskiwit")
#richie animation----
ggplot(data=richie,mapping=aes(y=Depth))+
  geom_path(aes(x=mnDO),color="red")+
  geom_path(aes(x=mntemp),color="blue")+
  scale_y_reverse()+
  transition_time(Date)+
  labs(title= "Richie Lake", subtitle="Date: {frame_time}")
#sargent animation----
ggplot(data=sargent,mapping=aes(y=Depth))+
  geom_path(aes(x=mnDO),color="red")+
  geom_path(aes(x=mntemp),color="blue")+
  scale_y_reverse()+
  transition_time(Date)+
  labs(title= "Sargent Lake", subtitle="Date: {frame_time}")
#siskiwit animation----
ggplot(data=siskiwit,mapping=aes(y=Depth))+
  geom_path(aes(x=mnDO),color="red")+
  geom_path(aes(x=mntemp),color="blue")+
  scale_y_reverse()+
  transition_time(Date)+
  labs(title= "Siskiwit Lake", subtitle="Date: {frame_time}")

ggplot(data=days,mapping=aes(y=Depth))+
  geom_path(aes(x=mnDO),color="red")+
  geom_path(aes(x=mntemp),color="blue")+
  scale_y_reverse()+
  facet_wrap(vars(Lake)) +
  transition_time(Date)+
  labs(title="Date: {frame_time}")
