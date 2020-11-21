#loading packages----
library(gganimate)
library(dplyr)
library(FSA)
#library(transformr)
#library(glue)

#filtering data----
ot<-read.csv("data/clean/Oxythermal_Cleaned.csv") %>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S"))
str(ot)

otdayavg<-ot %>%
  group_by(Date,Depth,Lake) %>%
  summarize(mntemp=mean(Temp),
            mnDO=mean(DO)) %>%
  mutate(Date=as.Date(Date))
headtail(otdayavg)

rdayavg<-otdayavg %>%
  filter(Lake=="Richie")

sardayavg<-otdayavg %>%
  filter(Lake=="Sargent")

sidayavg<-otdayavg %>%
  filter(Lake=="Siskiwit")

#wrong transition state----
ggplot(data=rdayavg,mapping=aes(y=Depth))+
  geom_line(aes(x=mntemp),color="blue")+
  geom_line(aes(x=mnDO),color="red")+
  scale_y_reverse()+
  transition_states(Date,
                    transition_length=1,
                    state_length=4)+

  ggtitle("Richie Lake",
  subtitle="Date: {closest_state}")

#trying something new, IT WORKED
#Richie animation----

ggplot(data=rdayavg,mapping=aes(y=Depth))+
  geom_path(aes(x=mntemp),color="blue")+
  geom_path(aes(x=mnDO),color="red")+
  geom_vline(xintercept=22.8)+
  geom_vline(xintercept=6)+
  scale_y_reverse()+
  transition_time(Date)+
  #ggtitle(rdayavg$Date)+
  labs(title= "Richie Lake", subtitle="Date: {frame_time}")

 static<-rdayavg %>% 
   filter(Date=="2018-08-13")
 
 ggplot(data=static, mapping=aes(y=Depth))+
   geom_path(aes(x=mntemp),color="blue")+
   geom_path(aes(x=mnDO),color="red")+
   geom_vline(xintercept=22.8)+
   geom_vline(xintercept=6)+
   scale_y_reverse()+
   
   labs(title= "Richie Lake", subtitle="Date:2018-08-13")
   
   
#Sargent animation----

ggplot(data=sardayavg,mapping=aes(y=Depth))+
  geom_path(aes(x=mntemp),color="blue")+
  geom_path(aes(x=mnDO),color="red")+
  scale_y_reverse()+
  transition_time(Date)+
  labs(title= "Sargent Lake", subtitle="Date: {frame_time}")

#Siskiwit animation ----

ggplot(data=sidayavg,mapping=aes(y=Depth))+
  geom_path(aes(x=mntemp),color="blue")+
  geom_path(aes(x=mnDO),color="red")+
  scale_y_reverse()+
  transition_time(Date)+
  labs(title= "Siskiwit Lake", subtitle="Date: {frame_time}")


 
 



#moving segments----

#can filter... moving point/line is impossible.
ggplot(data=rdayavg,mapping=aes(x=mntemp,y=Depth))+
  geom_path()+
  geom_point()+
  #scale_fill_gradient2(name="DO (mg/L)", low="red", mid='white', high="blue", space = 'rgb', guide = "colourbar", midpoint = 6, breaks=c(3,6,9), labels=c(3,6,9))+
  #geom_point(aes(x=6))+
  geom_segment(aes(x=6,y=Depth,xend=max(mnDO),yend=Depth))+
  scale_y_reverse()+
  transition_time(Date)

warnings()
