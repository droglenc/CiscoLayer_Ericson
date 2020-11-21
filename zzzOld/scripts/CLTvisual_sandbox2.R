#loading packages ----
library(dplyr)
library(purrr)
library(ggplot2)
library(gganimate)
library(patchwork)
library(magrittr) #%<>%
library(DataCombine)#sorry

# Filtering data ----
ot <- read.csv("data/clean/Oxythermal_Cleaned.csv") %>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S"))

otdayavg <- ot %>%
  group_by(Date,Depth,Lake) %>%
  summarize(mntemp=mean(Temp),
            mnDO=mean(DO)) %>%
  mutate(Date=as.Date(Date))

# Helper Functions ----
### Find the interpolated depths at critical DO and Temp
findDepthsAtCrits <- function(d,y,xDO,xT,critDO=6,critT=22.8) {
  iFindYatCritX <- function(y,x,crit,dir) {
    if (dir=="greater") cl1 <- which.min(x>=crit)
    else cl1 <- which.max(x<=crit)
    if(x[cl1]<crit) {
      cl2 <- cl1-1
    } else {
      cl2 <- cl1+1
    }
    if (cl2==0) crity <- y[cl1]
    else {
      cl <- c(cl1,cl2)
      cfs <- coef(lm(y[cl]~x[cl]))
      crity <- cfs[["(Intercept)"]]+cfs[[2]]*crit
    }
    crity
  }
  ## Find depth at critical DO
  y <- eval(substitute(y),envir=d)
  xDO <- eval(substitute(xDO),envir=d)
  xT <- eval(substitute(xT),envir=d)
  depthDO <- iFindYatCritX(y,xDO,critDO,dir="less")
  ## Find depth at critical temperature
  depthT <- iFindYatCritX(y,xT,critT,dir="greater")
  ## Put it all together
  tibble(Lake=unique(d$Lake),Date=unique(d$Date),critDO,depthDO,critT,depthT,CLT=abs(depthDO-depthT))
}

# Static plot
##  for one day on one lake ... proof of concept in the graphic
day <- "2018-08-20"
lake <- "Richie"
static1 <- otdayavg %>%
  filter(Lake==lake,Date==day)

## Use helper function to find depths at critical DO and temp
( crits <- findDepthsAtCrits(static1,Depth,mnDO,mntemp) )

## Make a plot ... vertical black segment is the "cisco layer thickness" (CLT)
ggplot() +
  geom_path(data=static1, aes(x=mntemp,y=Depth),color="blue",size=1.5) +
  geom_point(data=static1, aes(x=mntemp,y=Depth),pch=21,color="blue",fill="white",size=2) +
  geom_path(data=static1, aes(x=mnDO,y=Depth),color="red",size=1.5) +
  geom_point(data=static1, aes(x=mnDO,y=Depth),pch=21,color="red",fill="white",size=2) +
  geom_segment(data=crits,mapping=aes(x=critDO,xend=critDO,y=Inf,yend=depthDO),
               linetype="dashed",color="gray70") +
  geom_segment(data=crits,mapping=aes(x=critDO,xend=0,y=depthDO,yend=depthDO),
               linetype="dashed",color="gray70") +
  geom_point(data=crits,mapping=aes(x=critDO,y=depthDO),size=2.5) +
  geom_segment(data=crits,mapping=aes(x=critT,xend=critT,y=Inf,yend=depthT),
               linetype="dashed",color="gray70") +
  geom_segment(data=crits,mapping=aes(x=critT,xend=0,y=depthT,yend=depthT),
               linetype="dashed",color="gray70") +
  geom_point(data=crits,mapping=aes(x=critT,y=depthT),size=2.5) +
  geom_segment(data=crits,mapping=aes(x=0,xend=0,y=depthDO,yend=depthT),size=2.5) +
  scale_y_continuous(name="Depth (m)",trans="reverse",
                     limits=c(NA,0),expand=expansion(mult=c(0.02,0))) +
  scale_x_continuous(name="Dissolved Oxygen (mg/L) / Temperature (C)") +
  labs(title=paste(lake,"Lake on",day)) +
  theme_classic()


# Animated graphic ----
## Restrict to one lake (for now)
tmp <- otdayavg %>%
  filter(Lake=="Richie")

## Find CLT by date
### Must first find vector of unique dates
( dates <- unique(tmp$Date) )
### Find the depths at the critical DO and temperature
crits <- map_df(.x=dates,
        .f=~{
          tmp %>%
            filter(Date==.x) %>%
            findDepthsAtCrits(Depth,mnDO,mntemp)
        }
)

crits

## Make the plot
ggplot() +
  geom_path(data=tmp, aes(x=mntemp,y=Depth),color="blue",size=1.5) +
  geom_point(data=tmp, aes(x=mntemp,y=Depth),pch=21,color="blue",fill="white",size=2) +
  geom_path(data=tmp, aes(x=mnDO,y=Depth),color="red",size=1.5) +
  geom_point(data=tmp, aes(x=mnDO,y=Depth),pch=21,color="red",fill="white",size=2) +
  geom_segment(data=crits,mapping=aes(x=critDO,xend=critDO,y=Inf,yend=depthDO),
               linetype="dashed",color="gray70") +
  geom_segment(data=crits,mapping=aes(x=critDO,xend=0,y=depthDO,yend=depthDO),
               linetype="dashed",color="gray70") +
  geom_point(data=crits,mapping=aes(x=critDO,y=depthDO),size=2.5) +
  geom_segment(data=crits,mapping=aes(x=critT,xend=critT,y=Inf,yend=depthT),
               linetype="dashed",color="gray70") +
  geom_segment(data=crits,mapping=aes(x=critT,xend=0,y=depthT,yend=depthT),
               linetype="dashed",color="gray70") +
  geom_point(data=crits,mapping=aes(x=critT,y=depthT),size=2.5) +
  geom_segment(data=crits,mapping=aes(x=0,xend=0,y=depthDO,yend=depthT),size=2.5) +
  scale_y_continuous(name="Depth (m)",trans="reverse",
                     limits=c(NA,0),expand=expansion(mult=c(0.02,0))) +
  scale_x_continuous(name="Dissolved Oxygen (mg/L) / Temperature (C)") +
  labs(title=paste(lake,"Lake on {frame_time}")) +
  theme_classic() +
  gganimate::transition_time(Date)


ggplot(data=crits,mapping=aes(x=Date)) +
  geom_segment(mapping=aes(xend=Date,y=depthT,yend=depthDO),
               size=1.25,color="gray50") +
  geom_path(mapping=aes(y=depthT),color="blue",size=1.25) +
  geom_path(mapping=aes(y=depthDO),color="red",size=1.25) +
  scale_y_continuous(name="Depth (m)",trans="reverse",
                     limits=c(NA,0),expand=expansion(mult=c(0.02,0))) +
  labs(title=paste(lake,"Lake")) +
  theme_classic() +
  annotate(geom="text",x=as.Date("2018-07-15"),y=7.5,hjust="left",
           label="Minimum Depth with DO <6 mg/l") +
  annotate(geom="segment",x=as.Date("2018-07-15"),y=7.5,
           xend=as.Date("2018-07-01"),yend=5.7,
           size=0.25,arrow=arrow(length=unit(2,"mm"),type="closed")) +
  annotate(geom="text",x=as.Date("2018-07-1"),y=0.25,hjust="left",
           label="Maximum Depth with temperature <22.8C") +
  annotate(geom="segment",x=as.Date("2018-07-1"),y=0.25,
           xend=as.Date("2018-06-15"),yend=0.95,
           size=0.25,arrow=arrow(length=unit(2,"mm"),type="closed")) +
  annotate(geom="label",x=as.Date("2018-06-1"),y=3.5,hjust="left",
           label="CLT",size=8)

#Patchwork----
clim<-read.csv("data/clean/CLimate_Cleaned.csv")%>%
  mutate(Date=as.POSIXct(Date_Time,format="%m/%d/%Y %H:%M"))
clim<-change(data=clim,
             Var="precip_accum_set_1",
             TimeVar="Date",
             type="absolute")
clim%<>%
  rename(airtemp=air_temp_set_1,windspeed=wind_speed_set_1,
         winddir=wind_direction_set_1,windgust=wind_gust_set_1,
         precipaccum=precip_accum_set_1,peakwind=peak_wind_speed_set_1,
         peakwinddir=peak_wind_direction_set_1,precipchange=`precip_accum_set_1_PChangeFrom-1`)
clim<-clim%>%
  mutate(Date=as.Date(Date,format="%Y-%m-%d",tz='UTC'))%>%
  group_by(Date,precipchange,precipaccum,airtemp,windspeed,winddir,windgust,peakwind,
           peakwinddir)

dayclim<-clim%>%
  group_by(Date)%>%
  summarize(avgairtemp=mean(airtemp),
            avgwinddir=mean(winddir,na.rm=TRUE),peakwindgust=max(windgust),
            peakwinddir=max(peakwinddir),dayprecip=max(precipaccum),
            precipchange=sum(precipchange),meanwindspeed=mean(windspeed))%>%
  mutate(rainfalltot=dayprecip-3.52)
dayclim<-subset(dayclim,Date>="2018-06-02"&Date<="2018-09-04")%>%
  mutate(rain=ifelse(precipchange>0,"yes","no"))
dayclim<-dayclim%>%
  mutate(airtempC=(avgairtemp-32)*(5/9))

otdayavg<-subset(otdayavg,Date>="2018-06-02"&Date<="2018-09-04")
richie<-otdayavg%>%
  filter(Lake=='Richie')
climot<-full_join(richie,dayclim,by='Date')
surf<-climot%>%
  filter(Depth=='1')
#plots
pCLT<-ggplot(data=crits,mapping=aes(x=Date)) +
  geom_segment(mapping=aes(xend=Date,y=depthT,yend=depthDO),
               size=1.25,color="gray50") +
  geom_path(mapping=aes(y=depthT),color="blue",size=1.25) +
  geom_path(mapping=aes(y=depthDO),color="red",size=1.25) +
  labs(x=element_blank(),
       y=element_blank())+
  scale_y_continuous(name="Depth (m)",trans="reverse",
                     limits=c(NA,0),expand=expansion(mult=c(0.02,0))) +
  labs(title=paste(lake,"Lake")) +
  theme_classic() +
  annotate(geom="text",x=as.Date("2018-07-15"),y=7.5,hjust="left",
           label="Minimum Depth with DO <6 mg/l") +
  annotate(geom="segment",x=as.Date("2018-07-15"),y=7.5,
           xend=as.Date("2018-07-01"),yend=5.7,
           size=0.25,arrow=arrow(length=unit(2,"mm"),type="closed")) +
  annotate(geom="text",x=as.Date("2018-07-1"),y=0.25,hjust="left",
           label="Maximum Depth with temperature <22.8C") +
  annotate(geom="segment",x=as.Date("2018-07-1"),y=0.25,
           xend=as.Date("2018-06-15"),yend=0.95,
           size=0.25,arrow=arrow(length=unit(2,"mm"),type="closed")) +
  annotate(geom="label",x=as.Date("2018-06-1"),y=3.5,hjust="left",
           label="CLT",size=8)
pCLT

pAirTemp<-ggplot(data=surf,mapping=aes(x=Date))+
  geom_path(aes(y=airtempC),color="blue")+
  #geom_path(aes(y=mntemp),color='red')+
  geom_point(aes(y=airtempC,color=rain))+
  theme_classic()+
  labs(x=element_blank(),
       y=element_blank())+
  scale_y_continuous(name='Air Temperature (C)')#fix legend, increase point/line size to match?
  
pRain<-ggplot(data=surf,mapping=aes(x=Date,y=precipchange))+
  geom_path()+
  theme_classic()+
  scale_y_continuous(name='Daily Rain Total')
pRain


patch<-pCLT/pAirTemp/pRain
patch
ggsave("patch.jpeg",height=10,width=7)
