#loading packages and helper functions ----
library(dplyr)
library(purrr)

library(ggplot2)
library(gganimate)

source("scripts/helpers.R")

# Load data ----
## Set range of good dates
### Restrict oxythermal data to these dates to remove burn-in and take-out
start_day <- "2018-06-02"
end_day <- "2018-09-04"

## Oxythermal raw (but cleaned) data
ot <- read.csv("data/clean/Oxythermal_Cleaned.csv") %>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S")) %>%
  filter(Date>=start_day,Date<=end_day)

## Oxythermal daily summaries (by depth)
### Find mean temperature and mean DO (across hourly recordings)
otday <- ot %>%
  group_by(Lake,Date,Depth) %>%
  summarize(mntemp=mean(Temp),
            mnDO=mean(DO)) %>%
  mutate(Date=as.Date(Date))

## Get CLT "width" for each lake and date
otCLT <- cltByDay(otday)

##!! Cleanup
rm(ot)


# One Day, One Lake, Static ----
## Primarily for exploration, but could be used for an illustration
lake <- "Sargent"
day <- "2018-07-25"
otday1 <- otday %>% filter(Lake==lake,Date==day)
otCLT1 <- otCLT %>% filter(Lake==lake,Date==day)

## Make a plot ... vertical black segment is the "cisco layer thickness" (CLT)
ggplot() +
  geom_path(data=otday1, aes(x=mntemp,y=Depth),color="blue",size=1.5) +
  geom_point(data=otday1, aes(x=mntemp,y=Depth),
             pch=21,color="blue",fill="white",size=2) +
  geom_path(data=otday1, aes(x=mnDO,y=Depth),color="red",size=1.5) +
  geom_point(data=otday1, aes(x=mnDO,y=Depth),
             pch=21,color="red",fill="white",size=2) +
  geom_segment(data=otCLT1,mapping=aes(x=critDO,xend=critDO,y=Inf,yend=depthDO),
               linetype="dashed",color="gray70") +
  geom_segment(data=otCLT1,mapping=aes(x=critDO,xend=0,y=depthDO,yend=depthDO),
               linetype="dashed",color="gray70") +
  geom_point(data=otCLT1,mapping=aes(x=critDO,y=depthDO),size=2.5) +
  geom_segment(data=otCLT1,mapping=aes(x=critT,xend=critT,y=Inf,yend=depthT),
               linetype="dashed",color="gray70") +
  geom_segment(data=otCLT1,mapping=aes(x=critT,xend=0,y=depthT,yend=depthT),
               linetype="dashed",color="gray70") +
  geom_point(data=otCLT1,mapping=aes(x=critT,y=depthT),size=2.5) +
  geom_segment(data=otCLT1,mapping=aes(x=0,xend=0,y=depthDO,yend=depthT),
               size=2.5) +
  scale_y_continuous(name="Depth (m)",trans="reverse",
                     limits=c(NA,0),expand=expansion(mult=c(0.02,0))) +
  scale_x_continuous(name="Dissolved Oxygen (mg/L) / Temperature (C)") +
  labs(title=paste(lake,"Lake on",day)) +
  theme_classic()


# One Day, One Lake ----
## Primarily for exploration, but could be used for an illustration
lake <- "Sargent"
otCLT1 <- otCLT %>% filter(Lake==lake)

ggplot(data=otCLT1,mapping=aes(x=Date)) +
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


# One Day, One Lake, Animated ----
## Primarily for exploration, but could be used for an illustration
lake <- "Siskiwit"
otday1 <- otday %>% filter(Lake==lake)
otCLT1 <- otCLT %>% filter(Lake==lake)

## Make the plot
ggplot() +
  geom_path(data=otday1, aes(x=mntemp,y=Depth),color="blue",size=1.5) +
  geom_point(data=otday1, aes(x=mntemp,y=Depth),
             pch=21,color="blue",fill="white",size=2) +
  geom_path(data=otday1, aes(x=mnDO,y=Depth),color="red",size=1.5) +
  geom_point(data=otday1, aes(x=mnDO,y=Depth),
             pch=21,color="red",fill="white",size=2) +
  geom_segment(data=otCLT1,mapping=aes(x=critDO,xend=critDO,y=Inf,yend=depthDO),
               linetype="dashed",color="gray70") +
  geom_segment(data=otCLT1,mapping=aes(x=critDO,xend=0,y=depthDO,yend=depthDO),
               linetype="dashed",color="gray70") +
  geom_point(data=otCLT1,mapping=aes(x=critDO,y=depthDO),size=2.5) +
  geom_segment(data=otCLT1,mapping=aes(x=critT,xend=critT,y=Inf,yend=depthT),
               linetype="dashed",color="gray70") +
  geom_segment(data=otCLT1,mapping=aes(x=critT,xend=0,y=depthT,yend=depthT),
               linetype="dashed",color="gray70") +
  geom_point(data=otCLT1,mapping=aes(x=critT,y=depthT),size=2.5) +
  geom_segment(data=otCLT1,mapping=aes(x=0,xend=0,y=depthDO,yend=depthT),
               size=2.5) +
  scale_y_continuous(name="Depth (m)",trans="reverse",
                     limits=c(NA,0),expand=expansion(mult=c(0.02,0))) +
  scale_x_continuous(name="Dissolved Oxygen (mg/L) / Temperature (C)") +
  labs(title=paste(lake,"Lake on {frame_time}")) +
  theme_classic() +
  gganimate::transition_time(Date)




