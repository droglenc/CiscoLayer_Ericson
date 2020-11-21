# Loading packages and helper functions ----
library(dplyr)
library(purrr)

library(ggplot2)
library(patchwork)

source("scripts/helpers.R")

# Get the data ----
## Set range of good dates
### Restrict oxythermal data to these dates to remove burn-in and take-out
### Restrict weather data to these dates to speed things up
start_day <- "2018-06-02"
end_day <- "2018-09-04"

## Oxythermal raw (but cleaned) data
### Restrict to only "good" days
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


## Environmental / Weather raw (but cleaned) data
### Fix the date-times, get just the date
### Make sure sorted by Date-Time so that lag works properly
### Rename some variables for simplicity
### Compute the hourly precip from the accumulated precip
###  (must have condition to deal with accumulation starting on 1-jan each year)
### Convert air temperture to C
### Remove variables that we don't plan to use
clim <- read.csv("data/clean/CLimate_Cleaned.csv")%>%
  mutate(Date_Time=as.POSIXct(Date_Time,format="%m/%d/%Y %H:%M"),
         Date=as.Date(Date_Time,format="%Y-%m-%d",tz='UTC')) %>%
  filter(Date>=start_day,Date<=end_day) %>%
  arrange(Station_ID,Date_Time) %>%
  rename(airtemp=air_temp_set_1,precipaccum=precip_accum_set_1,
         windspeed=wind_speed_set_1,winddir=wind_direction_set_1,
         windgust=wind_gust_set_1,peakwind=peak_wind_speed_set_1,
         peakwinddir=peak_wind_direction_set_1,
         dewpt=dew_point_temperature_set_1d,solrad=solar_radiation_set_1) %>%
  mutate(precip_hr=precipaccum-lag(precipaccum),
         precip_hr=ifelse(precip_hr<0 | is.na(precip_hr),0,precip_hr),
         airtemp=(airtemp-32)*(5/9)) %>%
  select(-contains("snow"),-contains("fuel"),-contains("humidity"),
         -volt_set_1,-wind_chill_set_1d,-wind_cardinal_direction_set_1d,
         -heat_index_set_1d,-water_temp_set_1)

## Environmental / Weather daily summaries
climday <- clim %>%
  group_by(Station_ID,Date) %>%
  summarize(avgairtemp=mean(airtemp,na.rm=TRUE),
            avgwinddir=mean(winddir,na.rm=TRUE),
            peakwindgust=max(windgust,na.rm=TRUE),
            peakwinddir=max(peakwinddir,na.rm=TRUE),
            avgwindspeed=mean(windspeed,na.rm=TRUE),
            precip=sum(precip_hr,na.rm=TRUE)) %>%
  mutate(rain=ifelse(precip>0,"yes","no")) %>%
  arrange(Station_ID,Date)

##!! Clean-up
rm(clim)

### Oh-oh ... two stations ... how do they compare
ggplot(data=climday,mapping=aes(x=Date,y=avgairtemp,color=Station_ID)) +
  geom_path()
ggplot(data=climday,mapping=aes(x=Date,y=precip,color=Station_ID)) +
  geom_path()
ggplot(data=climday,mapping=aes(x=Date,y=avgwindspeed,color=Station_ID)) +
  geom_path()


## Join Environmental/Weather data with CLT data
### For the time-being just chose one of the weather stations
d <- right_join(otCLT,filter(climday,Station_ID=="OJIM4"),by="Date")



# Make Plots ----
dRich <- filter(d,Lake=="Richie")

pCLT <- ggplot(data=dRich,mapping=aes(x=Date,y=CLT)) +
  geom_path(color="blue") +
  scale_y_continuous(name='Cisco Layer Thickness (m)') +
  theme_classic()
pCLT

pAirTemp <- ggplot(data=dRich,mapping=aes(x=Date,y=avgairtemp)) +
  geom_path(color="blue") +
  scale_y_continuous(name='Air Temperature (C)') +
  theme_classic()
pAirTemp

pRain <- ggplot(data=dRich,mapping=aes(x=Date,y=precip)) +
  geom_path(color="blue") +
  scale_y_continuous(name='Precipitation (in)') +
  theme_classic()
pRain

pCLT/pAirTemp/pRain

ggplot(data=dRich,aes(x=avgairtemp,y=CLT)) +
  geom_point()


## Other lakes !!!!
ggplot(data=d,aes(x=avgairtemp,y=CLT)) +
  geom_point() +
  facet_wrap(vars(Lake))

ggplot(data=d,aes(x=CLT)) +
  geom_histogram() +
  facet_wrap(vars(Lake))
