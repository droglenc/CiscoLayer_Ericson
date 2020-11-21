## Load packages
library(ggplot2)
library(dplyr)

## Load cleaned oxythermal data
ot <- read.csv("data/clean/OxyThermal_Cleaned.csv") %>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S"))
str(ot)

## Just Richie Lake and 1-m depth
otrich1 <- ot %>%
  filter(Lake=="Richie",Depth==1)

# raw plot of temperature
ggplot(data=otrich1,mapping=aes(x=DateTime,y=Temp)) +
  geom_line(color="darkblue") +
  scale_y_continuous(name="Temperature (C)") +
  scale_x_datetime(name="Date of Recording") +
  theme_bw()

# get mean temp/DO by day
otrich2 <- otrich %>%
  group_by(Date) %>%
  summarize(mnTemp=mean(Temp),
            mnDO=mean(DO)) %>%
  mutate(Date=as.Date(Date))

ggplot(data=otrich2,mapping=aes(x=Date,y=mnTemp)) +
  geom_line(color="darkblue") +
  scale_y_continuous(name="Average Daily Temperature (C)") +
  scale_x_date(name="Day of Recording") +
  theme_bw()
