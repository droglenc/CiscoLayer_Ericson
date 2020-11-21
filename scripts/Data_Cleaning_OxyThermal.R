## FILE DESCRIPTION HERE

# ----
# Load required packages
library(dplyr)
library(magrittr) ## for %<>%

# ----
# Load data
## Richie Lake
fns <- data.frame(fn=c("7450-338212.txt","7450-506308.txt","7450-730440.txt",
                       "7450-855854.txt","7450-854914.txt","7450-514202.txt"),
                  depth=c(1,3,5,7,9,11))
lake <- "Richie"
df_r <- NULL
for (i in 1:nrow(fns)) {
  cat("Reading file:",fns$fn[i],"\n")
  tmp <- read.table(paste0("data/raw/",lake,"/",fns$fn[i]),
                    header=TRUE,na.strings="NA") %>%
    mutate(Lake=lake,Depth=fns$depth[i])
  df_r <- rbind(df_r,tmp)
}

## Sargent Lake
sarD<-data.frame(fn=c("7450-527969.txt","7450-457033.txt","7450-433226.txt",
                      "7450-326790.txt","7450-708861.txt","7450-826935.txt",
                      "7450-821901.txt"),
           depth=c(1,3,5,7,9,11,13))
lake<- "Sargent"
df_sa<- NULL

for (i in 1:nrow(sarD)) {
  cat("Reading file:",sarD$fn[i],"\n")
  tmp <- read.table(paste0("data/raw/",lake,"/",sarD$fn[i]),
                    header=TRUE,na.strings="NA") %>%
    mutate(Lake=lake,Depth=sarD$depth[i])
  df_sa <- rbind(df_sa,tmp)
}

## Siskiwit Lake
sisD<-data.frame(fn=c("7450-813606.txt","7450-541933.txt","7450-636945.txt",
                      "7450-639850.txt","7450-328801.txt","7450-870449.txt",
                      "7450-772031.txt","7450-354904.txt","7450-506814.txt",
                      "7450-668203.txt","7450-552282.txt","7450-464014.txt",
                      "7450-863458.txt","7450-390893.txt"),
                 depth=c(1,3,5,7,9,11,13,15,17,22,27,32,37,42))

lake<- "Siskiwit"
df_si<-NULL

for (i in 1:nrow(sisD)) {
  cat("Reading file:",sisD$fn[i],"\n")
  tmp <- read.table(paste0("data/raw/",lake,"/",sisD$fn[i]),
                    header=TRUE,na.strings="NA") %>%
    mutate(Lake=lake,Depth=sisD$depth[i])
  df_si <- rbind(df_si,tmp)
}

## Put all three lakes together
oxytherm <- rbind(df_r,df_sa,df_si)

## Some cleaning before outputing
## Changed some variable names (to reduce typing later)
## Added a combined date and time variable
## Removed variables that don't appear to be needed for further analysis
## Moved Lake and Depth to the beginning
oxytherm %<>%
  rename(Date=Date.CST,Time=Time.CST,Temp=Temperature,DO=Dissolved.Oxygen,
         DO_Sat=Dissolved.Oxygen.Saturation) %>%
  mutate(DateTime=paste(Date,Time)) %>%
  select(Lake, Depth,Date,Time,DateTime,Temp,DO,DO_Sat,Q)


## Write out a cleaned file
write.csv(oxytherm,"data/clean/OxyThermal_Cleaned.csv",row.names=FALSE)
