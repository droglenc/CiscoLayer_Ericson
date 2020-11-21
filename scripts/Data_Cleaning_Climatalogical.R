## FILE DESCRIPTION HERE

# ----
# Load required packages
library(dplyr)

# ----
# Load data
## Initially load data to get column (i.e., variable) namess ... in nms
nms <- readr::read_csv("data/raw/OJIM4.csv",comment="#") %>%
  colnames()
## Load data ... but skip first 7 lines to get to the actual data ... then
## create column (i.e., variable) names from nms
ojim4 <- readr::read_csv("data/raw/OJIM4.csv",skip=7)
colnames(ojim4) <- nms
ojim4

nms <- readr::read_csv("data/raw/WINM4.csv",comment="#") %>%
  colnames()
winm4 <- readr::read_csv("data/raw/WINM4.csv",skip=7)
colnames(winm4) <- nms

## Put two locations into one file
clim <- rbind(ojim4,winm4)

## may do more manipulations here ##

## Write out to a clean file
write.csv(clim,"data/clean/Climate_Cleaned.csv",row.names=FALSE)
