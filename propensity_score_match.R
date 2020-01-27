
#setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut")

#Read in dataset
#library(readstata13)
#hr <- read.dta13("data//gps_hr_20191120_dk.dta")

##library(tidyverse)
hr_prop_data <- hr_fish_gps_upd %>%
  #Make new cluster variable
  mutate(clusters=paste(hv000, hv021, sep="_"),
         #Change fishing indicator to factor 
         fishing_community=ifelse(is.na(fishing_community),0,fishing_community),
         fish_factor=as.factor(fishing_community),
         round=ifelse(hv000 %in% c("KE6", "MW7", "TZ7", "UG7", "ZM6"),2,1)) %>%
  select(hv000,round, hhid, fish_factor, clusters,hv009,asset_index_nowashnomat,country)%>%
  group_by(clusters,fish_factor,round) %>%
  summarise(median_hhmembers=median(hv009),
                   mean_ses=mean(asset_index_nowashnomat),
                   country=first(country),na.rm=TRUE)

#Round 1
hr1_sum <- as.data.frame(subset(hr_prop_data, round==1))
#Match bases on hhmembers,quintile,country
hr1_match <- matchit(fish_factor ~ median_hhmembers + mean_ses + country,
                data = hr1_sum, method = "nearest",
                ratio = 3) 

summary(hr1_match)
hr1_cc <- match.data(hr1_match)

#Repeat for Round 2
hr2_sum <- as.data.frame(subset(hr_prop_data, round==2))
#Match bases on hhmembers,quintile,country
hr2_match <- matchit(fish_factor ~ median_hhmembers + mean_ses + country,
                     data = hr2_sum, method = "nearest",
                     ratio = 3) 

summary(hr2_match)
hr2_cc <- match.data(hr2_match)

propensity_score <- rbind(hr1_cc, hr2_cc)

rm(list=ls(pattern="hr1"))
rm(list=ls(pattern="hr2"))
rm(hr_prop_data)

