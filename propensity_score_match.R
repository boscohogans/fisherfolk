
#setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut")

#Read in dataset
#library(readstata13)
#hr <- read.dta13("data//gps_hr_20191120_dk.dta")

##library(tidyverse)
hr_fish_gps <- hr_fish_gps %>%
  #Make new cluster variable
  mutate(clusters=paste(hv000, hv021, sep="_"),
         #Change fishing indicator to factor 
         fishing_community=ifelse(is.na(fishing_community),0,fishing_community),
         fish_factor=as.factor(fishing_community)) %>%
  rename(num_hh_members=hv009)


#Split up based on latest round of DHS survey

hr1_sum <- 
  hr_fish_gps %>%
  filter(hv000 %in% c("KE6", "MW7", "TZ7", "UG7", "ZM6")) %>%
  select(hv000, hhid, fish_factor, clusters,
         num_hh_members,asset_index_nowashnomat,country) %>%
    group_by(clusters,fish_factor) %>%
    dplyr::summarise(median_hhmembers=median(num_hh_members),
                   mean_ses=mean(asset_index_nowashnomat),
                   country=first(country),na.rm=TRUE)

library(MatchIt)

hr1_sum <- as.data.frame(hr1_sum)
#Match bases on hhmembers,quintile,country
hr1_match <- matchit(fish_factor ~ median_hhmembers + mean_ses + country,
                data = hr1_sum, method = "nearest",
                ratio = 3) 

summary(hr1_match)

hr1_cc <- match.data(hr1_match)

#Repeat for hr2
hr2_sum <- 
  hr_fish_gps %>%
  filter(hv000 %in% c("KE5", "MW5", "TZ5", "UG6", "ZM5")) %>%
  select(hv000, hhid, clusters, fish_factor,
         num_hh_members,asset_index_nowashnomat,country) %>%
  group_by(clusters,fish_factor) %>%
  dplyr::summarise(median_hhmembers=median(num_hh_members),
                   mean_ses=mean(asset_index_nowashnomat),
                   country=first(country),na.rm=TRUE)

hr2_sum <- as.data.frame(hr2_sum)
#Match bases on hhmembers,quintile,country
hr2_match <- matchit(fish_factor ~ median_hhmembers + mean_ses + country,
                     data = hr2_sum, method = "nearest",
                     ratio = 3) 

summary(hr2_match)

hr2_cc <- match.data(hr2_match)

propensity_score <- rbind(hr1_cc, hr2_cc)

rm(list=ls(pattern="hr1"))
rm(list=ls(pattern="hr2"))

#write.csv(all_data,"data//propensity_score_matching.csv")
