
#setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut")

#Read in dataset
library(readstata13)
hr <- read.dta13("data//gps_hr_20191119_dk.dta")

library(tidyverse)
#Make new cluster variable
hr$clusters <- paste(hr$hv000, hr$hv021, sep="_")

#Change fishing indicator to factor 
hr$fish <- as.factor(hr$fishing_community)

hr1_sum <- 
  hr %>%
  filter(hv000 %in% c("KE6", "MW7", "TZ7", "UG7", "ZM6")) %>%
  select(hv000, hhid, fish, clusters, fishing_community,
         num_hh_members,quintile_nowashnomat,country_) %>%
    group_by(clusters,fish) %>%
    dplyr::summarise(median_hhmembers=median(num_hh_members),
                   median_quintile=median(quintile_nowashnomat),
                   country_=first(country_),na.rm=TRUE)

library(MatchIt)

hr1_sum <- as.data.frame(hr1_sum)
#Match bases on hhmembers,quintile,country
hr1_match <- matchit(fish ~ median_hhmembers + median_quintile + country_,
                data = hr1_sum, method = "nearest",
                ratio = 3) 

summary(hr1_match)

hr1_cc <- match.data(hr1_match)

#Repeat for hr2
hr2_sum <- 
  hr %>%
  filter(hv000 %in% c("KE5", "MW5", "TZ5", "UG6", "ZM5")) %>%
  select(hv000, hhid, clusters, fishing_community,
         num_hh_members,fish,quintile_nowashnomat,country_) %>%
  group_by(clusters, fish) %>%
  dplyr::summarise(median_hhmembers=median(num_hh_members),
                   median_quintile=median(quintile_nowashnomat),
                   country_=first(country_),na.rm=TRUE)

hr2_sum <- as.data.frame(hr2_sum)
#Match bases on hhmembers,quintile,country
hr2_match <- matchit(fish ~ median_hhmembers + median_quintile + country_,
                     data = hr2_sum, method = "nearest",
                     ratio = 3) 

summary(hr2_match)

hr2_cc <- match.data(hr2_match)

all_data <- rbind(hr1_cc, hr2_cc)

write.csv(all_data,"data//propensity_score_matching_20191119.csv")
