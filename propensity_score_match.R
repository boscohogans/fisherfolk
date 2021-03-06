
# Propensity Score Matching -----------------------------------------------


hr_prop_data <- hr_fish_gps_upd %>%
  #Make new cluster variable
  mutate(clusters=paste(hv000, hv021, sep="_"),
         #Change fishing indicator to factor 
         #fishing_community_5=ifelse(is.na(fishing_community_5),0,fishing_community_5),
         fish_factor=as.factor(fishing_community_5),
         round=ifelse(hv000 %in% c("KE5", "MW5", "TZ5", "UG6", "ZM5"),1,2)) %>%
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

