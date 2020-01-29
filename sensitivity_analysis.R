#Sensitivity analysis

mr_sensitivty_analysis <- readstata13::read.dta13("data\\DHS\\MR\\KEMR70FL.DTA") %>% 
  #Select variables of interest
  select(mv025, mv716, mv000, mv007, mv001) %>% 
  #Filter on rural communities
  filter(mv025=="rural") %>% 
  #Generate new variables for identifying fisherfolk and country
  mutate(fishing=ifelse(grepl("^fishery",mv716),1,0),
         country=substr(mv000,1,2)) %>%
  #Merge with fishing clusters
  left_join(fish_village_final, by=c("country"="DHSCC", "mv007"="DHSYEAR", "mv001"="DHSCLUST"))

ir_sensitivty_analysis <- readstata13::read.dta13("data\\DHS\\IR\\KEIR72FL.DTA") %>% 
  #Select variables of interest
  select(v025, v716, v000, v007, v001) %>% 
  #Filter on rural communities
  filter(v025=="rural") %>% 
  #Generate new variables for identifying fisherfolk and country
  mutate(fishing=ifelse(grepl("^fishery",v716),1,0),
         country=substr(v000,1,2)) %>%
  #Merge with fishing clusters
  left_join(fish_village_final, by=c("country"="DHSCC", "v007"="DHSYEAR", "v001"="DHSCLUST"))

#5km
sensitivty_analysis %>% 
  count(fishing_community_5, fishing) 

table_5km <- as.table(matrix(c(20,134,51,7699), nrow = 2, byrow = TRUE))
rownames(table_5km) <- c("BUFFER FISHING","BUFFER NOT FISHING")
colnames(table_5km) <- c("DHS FISHING", "DHS NOT FISHING")
table_5km

#2.5km
sensitivty_analysis %>% 
  count(fishing_community_25, fishing) 

table_25km <- as.table(matrix(c(16,68,55,7765), nrow = 2, byrow = TRUE))
rownames(table_25km) <- c("BUFFER FISHING","BUFFER NOT FISHING")
colnames(table_25km) <- c("DHS FISHING", "DHS NOT FISHING")
table_25km

#1.25km
sensitivty_analysis %>% 
  count(fishing_community_125, fishing) 

table_125km <- as.table(matrix(c(11,56,60,7777), nrow = 2, byrow = TRUE))
rownames(table_125km) <- c("BUFFER FISHING","BUFFER NOT FISHING")
colnames(table_125km) <- c("DHS FISHING", "DHS NOT FISHING")
table_125km

library(epiR)
sens_5km <- epi.tests(table_5km, conf.level = 0.95)
sens_25km <- epi.tests(table_25km, conf.level = 0.95)
sens_125km <- epi.tests(table_125km, conf.level = 0.95)

#5km is most sensitive and has comparable specificity
#2.5km has highest PPV

sensitivty_analysis %>% tab(fishing_community_5)

#rm(list=ls(pattern="fish_village_"))
rm(list=ls(pattern="table_"))
rm(list=ls(pattern="lakes_"))
rm(list=ls(pattern="villages"))


sens

