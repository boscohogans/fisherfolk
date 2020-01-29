
# Merging variables from childhood data to household data -----------------

kr1 <- as.data.frame(kr_fish_gps_upd)

kr2 <- kr1 %>% 
  mutate(unique_id=paste(v000,v001,v002),
         number=1)  %>% 
  group_by(unique_id) %>% 
  rename(mat_age=v012,
         mat_edu=v133) %>% 
  mutate(ticket=cumsum(number))

kr3 <- subset(kr2, ticket==1) %>% 
  select(unique_id, caseid, v000, v001, v002, mat_age, mat_edu) 


# Cohort characteristics --------------------------------------------------
#Data frame for cohort characteristic tables

data_table1 <- hr_fish_gps_upd %>% 
  mutate(fish_factor=factor(fishing_community_5,
                            levels=c(0,1),
                            labels=c("Non-fishing community", "Fishing community")),
         country_factor=factor(country,
                               levels = c("KE", "MW", "TZ", "UG", "ZM"),
                               labels = c("Kenya", "Malawi", "Tanzania", "Uganda", "Zambia")),
  unique_id=paste(hv000,hv001,hv002)) %>% 
    rename(num_hhmembers=hv009) %>% 
  left_join(kr3, by="unique_id") %>% 
  #Join propensity score
    left_join(select(propensity_score,clusters,weights), by="clusters") 

rm(kr1,  kr3)

#Overall % of clusters identified as fishing communities and communities included fro propensity score matching
fish_table <- data_table1 %>%  tab(weights, fish_factor)

tab1_vars <- c("num_hhmembers", "asset_index_nowashnomat", "mat_age", "mat_edu", "country_factor")

library(tableone)
library(labelled)
tab1 <- CreateTableOne(vars = tab1_vars, strata = "fish_factor", 
                       data = subset(data_table1,weights==1),quote=TRUE)


# All outcome variables ---------------------------------------------------
kr4 <- subset(kr2, ticket==1) %>% 
  select(unique_id, caseid, v000, v001, v002, ari, diarrhea, fever,not_immun) 


#Data frame for outcome variable tables
data_table2 <- hr_fish_gps_upd %>% 
  mutate(fish_factor=factor(fishing_community_5,
                            levels=c(0,1),
                            labels=c("Non-fishing community", "Fishing community")),
         country_factor=factor(country,
                               levels = c("KE", "MW", "TZ", "UG", "ZM"),
                               labels = c("Kenya", "Malawi", "Tanzania", "Uganda", "Zambia")),
         quintile_factor=factor(quintile_nowashnomat,
                                levels = c("1", "2", "3", "4", "5"),
                                labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))) %>% 
  mutate(clusters= paste(hv000,hv021,sep="_")) %>%
  mutate(unique_id=paste(hv000,hv001,hv002)) %>% 
  rename(num_hhmembers=hv009) %>% 
  left_join(kr4, by="unique_id") %>% 
  #Join propensity score
  left_join(select(propensity_score,clusters,weights), by="clusters") 

tab2_vars <- c("water_imp","less_than_5", "san_imp", "housing_imp", 
               "fever", "not_immun", "ari", "diarrhea")

tab2 <- CreateTableOne(vars = tab2_vars, strata = "fish_factor", quote=TRUE,
                       data = subset(data_table2,weights==1))

rm(kr2, kr4, datatable1, data_table2, tab1_vars, tab2_vars)
