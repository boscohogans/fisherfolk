#Building DHS datasets
countries <- c("KE", "MW", "TZ", "UG", "ZM")

###Household data
#Import HR DHS data
hr <- list.files("data/DHS//HR", pattern=".csv", full.names = TRUE)

keep_hr <- c("zmhr6", "zmhr5", "ughr7", "ughr6", "tzhr7", "tzhr6", "mwhr7", "mwhr6", "kehr7", "kehr5")

hr_files <- hr[grep(paste(keep_hr, collapse="|"),hr)]
hr_vars_to_keep <- c("hhid","hv000","hv001","hv002","hv005","hv007","hv009","hv014",
                     "hv021","hv025","hv201","hv204","hv205","hv206", "hv207", "hv208", 
                     "hv243a", "hv221", "hv209", "hv230a", "hv230b", "hv232","hv232b","hv232y",  "sh110l", "sh110i", "sh110j", "sh110n", 
                     "sh110v", "hv243b", "sh118g", "hv210", "hv211", "hv243c", "hv212", 
                     "hv243d", "hv244", "hv213","hv214","hv215",
                     "hv216","hv225","hv243","hv246*","hv270","hv271",
                     "hv246a","hv246c", "hv246d", "hv246e","hv246f", "hv246g", "sh122a", "sh122b", "sh122c")

all_hr <- lapply(hr_files, fread, select=hr_vars_to_keep, header=TRUE)

names (all_hr) <- keep_hr

all_hr_combined <- rbindlist(all_hr, idcol="fill", fill=TRUE)

wealth_index <- fread("data//wealth_index_20191201.csv")

hr_data <- all_hr_combined %>%
  mutate(dhsclust=hv001,
         dhsyear=hv007,
         dhsyear=case_when(
           .$hv000=="KE5" ~ 2008,  .$hv000=="MW4" ~ 2005,
           .$hv000=="MW7" ~ 2015,  .$hv000=="TZ5" ~ 2010,
           .$hv000=="TZ7" ~ 2015,  .$hv000=="UG4" ~ 2000,
           .$hv000=="ZM6" ~ 2013,   TRUE ~ as.numeric(dhsyear)),
         dhsround=substr(hv000,3,1),
         country=substr(hv000,1,2)) %>%
  left_join(wealth_index,by=c("hv000"="hv000", "hv001"="hv001", "hv002"="hv002")) %>%
  left_join(fish_village_final, by=c("country"="DHSCC", "dhsyear"="DHSYEAR", "dhsclust"="DHSCLUST")) %>%
  mutate(fishing_community_5 = ifelse(is.na(fishing_community_5), 0, fishing_community_5),
         fishing_community_25 = ifelse(is.na(fishing_community_25), 0, fishing_community_25),
         fishing_community_125 = ifelse(is.na(fishing_community_125), 0, fishing_community_125))

hr_fish_gps_upd <- setDT(hr_data, key="hhid")

#Compare wealth index with DHS wealth index
res <-   cor.test(hr_fish_gps_upd$hv271, hr_fish_gps_upd$asset_index_nowashnomat,method = "pearson")

rm(all_hr, hr_fish_gps, all_hr_combined, hr_data, hr, hr_files, hr_vars_to_keep, keep_hr, res)

###Childhood data
#Import KR DHS data
kr <- list.files("data/DHS//KR", pattern=".csv", full.names = TRUE)

keep_kr <- c("zmkr6", "zmkr5", "ugkr7", "ugkr6", "tzkr7", "tzkr6", "mwkr7", "mwkr6", "kekr7", "kekr5")

kr_files <- kr[grep(paste(keep_kr, collapse="|"),kr)]
kr_vars_to_keep <- c("caseid", "v000", "v001", "v002", "v007",
                      "v009", "v012", "v021", "v106", "v133" ,
                     "v135", "v136", "v137", "h6", "h7", "h8", "h9",
                     "b5", "h10", "h11",  "h22", "h31c", "h31b", "m4")

all_kr <- lapply(kr_files, fread, select=kr_vars_to_keep, header=TRUE)

names (all_kr) <- keep_kr

all_kr_combined <- rbindlist(all_kr, idcol="fill", fill=TRUE)

kr_data <- all_kr_combined %>%
  mutate(dhsclust=v001,
         dhsyear=v007,
         dhsyear=case_when(
           .$v000=="KE5" ~ 2008,  .$v000=="MW4" ~ 2005,
           .$v000=="MW7" ~ 2015,  .$v000=="TZ5" ~ 2010,
           .$v000=="TZ7" ~ 2015,  .$v000=="UG4" ~ 2000,
           .$v000=="ZM6" ~ 2013,   TRUE ~ as.numeric(dhsyear)),
         dhsround=substr(v000,3,1),
         country=substr(v000,1,2))  %>%
  left_join(wealth_index,by=c("v000"="hv000", "v001"="hv001", "v002"="hv002")) %>%
  left_join(fish_village_final, by=c("country"="DHSCC", "dhsyear"="DHSYEAR", "dhsclust"="DHSCLUST")) %>%
   #only keep record if child is alive and resident
        filter((b5=="Yes"|b5=="yes") & (v135=="usual resident"|v135=="Usual resident")) %>%
  mutate(fishing_community_5 = ifelse(is.na(fishing_community_5), 0, fishing_community_5),
         fishing_community_25 = ifelse(is.na(fishing_community_25), 0, fishing_community_25),
         fishing_community_125 = ifelse(is.na(fishing_community_125), 0, fishing_community_125))

kr_fish_gps_upd <- setDT(kr_data, key="caseid")

rm(all_kr, all_kr_combined, kr_data, keep_kr, kr, kr_files, kr_vars_to_keep, fish_gps, wealth_index, fish_village_final)