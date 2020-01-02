
#Building DHS datasets
countries <- c("KE", "MW", "TZ", "UG", "ZM")
#Read in all GPS Points
all_gps <- fread("data//gps_dhs_rural.csv")[DHSCC %in% countries]
#drop if longnum==0
#Read in all fishing communities 
rural_5km <- fread("data//rural_intersection_5km_20190815.csv")[DHSCC %in% countries]
#Generate fishing community variable
rural_5km$fishing_community <- 1

fish_gps <- all_gps %>%
  #Join fishing communities to all GPS points
  left_join(select(rural_5km, DHSCC, DHSYEAR, DHSCLUST, fishing_community, LAKE_NAME), 
  #left_join(rural_5km, 
           c("DHSCC", "DHSYEAR", "DHSCLUST")) %>%
  #Select columns for further analysis
  #select(DHSID.x, DHSCC, DHSYEAR, DHSCLUST, LAKE_NAME, LATNUM.x, LONGNUM.x, fishing_community) %>%
  #Generate hv000 based on survey year and country
  # mutate(hv000 = ifelse((DHSYEAR==2003 & DHSCC=="KE"),"KE4",
  #        ifelse((DHSYEAR==2008 & DHSCC=="KE"),"KE5",
  #        ifelse((DHSYEAR==2014 & DHSCC=="KE"),"KE6",
  #        ifelse((DHSYEAR==2010 & DHSCC=="MW"),"MW5",
  #        ifelse((DHSYEAR==2015 & DHSCC=="MW"),"MW7",
  #        ifelse((DHSYEAR==2010 & DHSCC=="TZ"),"TZ5",
  #        ifelse((DHSYEAR==2015 & DHSCC=="TZ"),"TZ7",
  #        ifelse((DHSYEAR==2006 & DHSCC=="UG"),"UG5",
  #        ifelse((DHSYEAR==2011 & DHSCC=="UG"),"UG6",
  #        ifelse((DHSYEAR==2016 & DHSCC=="UG"),"UG7",
  #        ifelse((DHSYEAR==2007 & DHSCC=="ZM"),"ZM5",
  #        ifelse((DHSYEAR==2013 & DHSCC=="ZM"),"ZM6",
  #        NA ))))))))))))) %>%
  #filter(DHSYEAR !="")
  filter(fishing_community==1) %>%
  mutate(fishing_community = ifelse(is.na(fishing_community), 0, fishing_community))


rm(all_gps, rural_5km)
#keep dhs* lat* long* lake_name

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
         dhsyear= ifelse(hv000=="KE5", 2008,
                  ifelse(hv000=="MW4", 2005,
                  ifelse(hv000=="MW7", 2015,
                  ifelse(hv000=="TZ5", 2010,
                  ifelse(hv000=="TZ7", 2015,
                  ifelse(hv000=="UG4",2000,
                  ifelse(hv000=="ZM6", 2013,
                  dhsyear))))))),
         dhsround=substr(hv000,3,1),
         country=substr(hv000,1,2)) %>%
left_join(wealth_index,by=c("hv000"="hv000", "hv001"="hv001", "hv002"="hv002")) %>%
left_join(fish_gps, by=c("country"="DHSCC", "dhsyear"="DHSYEAR", "dhsclust"="DHSCLUST")) %>%
  mutate(fishing_community = ifelse(is.na(fishing_community), 0, fishing_community))


hr_fish_gps <- setDT(hr_data, key="hhid")

rm(all_hr, all_hr_combined, hr_data, hr, hr_files, hr_vars_to_keep, keep_hr)


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
         dhsyear= ifelse(v000=="KE5", 2008,
                         ifelse(v000=="MW4", 2005,
                                ifelse(v000=="MW7", 2015,
                                       ifelse(v000=="TZ5", 2010,
                                              ifelse(v000=="TZ7", 2015,
                                                     ifelse(v000=="UG4",2000,
                                                            ifelse(v000=="ZM6", 2013,
                                                                   dhsyear))))))),
         dhsround=substr(v000,3,1),
         country=substr(v000,1,2))  %>%
  left_join(wealth_index,by=c("v000"="hv000", "v001"="hv001", "v002"="hv002")) %>%
  left_join(fish_gps, by=c("country"="DHSCC", "dhsyear"="DHSYEAR", "dhsclust"="DHSCLUST")) %>%
   #only keep record if child is alive and resident
        filter((b5=="Yes"|b5=="yes") & (v135=="usual resident"|v135=="Usual resident")) %>%
  mutate(fishing_community = ifelse(is.na(fishing_community), 0, fishing_community))


kr_fish_gps <- setDT(kr_data, key="caseid")

rm(all_kr, all_kr_combined, kr_data, keep_kr, kr, kr_files, kr_vars_to_keep, fish_gps, wealth_index)



